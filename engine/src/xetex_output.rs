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
    IntPar, ACTIVE_BASE, BIGGEST_USV, CAT_CODE, DIMEN_VAL_LIMIT, EQTB_SIZE, HASH_BASE, INTPAR,
    NULL_CS, SCRIPT_SIZE, SINGLE_BASE, TEXT_SIZE, UNDEFINED_CONTROL_SEQUENCE,
};
use crate::cmd::Cmd;
use crate::node::NativeWord;
use crate::xetex_scaledmath::Scaled;

use super::xetex_ini::Selector;
use super::xetex_ini::{
    dig, doing_special, error_line, file_offset, hash, line, log_file, max_print_line, pool_ptr,
    pool_size, rust_stdout, selector, str_pool, str_ptr, str_start, tally, term_offset, trick_buf,
    trick_count, write_file, EQTB_TOP, FULL_SOURCE_FILENAME_STACK, IN_OPEN, LINE_STACK, MEM,
};
use bridge::ttstub_output_putc;

/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* Extra stuff used in various change files for various reasons.  */
/* Array allocations. Add 1 to size to account for Pascal indexing convention. */
/*11:*/
/*18: */
pub(crate) type UTF16_code = u16;
pub(crate) type str_number = i32;
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
pub(crate) unsafe fn print_chr(s: char) {
    print_char(s as i32)
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
        print_raw_char((128 + s % 0x1000 / 64) as UTF16_code, false);
        print_raw_char((128 + s % 64) as UTF16_code, true);
    };
}
pub(crate) unsafe fn print(mut s: i32) {
    let mut nl: i32 = 0;
    if s >= str_ptr {
        return print_cstr("???");
    } else {
        if s < 0xffff {
            if s < 0 {
                return print_cstr("???");
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
                *INTPAR(IntPar::new_line_char) = -1;
                print_char(s);
                *INTPAR(IntPar::new_line_char) = nl;
                return;
            }
        }
    }
    let mut pool_idx: i32 = s - 0x10000;

    // TODO: fix this bug workaround
    let start = (if pool_idx as usize > crate::xetex_ini::max_strings {
        0
    } else {
        str_start[pool_idx as usize]
    }) as usize;
    for c in std::char::decode_utf16(
        str_pool[start..(str_start[(pool_idx + 1) as usize] as usize)]
            .iter()
            .cloned(),
    ) {
        print_char(c.unwrap() as i32)
    }
}
pub(crate) unsafe fn print_cstr(slice: &str) {
    for &s in slice.as_bytes() {
        print_char(s as i32);
    }
}
pub(crate) unsafe fn print_nl(mut s: str_number) {
    if term_offset > 0 && u8::from(selector) & 1 != 0
        || file_offset > 0 && (u8::from(selector) >= u8::from(Selector::LOG_ONLY))
    {
        print_ln();
    }
    print(s);
}
pub(crate) unsafe fn print_nl_cstr(slice: &str) {
    if term_offset > 0 && u8::from(selector) & 1 != 0
        || file_offset > 0 && (u8::from(selector) >= u8::from(Selector::LOG_ONLY))
    {
        print_ln();
    }
    print_cstr(slice);
}
pub(crate) unsafe fn print_esc(mut s: str_number) {
    let mut c = *INTPAR(IntPar::escape_char);
    if c >= 0 && c <= BIGGEST_USV as i32 {
        print_char(c);
    }
    print(s);
}
pub(crate) unsafe fn print_esc_cstr(s: &str) {
    let mut c = *INTPAR(IntPar::escape_char);
    if c >= 0 && c <= BIGGEST_USV as i32 {
        print_char(c);
    }
    print_cstr(s);
}
unsafe fn print_the_digs(mut k: u8) {
    while k as i32 > 0 {
        k -= 1;
        if (dig[k as usize] as i32) < 10 {
            print_char('0' as i32 + dig[k as usize] as i32);
        } else {
            print_char(55 + dig[k as usize] as i32);
        }
    }
}
pub(crate) unsafe fn print_int(mut n: i32) {
    let mut k = 0_u8;
    let mut m: i32 = 0;
    if n < 0 {
        print_chr('-');
        if n as i64 > -100000000 {
            n = -n
        } else {
            m = -1 - n;
            n = m / 10;
            m = m % 10 + 1;
            k = 1_u8;
            if m < 10 {
                dig[0] = m as u8
            } else {
                dig[0] = 0;
                n += 1;
            }
        }
    }
    loop {
        dig[k as usize] = (n % 10) as u8;
        n = n / 10;
        k += 1;
        if n == 0 {
            break;
        }
    }
    print_the_digs(k);
}
pub(crate) unsafe fn print_cs(mut p: i32) {
    if p < HASH_BASE as i32 {
        if p >= SINGLE_BASE as i32 {
            if p == NULL_CS as i32 {
                print_esc_cstr("csname");
                print_esc_cstr("endcsname");
                print_chr(' ');
            } else {
                print_esc(p - SINGLE_BASE as i32);
                if *CAT_CODE(p as usize - SINGLE_BASE) == Cmd::Letter as _ {
                    print_chr(' ');
                }
            }
        } else if p < ACTIVE_BASE as i32 {
            print_esc_cstr("IMPOSSIBLE.");
        } else {
            print_char(p - 1);
        }
    } else if p >= UNDEFINED_CONTROL_SEQUENCE as i32 && p <= EQTB_SIZE as i32 || p > EQTB_TOP as i32
    {
        print_esc_cstr("IMPOSSIBLE.");
    } else if (*hash.offset(p as isize)).s1 >= str_ptr {
        print_esc_cstr("NONEXISTENT.");
    } else {
        print_esc((*hash.offset(p as isize)).s1);
        print_chr(' ');
    };
}
pub(crate) unsafe fn sprint_cs(mut p: i32) {
    if p < HASH_BASE as i32 {
        if p < SINGLE_BASE as i32 {
            print_char(p - 1);
        } else if p < NULL_CS as i32 {
            print_esc(p - SINGLE_BASE as i32);
        } else {
            print_esc_cstr("csname");
            print_esc_cstr("endcsname");
        }
    } else {
        print_esc((*hash.offset(p as isize)).s1);
    };
}
pub(crate) unsafe fn print_file_name(n: i32, a: i32, e: i32) {
    let mut must_quote: bool = false;
    let mut quote_char: i32 = 0;
    let mut j = 0;
    if a != 0 {
        j = str_start[(a - 0x10000) as usize] as usize;
        while (!must_quote || quote_char == 0) && j < str_start[(a + 1 - 0x10000) as usize] as usize
        {
            if str_pool[j] as i32 == ' ' as i32 {
                must_quote = true
            } else if str_pool[j] as i32 == '\"' as i32 || str_pool[j] as i32 == '\'' as i32 {
                must_quote = true;
                quote_char = 73 - str_pool[j] as i32
            }
            j += 1
        }
    }
    if n != 0 {
        j = str_start[(n - 0x10000) as usize] as usize;
        while (!must_quote || quote_char == 0) && j < str_start[(n + 1 - 0x10000) as usize] as usize
        {
            if str_pool[j] as i32 == ' ' as i32 {
                must_quote = true
            } else if str_pool[j] as i32 == '\"' as i32 || str_pool[j] as i32 == '\'' as i32 {
                must_quote = true;
                quote_char = 73 - str_pool[j] as i32
            }
            j += 1
        }
    }
    if e != 0 {
        j = str_start[(e - 0x10000) as usize] as usize;
        while (!must_quote || quote_char == 0) && j < str_start[(e + 1 - 0x10000) as usize] as usize
        {
            if str_pool[j] as i32 == ' ' as i32 {
                must_quote = true
            } else if str_pool[j] as i32 == '\"' as i32 || str_pool[j] as i32 == '\'' as i32 {
                must_quote = true;
                quote_char = 73 - str_pool[j] as i32
            }
            j += 1
        }
    }
    if must_quote {
        if quote_char == 0 {
            quote_char = '\"' as i32
        }
        print_char(quote_char);
    }
    if a != 0 {
        for j in (str_start[(a - 0x10000) as usize] as usize)
            ..(str_start[(a + 1 - 0x10000) as usize] as usize)
        {
            if str_pool[j] as i32 == quote_char {
                print(quote_char);
                quote_char = 73 - quote_char;
                print(quote_char);
            }
            print(str_pool[j] as i32);
        }
    }
    if n != 0 {
        for j in (str_start[(n - 0x10000) as usize] as usize)
            ..(str_start[(n + 1 - 0x10000) as usize] as usize)
        {
            if str_pool[j] as i32 == quote_char {
                print(quote_char);
                quote_char = 73 - quote_char;
                print(quote_char);
            }
            print(str_pool[j] as i32);
        }
    }
    if e != 0 {
        for j in (str_start[(e - 0x10000) as usize] as usize)
            ..(str_start[(e + 1 - 0x10000) as usize] as usize)
        {
            if str_pool[j] as i32 == quote_char {
                print(quote_char);
                quote_char = 73 - quote_char;
                print(quote_char);
            }
            print(str_pool[j] as i32);
        }
    }
    if quote_char != 0 {
        print_char(quote_char);
    };
}
pub(crate) unsafe fn print_size(mut s: i32) {
    if s == TEXT_SIZE as i32 {
        print_esc_cstr("textfont");
    } else if s == SCRIPT_SIZE as i32 {
        print_esc_cstr("scriptfont");
    } else {
        print_esc_cstr("scriptscriptfont");
    };
}
pub(crate) unsafe fn print_write_whatsit(s: &str, p: usize) {
    print_esc_cstr(s);
    if MEM[p + 1].b32.s0 < 16 {
        print_int(MEM[p + 1].b32.s0);
    } else if MEM[p + 1].b32.s0 == 16 {
        print_chr('*');
    } else {
        print_chr('-');
    };
}
pub(crate) unsafe fn print_native_word(p: &NativeWord) {
    for c in std::char::decode_utf16(p.text().iter().cloned()) {
        if let Ok(c) = c {
            print_char(c as i32);
        } else {
            print('.' as i32);
        }
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
        print_nl_cstr("! ");
    } else {
        print_nl_cstr("");
        print(FULL_SOURCE_FILENAME_STACK[level]);
        print(':' as i32);
        if level == IN_OPEN {
            print_int(line);
        } else {
            print_int(LINE_STACK[level + 1]);
        }
        print_cstr(": ");
    };
}
/*:251 */
/*:251 */
/*:1660*/
pub(crate) unsafe fn print_two(mut n: i32) {
    n = n.abs() % 100;
    print_char('0' as i32 + n / 10);
    print_char('0' as i32 + n % 10);
}
pub(crate) unsafe fn print_hex(mut n: i32) {
    let mut k: u8 = 0_u8;
    print_chr('\"');
    loop {
        dig[k as usize] = (n % 16) as u8;
        n = n / 16;
        k = k.wrapping_add(1);
        if n == 0 {
            break;
        }
    }
    print_the_digs(k);
}
pub(crate) unsafe fn print_roman_int(mut n: i32) {
    let mut u: i32 = 0;
    let mut v: i32 = 0;
    const roman_data: &[u8] = b"m2d5c2l5x2v5i";
    let mut j: u8 = 0_u8;
    v = 1000i32;
    loop {
        while n >= v {
            print_char(roman_data[j as usize] as i32);
            n = n - v
        }
        if n <= 0 {
            return;
        }
        let mut k = j + 2;
        u = v / (roman_data[k as usize - 1] as i32 - '0' as i32);
        if roman_data[k as usize - 1] as i32 == '2' as i32 {
            k += 2;
            u = u / (roman_data[k as usize - 1] as i32 - '0' as i32)
        }
        if n + u >= v {
            print_char(roman_data[k as usize] as i32);
            n = n + u
        } else {
            j += 2;
            v = v / (roman_data[j as usize - 1] as i32 - '0' as i32)
        }
    }
}
pub(crate) unsafe fn print_current_string() {
    let mut j = str_start[(str_ptr - 0x10000) as usize];
    while j < pool_ptr {
        print_char(str_pool[j as usize] as i32);
        j += 1
    }
}
pub(crate) unsafe fn print_scaled(s: Scaled) {
    let mut s = s.0;
    let mut delta = 0;
    if s < 0 {
        print_chr('-');
        s = s.wrapping_neg(); // TODO: check
    }
    print_int(s / 0x10000);
    print_chr('.');
    s = 10 * (s % 0x10000) + 5;
    delta = 10;
    loop {
        if delta > 0x10000 {
            s = s + 0x8000 - 50000
        }
        print_char('0' as i32 + s / 0x10000);
        s = 10 * (s % 0x10000);
        delta *= 10;
        if !(s > delta) {
            break;
        }
    }
}
