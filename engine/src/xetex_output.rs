/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::{t_print, t_print_nl};

use super::xetex_consts::{
    get_int_par, IntPar, ACTIVE_BASE, BIGGEST_USV, CAT_CODE, DIMEN_VAL_LIMIT, EQTB_SIZE, HASH_BASE,
    NULL_CS, SCRIPT_SIZE, SINGLE_BASE, TEXT_SIZE, UNDEFINED_CONTROL_SEQUENCE,
};
use crate::cmd::Cmd;
use crate::node::NativeWord;
use crate::xetex_stringpool::PoolString;

use super::xetex_ini::Selector;
use super::xetex_ini::{
    doing_special, error_line, file_offset, hash, line, log_file, max_print_line, pool_ptr,
    pool_size, rust_stdout, selector, str_pool, str_ptr, tally, term_offset, trick_buf,
    trick_count, write_file, EQTB_TOP, FULL_SOURCE_FILENAME_STACK, IN_OPEN, LINE_STACK, MEM,
};

/* Extra stuff used in various change files for various reasons.  */
/* Array allocations. Add 1 to size to account for Pascal indexing convention. */
/*11:*/
/*18: */
/* xetex-output */
/* tectonic/output.c -- functions related to outputting messages
 * Copyright 2016 the Tectonic Project
 * Licensed under the MIT License.
*/
pub(crate) unsafe fn print_ln() {
    use io::Write;
    match selector {
        Selector::TERM_AND_LOG => {
            let stdout = rust_stdout.as_mut().unwrap();
            write_term_ln(stdout).unwrap();
            let lg = log_file.as_mut().unwrap();
            write_log_ln(lg).unwrap();
        }
        Selector::LOG_ONLY => {
            let lg = log_file.as_mut().unwrap();
            write_log_ln(lg).unwrap();
        }
        Selector::TERM_ONLY => {
            let stdout = rust_stdout.as_mut().unwrap();
            write_term_ln(stdout).unwrap();
        }
        Selector::NO_PRINT | Selector::PSEUDO | Selector::NEW_STRING => {}
        Selector::File(u) => {
            write!(write_file[u as usize].as_mut().unwrap(), "\n").unwrap();
        }
    };
}

use std::io;
unsafe fn write_term_ln<W: io::Write>(out: &mut W) -> io::Result<()> {
    term_offset = 0;
    write!(out, "\n")
}
unsafe fn write_log_ln<W: io::Write>(lg: &mut W) -> io::Result<()> {
    file_offset = 0;
    write!(lg, "\n")
}
unsafe fn write_term_char<W: io::Write>(out: &mut W, c: char) -> io::Result<()> {
    write!(out, "{}", c)?;
    term_offset += 1;
    if term_offset == max_print_line {
        write_term_ln(out)?;
    }
    Ok(())
}

unsafe fn write_log_char<W: io::Write>(lg: &mut W, c: char) -> io::Result<()> {
    write!(lg, "{}", c)?;
    file_offset += 1;
    if file_offset == max_print_line {
        write_log_ln(lg)?;
    }
    Ok(())
}

unsafe fn print_term_log_char<W: io::Write>(stdout: &mut W, lg: &mut W, s: char) -> io::Result<()> {
    if (s as i32) == get_int_par(IntPar::new_line_char) {
        write_term_ln(stdout)?;
        write_log_ln(lg)?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write_term_char(stdout, c)?;
                write_log_char(lg, c)?;
            }
        } else {
            write_term_char(stdout, s)?;
            write_log_char(lg, s)?;
        }
        tally += 1;
    }
    Ok(())
}

unsafe fn print_log_char<W: io::Write>(lg: &mut W, s: char) -> io::Result<()> {
    if (s as i32) == get_int_par(IntPar::new_line_char) {
        write_log_ln(lg)?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write_log_char(lg, c)?;
            }
        } else {
            write_log_char(lg, s)?;
        }
        tally += 1;
    }
    Ok(())
}
unsafe fn print_term_char<W: io::Write>(stdout: &mut W, s: char) -> io::Result<()> {
    if (s as i32) == get_int_par(IntPar::new_line_char) {
        write_term_ln(stdout)?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write_term_char(stdout, c)?;
            }
        } else {
            write_term_char(stdout, s)?;
        }
        tally += 1;
    }
    Ok(())
}

unsafe fn print_file_char<W: io::Write>(file: &mut W, s: char) -> io::Result<()> {
    if (s as i32) == get_int_par(IntPar::new_line_char) {
        write!(file, "\n")?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write!(file, "{}", c)?;
            }
        } else {
            write!(file, "{}", s)?;
        }
        tally += 1;
    }
    Ok(())
}

pub(crate) unsafe fn print_rust_char(s: char) {
    match selector {
        Selector::TERM_AND_LOG => {
            let stdout = rust_stdout.as_mut().unwrap();
            let lg = log_file.as_mut().unwrap();
            print_term_log_char(stdout, lg, s).unwrap();
        }
        Selector::LOG_ONLY => {
            let lg = log_file.as_mut().unwrap();
            print_log_char(lg, s).unwrap();
        }
        Selector::TERM_ONLY => {
            let stdout = rust_stdout.as_mut().unwrap();
            print_term_char(stdout, s).unwrap();
        }
        Selector::NO_PRINT => {}
        Selector::PSEUDO => {
            let mut count;
            if s.is_control() {
                let (buf, len) = replace_control(s);
                count = 0;
                for &c in &buf[..len] {
                    if tally < trick_count {
                        trick_buf[((tally + count) % error_line) as usize] = c;
                    }
                    count += 1;
                }
            } else {
                count = 1;
                if tally < trick_count {
                    trick_buf[(tally % error_line) as usize] = s;
                }
            }
            tally += count;
        }
        Selector::NEW_STRING => {
            if !doing_special {
                let mut b = [0; 2];
                for i in s.encode_utf16(&mut b) {
                    if pool_ptr < pool_size {
                        str_pool[pool_ptr as usize] = *i;
                        pool_ptr += 1
                    }
                }
                return;
            } else {
                let mut b = [0; 4];
                for c in s.encode_utf8(&mut b).bytes() {
                    if pool_ptr < pool_size {
                        str_pool[pool_ptr as usize] = c as u16;
                        pool_ptr += 1
                    }
                }
                return;
            }
        }
        Selector::File(u) => {
            let file = write_file[u as usize].as_mut().unwrap();
            print_file_char(file, s).unwrap();
        }
    }
}
/* TODO: optimize
pub(crate) unsafe fn print_rust_string(s: &str) {
    for c in s.chars() {
        print_rust_char(c);
    }
    use io::Write;
    let mut count = s.chars().count() as i32;
    match selector {
        Selector::TERM_AND_LOG => {
            if file_offset + count <= max_print_line && term_offset + count <= max_print_line {
                let stdout = rust_stdout.as_mut().unwrap();
                let lg = log_file.as_mut().unwrap();
                write!(stdout, "{}", s).unwrap();
                write!(lg, "{}", s).unwrap();
                term_offset += count;
                file_offset += count;
                if term_offset == max_print_line {
                    write!(stdout, "\n").unwrap();
                    term_offset = 0;
                }
                if file_offset == max_print_line {
                    write!(lg, "\n").unwrap();
                    file_offset = 0;
                }
            } else {
                for c in s.chars() {
                    print_rust_char(c);
                }
                return;
            }
        }
        Selector::LOG_ONLY => {
            if file_offset + count <= max_print_line {
                write!(log_file.as_mut().unwrap(), "{}", s).unwrap();
                file_offset += count;
                if file_offset == max_print_line {
                    print_ln();
                }
            } else {
                for c in s.chars() {
                    print_rust_char(c);
                }
                return;
            }
        }
        Selector::TERM_ONLY => {
            if term_offset + count <= max_print_line {
                write!(rust_stdout.as_mut().unwrap(), "{}", s).unwrap();
                term_offset += count;
                if term_offset == max_print_line {
                    print_ln();
                }
            } else {
                for c in s.chars() {
                    print_rust_char(c);
                }
                return;
            }
        }
        Selector::NO_PRINT => {}
        Selector::PSEUDO => {
            count = 0;
            for (t, c) in s
                .chars()
                .take((trick_count - tally).max(0) as usize)
                .enumerate()
            {
                trick_buf[((tally as usize + t) % (error_line as usize))] = c;
                count += 1;
            }
        }
        Selector::NEW_STRING => unreachable!(),
        Selector::File(u) => {
            write!(write_file[u as usize].as_mut().unwrap(), "{}", s).unwrap();
        }
    }
    tally += count;
}*/

pub(crate) unsafe fn print_chr(s: char) {
    print_rust_char(s);
}

use std::fmt;
pub(crate) struct Output;
impl fmt::Write for Output {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        // TODO: optimize
        for c in s.chars() {
            unsafe {
                print_rust_char(c);
            }
        }
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        unsafe {
            print_rust_char(c);
        }
        Ok(())
    }
}

unsafe fn replace_control(s: char) -> ([char; 4], usize) {
    match s {
        '\u{0}'..='\u{1f}' => (['^', '^', char::from((s as u8) + 0x40), '\u{0}'], 3),
        '\u{7f}' => (['^', '^', '?', '\u{0}'], 3),
        '\u{80}'..='\u{9f}' => (
            [
                '^',
                '^',
                {
                    let l = (s as u8) / 16;
                    if l < 10 {
                        char::from(b'0' + l)
                    } else {
                        char::from(b'a' + l - 10)
                    }
                },
                {
                    let l = (s as u8) % 16;
                    if l < 10 {
                        char::from(b'0' + l)
                    } else {
                        char::from(b'a' + l - 10)
                    }
                },
            ],
            4,
        ),
        _ => unreachable!(),
    }
}

pub(crate) unsafe fn printnl() {
    match selector {
        Selector::TERM_ONLY | Selector::TERM_AND_LOG if term_offset > 0 => print_ln(),
        Selector::LOG_ONLY | Selector::TERM_AND_LOG if file_offset > 0 => print_ln(),
        _ => {}
    }
}
pub(crate) struct Esc<'a>(pub &'a str);
impl<'a> fmt::Display for Esc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = unsafe { get_int_par(IntPar::escape_char) };
        if c >= 0 && c <= BIGGEST_USV as i32 {
            std::char::from_u32(c as u32).unwrap().fmt(f)?;
        }
        self.0.fmt(f)
    }
}
pub(crate) unsafe fn print_esc_cstr(s: &str) {
    t_print!("{}", Esc(s));
}
pub(crate) unsafe fn print_cs(p: i32) {
    if p < HASH_BASE as i32 {
        if p >= SINGLE_BASE as i32 {
            if p == NULL_CS as i32 {
                print_esc_cstr("csname");
                print_esc_cstr("endcsname");
                print_chr(' ');
            } else {
                t_print!(
                    "{}",
                    Esc(&PoolString::from(p - SINGLE_BASE as i32).to_string())
                );
                if *CAT_CODE(p as usize - SINGLE_BASE) == Cmd::Letter as _ {
                    print_chr(' ');
                }
            }
        } else if p < ACTIVE_BASE as i32 {
            print_esc_cstr("IMPOSSIBLE.");
        } else {
            print_chr(std::char::from_u32((p - 1) as u32).unwrap());
        }
    } else if p >= UNDEFINED_CONTROL_SEQUENCE as i32 && p <= EQTB_SIZE as i32 || p > EQTB_TOP as i32
    {
        print_esc_cstr("IMPOSSIBLE.");
    } else if (*hash.offset(p as isize)).s1 >= str_ptr {
        print_esc_cstr("NONEXISTENT.");
    } else {
        t_print!(
            "{} ",
            Esc(&PoolString::from((*hash.offset(p as isize)).s1).to_string())
        );
    };
}
pub(crate) unsafe fn sprint_cs(p: i32) {
    if p < HASH_BASE as i32 {
        if p < SINGLE_BASE as i32 {
            print_chr(std::char::from_u32((p - 1) as u32).unwrap());
        } else if p < NULL_CS as i32 {
            t_print!(
                "{}",
                Esc(&PoolString::from(p - SINGLE_BASE as i32).to_string())
            );
        } else {
            print_esc_cstr("csname");
            print_esc_cstr("endcsname");
        }
    } else {
        t_print!(
            "{}",
            Esc(&PoolString::from((*hash.offset(p as isize)).s1).to_string())
        );
    };
}
pub(crate) unsafe fn print_file_name(n: i32, a: i32, e: i32) {
    let mut must_quote: bool = false;
    let mut quote_char = None;
    if a != 0 {
        for &j in PoolString::from(a).as_slice() {
            if must_quote && quote_char.is_some() {
                break;
            }
            if j as i32 == ' ' as i32 {
                must_quote = true;
            } else if j as i32 == '\"' as i32 {
                must_quote = true;
                quote_char = Some('\'');
            } else if j as i32 == '\'' as i32 {
                must_quote = true;
                quote_char = Some('\"');
            }
        }
    }
    if n != 0 {
        for &j in PoolString::from(n).as_slice() {
            if must_quote && quote_char.is_some() {
                break;
            }
            if j as i32 == ' ' as i32 {
                must_quote = true;
            } else if j as i32 == '\"' as i32 {
                must_quote = true;
                quote_char = Some('\'');
            } else if j as i32 == '\'' as i32 {
                must_quote = true;
                quote_char = Some('\"');
            }
        }
    }
    if e != 0 {
        for &j in PoolString::from(e).as_slice() {
            if must_quote && quote_char.is_some() {
                break;
            }
            if j as i32 == ' ' as i32 {
                must_quote = true;
            } else if j as i32 == '\"' as i32 {
                must_quote = true;
                quote_char = Some('\'');
            } else if j as i32 == '\'' as i32 {
                must_quote = true;
                quote_char = Some('\"');
            }
        }
    }
    if must_quote {
        if let Some(qc) = quote_char {
            print_chr(qc);
        } else {
            quote_char = Some('\"');
            print_chr('\"');
        }
    }
    if a != 0 {
        for j in std::char::decode_utf16(PoolString::from(a).as_slice().iter().cloned()) {
            let j = j.unwrap();
            if Some(j) == quote_char {
                print_chr(j);
                let c = match j {
                    '\"' => '\'',
                    '\'' => '\"',
                    _ => unreachable!(),
                };
                quote_char = Some(c);
                print_chr(c);
            }
            print_chr(j);
        }
    }
    if n != 0 {
        for j in std::char::decode_utf16(PoolString::from(n).as_slice().iter().cloned()) {
            let j = j.unwrap();
            if Some(j) == quote_char {
                print_chr(j);
                let c = match j {
                    '\"' => '\'',
                    '\'' => '\"',
                    _ => unreachable!(),
                };
                quote_char = Some(c);
                print_chr(c);
            }
            print_chr(j);
        }
    }
    if e != 0 {
        for j in std::char::decode_utf16(PoolString::from(e).as_slice().iter().cloned()) {
            let j = j.unwrap();
            if Some(j) == quote_char {
                print_chr(j);
                let c = match j {
                    '\"' => '\'',
                    '\'' => '\"',
                    _ => unreachable!(),
                };
                quote_char = Some(c);
                print_chr(c);
            }
            print_chr(j);
        }
    }
    if let Some(qc) = quote_char {
        print_chr(qc);
    };
}
pub(crate) unsafe fn print_size(s: i32) {
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
        t_print!("{}", MEM[p + 1].b32.s0);
    } else if MEM[p + 1].b32.s0 == 16 {
        t_print!("*");
    } else {
        t_print!("-");
    };
}
pub(crate) unsafe fn print_native_word(p: &NativeWord) {
    for c in std::char::decode_utf16(p.text().iter().cloned()) {
        if let Ok(c) = c {
            t_print!("{}", c);
        } else {
            t_print!(".");
        }
    }
}
pub(crate) unsafe fn print_sa_num(mut q: usize) {
    let mut n;
    if MEM[q].b16.s1 < DIMEN_VAL_LIMIT {
        n = MEM[q + 1].b32.s1
    } else {
        n = MEM[q].b16.s1 as i32 % 64;
        q = MEM[q].b32.s1 as usize;
        n = n + 64 * MEM[q].b16.s1 as i32;
        q = MEM[q].b32.s1 as usize;
        n = n + 64 * 64 * (MEM[q].b16.s1 as i32 + 64 * MEM[MEM[q].b32.s1 as usize].b16.s1 as i32)
    }
    t_print!("{}", n);
}
pub(crate) unsafe fn print_file_line() {
    let mut level = IN_OPEN;
    while level > 0 && FULL_SOURCE_FILENAME_STACK[level] == 0 {
        level -= 1
    }
    if level == 0 {
        t_print_nl!("! ");
    } else {
        t_print_nl!(
            "{}:{}: ",
            FULL_SOURCE_FILENAME_STACK[level],
            if level == IN_OPEN {
                line
            } else {
                LINE_STACK[level + 1]
            }
        );
    };
}

pub(crate) struct Roman(pub i32);
impl fmt::Display for Roman {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const roman_data: &[u8] = b"m2d5c2l5x2v5i";
        let mut n = self.0;
        let mut j: u8 = 0_u8;
        let mut v = 1000;
        loop {
            while n >= v {
                char::from(roman_data[j as usize]).fmt(f)?;
                n = n - v
            }
            if n <= 0 {
                return Ok(());
            }
            let mut k = j + 2;
            let mut u = v / (roman_data[k as usize - 1] as i32 - '0' as i32);
            if roman_data[k as usize - 1] as i32 == '2' as i32 {
                k += 2;
                u = u / (roman_data[k as usize - 1] as i32 - '0' as i32)
            }
            if n + u >= v {
                char::from(roman_data[k as usize]).fmt(f)?;
                n = n + u
            } else {
                j += 2;
                v = v / (roman_data[j as usize - 1] as i32 - '0' as i32)
            }
        }
    }
}
