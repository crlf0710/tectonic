/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::xetex_consts::{FROZEN_NULL_FONT, PRIM_EQTB_BASE};
use crate::xetex_ini::prim;
use crate::{t_print, t_print_nl};

use super::xetex_consts::{
    get_int_par, IntPar, ACTIVE_BASE, BIGGEST_USV, CAT_CODE, DIMEN_VAL_LIMIT, EQTB_SIZE, HASH_BASE,
    NULL_CS, SINGLE_BASE, UNDEFINED_CONTROL_SEQUENCE,
};
use crate::cmd::Cmd;
use crate::node::NativeWord;
use crate::xetex_stringpool::{str_ptr, PoolString};

use super::xetex_ini::Selector;
use super::xetex_ini::{
    error_line, file_offset, hash_offset, line, log_file, max_print_line, rust_stdout, selector,
    tally, term_offset, trick_buf, trick_count, write_file, yhash, EQTB_TOP,
    FULL_SOURCE_FILENAME_STACK, IN_OPEN, LINE_STACK, MEM,
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
        Selector::NO_PRINT | Selector::PSEUDO => {}
        Selector::File(u) => {
            write_file[u as usize]
                .as_mut()
                .unwrap()
                .write_all(b"\n")
                .unwrap();
        }
    };
}

use std::io;
unsafe fn write_term_ln<W: io::Write>(out: &mut W) -> io::Result<()> {
    term_offset = 0;
    out.write_all(b"\n")
}
unsafe fn write_log_ln<W: io::Write>(lg: &mut W) -> io::Result<()> {
    file_offset = 0;
    lg.write_all(b"\n")
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

unsafe fn print_term_log_char<W: io::Write>(
    stdout: &mut W,
    lg: &mut W,
    s: char,
    nl: i32,
) -> io::Result<()> {
    if (s as i32) == nl {
        write_term_ln(stdout)?;
        write_log_ln(lg)?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                let c = char::from(c);
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

unsafe fn print_log_char<W: io::Write>(lg: &mut W, s: char, nl: i32) -> io::Result<()> {
    if (s as i32) == nl {
        write_log_ln(lg)?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write_log_char(lg, char::from(c))?;
            }
        } else {
            write_log_char(lg, s)?;
        }
        tally += 1;
    }
    Ok(())
}
unsafe fn print_term_char<W: io::Write>(stdout: &mut W, s: char, nl: i32) -> io::Result<()> {
    if (s as i32) == nl {
        write_term_ln(stdout)?;
    } else {
        if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write_term_char(stdout, char::from(c))?;
            }
        } else {
            write_term_char(stdout, s)?;
        }
        tally += 1;
    }
    Ok(())
}

unsafe fn print_file_char<W: io::Write>(file: &mut W, s: char, nl: i32) -> io::Result<()> {
    if (s as i32) == nl {
        file.write_all(b"\n")?;
    } else if s.is_control() {
        let (buf, len) = replace_control(s);
        for &c in &buf[..len] {
            write!(file, "{}", char::from(c))?;
        }
    } else {
        write!(file, "{}", s)?;
    }
    Ok(())
}

pub(crate) unsafe fn print_chr(c: char) {
    use std::fmt::Write;
    selector.write_char(c).unwrap();
}

use std::fmt;
impl fmt::Write for Selector {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        unsafe {
            match self {
                Selector::TERM_AND_LOG => {
                    let stdout = rust_stdout.as_mut().unwrap();
                    let lg = log_file.as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    let bytelen = s.len() as i32;
                    if (file_offset + bytelen < max_print_line)
                        && (term_offset + bytelen < max_print_line)
                        && !s.contains(|c: char| (c as i32) == nl || c.is_control())
                    {
                        io::Write::write(lg, s.as_bytes()).unwrap();
                        file_offset += bytelen;
                        io::Write::write(stdout, s.as_bytes()).unwrap();
                        term_offset += bytelen;
                        tally += bytelen;
                    } else {
                        for c in s.chars() {
                            print_term_log_char(stdout, lg, c, nl).unwrap();
                        }
                    }
                }
                Selector::LOG_ONLY => {
                    let lg = log_file.as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    let bytelen = s.len() as i32;
                    if (file_offset + bytelen < max_print_line)
                        && !s.contains(|c: char| (c as i32) == nl || c.is_control())
                    {
                        io::Write::write(lg, s.as_bytes()).unwrap();
                        file_offset += bytelen;
                        tally += bytelen;
                    } else {
                        for c in s.chars() {
                            print_log_char(lg, c, nl).unwrap();
                        }
                    }
                }
                Selector::TERM_ONLY => {
                    let stdout = rust_stdout.as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    let bytelen = s.len() as i32;
                    if (term_offset + bytelen < max_print_line)
                        && !s.contains(|c: char| (c as i32) == nl || c.is_control())
                    {
                        io::Write::write(stdout, s.as_bytes()).unwrap();
                        term_offset += bytelen;
                        tally += bytelen;
                    } else {
                        for c in s.chars() {
                            print_term_char(stdout, c, nl).unwrap();
                        }
                    }
                }
                Selector::NO_PRINT => {}
                Selector::PSEUDO => {
                    for c in s.chars() {
                        self.write_char(c)?;
                    }
                }
                Selector::File(u) => {
                    let file = write_file[*u as usize].as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    if !s.contains(|c: char| (c as i32) == nl || c.is_control()) {
                        io::Write::write(file, s.as_bytes()).unwrap();
                    } else {
                        for c in s.chars() {
                            print_file_char(file, c, nl).unwrap();
                        }
                    }
                }
            }
        }
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, s: char) -> fmt::Result {
        unsafe {
            match selector {
                Selector::TERM_AND_LOG => {
                    let stdout = rust_stdout.as_mut().unwrap();
                    let lg = log_file.as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    print_term_log_char(stdout, lg, s, nl).unwrap();
                }
                Selector::LOG_ONLY => {
                    let lg = log_file.as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    print_log_char(lg, s, nl).unwrap();
                }
                Selector::TERM_ONLY => {
                    let stdout = rust_stdout.as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    print_term_char(stdout, s, nl).unwrap();
                }
                Selector::NO_PRINT => {}
                Selector::PSEUDO => {
                    let mut count;
                    if s.is_control() {
                        let (buf, len) = replace_control(s);
                        count = 0;
                        for &c in &buf[..len] {
                            if tally < trick_count {
                                trick_buf[((tally + count) % error_line) as usize] = char::from(c);
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
                Selector::File(u) => {
                    let file = write_file[u as usize].as_mut().unwrap();
                    let nl = get_int_par(IntPar::new_line_char);
                    print_file_char(file, s, nl).unwrap();
                }
            }
        }
        Ok(())
    }
}

unsafe fn replace_control(s: char) -> ([u8; 4], usize) {
    match s {
        '\u{0}'..='\u{1f}' => ([b'^', b'^', (s as u8) + 0x40, 0], 3),
        '\u{7f}' => ([b'^', b'^', b'?', 0], 3),
        '\u{80}'..='\u{9f}' => (
            [
                b'^',
                b'^',
                {
                    let l = (s as u8) / 16;
                    if l < 10 {
                        b'0' + l
                    } else {
                        b'a' + l - 10
                    }
                },
                {
                    let l = (s as u8) % 16;
                    if l < 10 {
                        b'0' + l
                    } else {
                        b'a' + l - 10
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

pub(crate) struct Cs(pub(crate) i32);
impl fmt::Display for Cs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let p = self.0;
            if f.alternate() {
                if p < HASH_BASE as i32 {
                    if p < SINGLE_BASE as i32 {
                        std::char::from_u32((p - 1) as u32).unwrap().fmt(f)
                    } else if p < NULL_CS as i32 {
                        Esc(&PoolString::from(p - SINGLE_BASE as i32).to_string()).fmt(f)
                    } else {
                        Esc("csname").fmt(f)?;
                        Esc("endcsname").fmt(f)
                    }
                } else if p >= PRIM_EQTB_BASE as i32 && p < FROZEN_NULL_FONT as i32 {
                    Esc(&PoolString::from(prim[p as usize - PRIM_EQTB_BASE].s1 - 1).to_string())
                        .fmt(f)
                } else {
                    Esc(&PoolString::from(yhash[p as usize - hash_offset].s1).to_string()).fmt(f)
                }
            } else if p < HASH_BASE as i32 {
                if p >= SINGLE_BASE as i32 {
                    if p == NULL_CS as i32 {
                        Esc("csname").fmt(f)?;
                        Esc("endcsname").fmt(f)?;
                        ' '.fmt(f)
                    } else {
                        Esc(&PoolString::from(p - SINGLE_BASE as i32).to_string()).fmt(f)?;
                        if *CAT_CODE(p as usize - SINGLE_BASE) == Cmd::Letter as _ {
                            ' '.fmt(f)?;
                        }
                        Ok(())
                    }
                } else if p < ACTIVE_BASE as i32 {
                    Esc("IMPOSSIBLE").fmt(f)?;
                    '.'.fmt(f)
                } else {
                    std::char::from_u32((p - 1) as u32).unwrap().fmt(f)
                }
            } else if p >= UNDEFINED_CONTROL_SEQUENCE as i32 && p <= EQTB_SIZE as i32
                || p > EQTB_TOP as i32
            {
                Esc("IMPOSSIBLE").fmt(f)?;
                '.'.fmt(f)
            } else if yhash[p as usize - hash_offset].s1 >= str_ptr {
                Esc("NONEXISTENT").fmt(f)?;
                '.'.fmt(f)
            } else if p >= PRIM_EQTB_BASE as i32 && p < FROZEN_NULL_FONT as i32 {
                Esc(&PoolString::from(prim[p as usize - PRIM_EQTB_BASE].s1 - 1).to_string())
                    .fmt(f)?;
                ' '.fmt(f)
            } else {
                Esc(&PoolString::from(yhash[p as usize - hash_offset].s1).to_string()).fmt(f)?;
                ' '.fmt(f)
            }
        }
    }
}

use crate::xetex_texmfmp::gettexstring;
use crate::xetex_xetex0::FileName;
impl fmt::Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: optimize
        let n = self.name;
        let a = self.area;
        let e = self.ext;
        if f.alternate() {
            let mut must_quote: bool = false;
            let mut quote_char = None;

            fn find_quote(s: i32, quote_char: &mut Option<char>, must_quote: &mut bool) {
                if s != 0 {
                    for &j in PoolString::from(s).as_slice() {
                        if *must_quote && quote_char.is_some() {
                            break;
                        }
                        if j == b' ' as _ {
                            *must_quote = true;
                        } else if j == b'\"' as _ {
                            *must_quote = true;
                            *quote_char = Some('\'');
                        } else if j == b'\'' as _ {
                            *must_quote = true;
                            *quote_char = Some('\"');
                        }
                    }
                }
            }
            find_quote(a, &mut quote_char, &mut must_quote);
            find_quote(n, &mut quote_char, &mut must_quote);
            find_quote(e, &mut quote_char, &mut must_quote);

            if must_quote {
                if let Some(qc) = quote_char {
                    qc.fmt(f)?;
                } else {
                    quote_char = Some('\"');
                    '\"'.fmt(f)?;
                }
            }

            fn fmt_name_part(
                f: &mut fmt::Formatter,
                s: i32,
                quote_char: &mut Option<char>,
            ) -> fmt::Result {
                if s != 0 {
                    for j in std::char::decode_utf16(PoolString::from(s).as_slice().iter().cloned())
                    {
                        let j = j.unwrap();
                        if Some(j) == *quote_char {
                            j.fmt(f)?;
                            let c = match j {
                                '\"' => '\'',
                                '\'' => '\"',
                                _ => unreachable!(),
                            };
                            *quote_char = Some(c);
                            c.fmt(f)?;
                        }
                        j.fmt(f)?;
                    }
                }
                Ok(())
            }
            fmt_name_part(f, a, &mut quote_char)?;
            fmt_name_part(f, n, &mut quote_char)?;
            fmt_name_part(f, e, &mut quote_char)?;

            if let Some(qc) = quote_char {
                qc.fmt(f)?;
            };
            Ok(())
        } else {
            gettexstring(a).fmt(f)?;
            gettexstring(n).fmt(f)?;
            gettexstring(e).fmt(f)
        }
    }
}

impl fmt::Display for crate::node::whatsit::WriteFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Esc("write").fmt(f)?;
        let i = unsafe { self.id() };
        match i {
            0..=15 => i.fmt(f),
            16 => '*'.fmt(f),
            _ => '-'.fmt(f),
        }
    }
}

impl fmt::Display for crate::node::whatsit::OpenFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Esc("openout").fmt(f)?;
        unsafe { self.id() }.fmt(f)
    }
}

impl fmt::Display for crate::node::whatsit::CloseFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Esc("closeout").fmt(f)?;
        unsafe { self.id() }.fmt(f)
    }
}

impl fmt::Display for NativeWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for c in std::char::decode_utf16(unsafe { self.text() }.iter().cloned()) {
            if let Ok(c) = c {
                c.fmt(f)?;
            } else {
                ".".fmt(f)?;
            }
        }
        Ok(())
    }
}

pub(crate) struct SaNum(pub usize);
impl fmt::Display for SaNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            let mut q = self.0;
            let mut n;
            if MEM[q].b16.s1 < DIMEN_VAL_LIMIT {
                n = MEM[q + 1].b32.s1
            } else {
                n = MEM[q].b16.s1 as i32 % 64;
                q = MEM[q].b32.s1 as usize;
                n += 64 * MEM[q].b16.s1 as i32;
                q = MEM[q].b32.s1 as usize;
                n += 64
                    * 64
                    * (MEM[q].b16.s1 as i32 + 64 * MEM[MEM[q].b32.s1 as usize].b16.s1 as i32)
            }
            n.fmt(f)
        }
    }
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
        const ROMAN_DATA: &[u8] = b"m2d5c2l5x2v5i";
        let mut n = self.0;
        let mut j = 0;
        let mut v = 1000;
        loop {
            while n >= v {
                char::from(ROMAN_DATA[j]).fmt(f)?;
                n -= v;
            }
            if n <= 0 {
                return Ok(());
            }
            let mut k = j + 2;
            let mut u = v / ((ROMAN_DATA[k - 1] - b'0') as i32);
            if ROMAN_DATA[k - 1] == b'2' {
                k += 2;
                u /= (ROMAN_DATA[k - 1] - b'0') as i32;
            }
            if n + u >= v {
                char::from(ROMAN_DATA[k]).fmt(f)?;
                n += u;
            } else {
                j += 2;
                v /= (ROMAN_DATA[j - 1] - b'0') as i32;
            }
        }
    }
}
