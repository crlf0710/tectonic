/* tectonic/xetex-xetexd.h -- many, many XeTeX symbol definitions
   Copyright 2016-2018 The Tectonic Project
   Licensed under the MIT License.
*/
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::{t_print, t_print_nl};

use super::xetex_consts::{
    get_int_par, IntPar, ACTIVE_BASE, BIGGEST_USV, CAT_CODE, DIMEN_VAL_LIMIT, EQTB_SIZE, HASH_BASE,
    NULL_CS, SINGLE_BASE, UNDEFINED_CONTROL_SEQUENCE,
};
use crate::cmd::Cmd;
use crate::node::NativeWord;
use crate::xetex_stringpool::{str_ptr, PoolString};

use super::xetex_ini::{
    hash_offset, line, max_print_line, yhash, EQTB_TOP, FULL_SOURCE_FILENAME_STACK, IN_OPEN,
    LINE_STACK, MEM,
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

pub(crate) static mut write_file: [Option<WFile>; 16] = [
    None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None,
];

pub(crate) static mut selector: Selector = Selector::NoPrint;

pub(crate) static mut rust_stdout: Option<LogTermOutput> = None;

pub(crate) static mut log_file: Option<LogTermOutput> = None;

pub(crate) static mut log_opened: bool = false;

pub(crate) static mut trick_buf: Vec<char> = Vec::new();

use bridge::OutputHandleWrapper;

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Selector {
    NoPrint,
    TermOnly,
    LogOnly,
    TermAndLog,
}

impl Selector {
    #[inline]
    pub unsafe fn zero_tally(&mut self) {
        match self {
            Selector::TermAndLog => {
                rust_stdout.as_mut().unwrap().tally = 0;
                log_file.as_mut().unwrap().tally = 0;
            }
            Selector::LogOnly => {
                log_file.as_mut().unwrap().tally = 0;
            }
            Selector::TermOnly => {
                rust_stdout.as_mut().unwrap().tally = 0;
            }
            Selector::NoPrint => {}
        }
    }
    #[inline]
    pub unsafe fn get_max_tally(&self) -> usize {
        match self {
            Selector::TermAndLog => rust_stdout
                .as_ref()
                .unwrap()
                .tally
                .max(log_file.as_ref().unwrap().tally),
            Selector::LogOnly => log_file.as_ref().unwrap().tally,
            Selector::TermOnly => rust_stdout.as_ref().unwrap().tally,
            Selector::NoPrint => 0,
        }
    }
}

pub(crate) struct WFile(pub(crate) OutputHandleWrapper);
impl WFile {
    pub(crate) fn new(handler: OutputHandleWrapper) -> Self {
        Self(handler)
    }
    #[inline]
    pub(crate) fn write_ln(&mut self) -> std::io::Result<()> {
        use std::io::Write;
        self.write_all(b"\n")
    }
    #[inline]
    fn print_char(&mut self, s: char, nl: i32) -> io::Result<()> {
        use std::io::Write;
        if (s as i32) == nl {
            self.write_all(b"\n")?;
        } else if s.is_control() {
            let (buf, len) = replace_control(s);
            for &c in &buf[..len] {
                write!(self, "{}", char::from(c))?;
            }
        } else {
            write!(self, "{}", s)?;
        }
        Ok(())
    }
}
impl core::ops::Deref for WFile {
    type Target = OutputHandleWrapper;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl core::ops::DerefMut for WFile {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) struct LogTermOutput {
    pub(crate) handler: OutputHandleWrapper,
    pub(crate) offset: usize,
    pub(crate) max_line_width: usize,
    pub(crate) tally: usize,
}

impl LogTermOutput {
    pub(crate) fn new(handler: OutputHandleWrapper) -> Self {
        Self {
            handler,
            offset: 0,
            max_line_width: unsafe { max_print_line },
            tally: 0,
        }
    }
    #[inline]
    fn write_ln(&mut self) -> std::io::Result<()> {
        use std::io::Write;
        self.offset = 0;
        self.write_all(b"\n")
    }
    #[inline]
    fn write_chr(&mut self, c: char) -> std::io::Result<()> {
        use std::io::Write;
        write!(self, "{}", c)?;
        self.offset += 1;
        if self.offset == self.max_line_width {
            self.write_ln()?;
        }
        Ok(())
    }
    #[inline]
    fn print_char(&mut self, s: char, nl: i32) -> io::Result<()> {
        if (s as i32) == nl {
            self.write_ln()?;
        } else {
            if s.is_control() {
                let (buf, len) = replace_control(s);
                for &c in &buf[..len] {
                    self.write_chr(char::from(c))?;
                }
            } else {
                self.write_chr(s)?;
            }
            self.tally += 1;
        }
        Ok(())
    }
}

impl core::ops::Deref for LogTermOutput {
    type Target = OutputHandleWrapper;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.handler
    }
}
impl core::ops::DerefMut for LogTermOutput {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.handler
    }
}

#[inline]
pub(crate) unsafe fn print_ln() {
    match selector {
        Selector::TermAndLog => {
            rust_stdout.as_mut().unwrap().write_ln().unwrap();
            log_file.as_mut().unwrap().write_ln().unwrap();
        }
        Selector::LogOnly => {
            log_file.as_mut().unwrap().write_ln().unwrap();
        }
        Selector::TermOnly => {
            rust_stdout.as_mut().unwrap().write_ln().unwrap();
        }
        Selector::NoPrint => {}
    };
}

use std::io;

pub(crate) struct Pseudo {
    pub(crate) tally: usize,
    pub(crate) first_count: usize,
    pub(crate) trick_count: usize,
}
impl Pseudo {
    pub(crate) fn new() -> Self {
        unsafe {
            trick_buf.clear();
        }
        Self {
            tally: 0,
            first_count: 0,
            trick_count: 1_000_000,
        }
    }
}
impl fmt::Write for Pseudo {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }
        Ok(())
    }
    #[inline]
    fn write_char(&mut self, s: char) -> fmt::Result {
        unsafe {
            let mut count;
            if s.is_control() {
                let (buf, len) = replace_control(s);
                count = 0;
                for &c in &buf[..len] {
                    if self.tally < self.trick_count {
                        trick_buf.push(char::from(c));
                    }
                    count += 1;
                }
            } else {
                count = 1;
                if self.tally < self.trick_count {
                    trick_buf.push(s);
                }
            }
            self.tally += count;
        }
        Ok(())
    }
}

#[inline]
pub(crate) unsafe fn print_chr(c: char) {
    use std::fmt::Write;
    selector.write_char(c).unwrap();
}

use std::fmt;
impl fmt::Write for LogTermOutput {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        use std::ops::DerefMut;
        let nl = unsafe { get_int_par(IntPar::new_line_char) };
        let bytelen = s.len();
        if (self.offset + bytelen < self.max_line_width)
            && !s.contains(|c: char| (c as i32) == nl || c.is_control())
        {
            std::io::Write::write(self.deref_mut(), s.as_bytes()).unwrap();
            self.offset += bytelen;
            self.tally += bytelen;
        } else {
            for c in s.chars() {
                self.print_char(c, nl).unwrap();
            }
        }
        Ok(())
    }
    #[inline]
    fn write_char(&mut self, s: char) -> fmt::Result {
        let nl = unsafe { get_int_par(IntPar::new_line_char) };
        self.print_char(s, nl).unwrap();
        Ok(())
    }
}

impl fmt::Write for Selector {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        unsafe {
            match self {
                Selector::TermAndLog => {
                    rust_stdout.as_mut().unwrap().write_str(s).unwrap();
                    log_file.as_mut().unwrap().write_str(s).unwrap();
                }
                Selector::LogOnly => {
                    log_file.as_mut().unwrap().write_str(s).unwrap();
                }
                Selector::TermOnly => {
                    rust_stdout.as_mut().unwrap().write_str(s).unwrap();
                }
                Selector::NoPrint => {}
            }
        }
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, s: char) -> fmt::Result {
        unsafe {
            match selector {
                Selector::TermAndLog => {
                    rust_stdout.as_mut().unwrap().write_char(s).unwrap();
                    log_file.as_mut().unwrap().write_char(s).unwrap();
                }
                Selector::LogOnly => {
                    log_file.as_mut().unwrap().write_char(s).unwrap();
                }
                Selector::TermOnly => {
                    rust_stdout.as_mut().unwrap().write_char(s).unwrap();
                }
                Selector::NoPrint => {}
            }
        }
        Ok(())
    }
}
impl fmt::Write for WFile {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        use std::io::Write as IoWrite;
        use std::ops::DerefMut;
        unsafe {
            let nl = get_int_par(IntPar::new_line_char);
            if !s.contains(|c: char| (c as i32) == nl || c.is_control()) {
                IoWrite::write(self.deref_mut(), s.as_bytes()).unwrap();
            } else {
                for c in s.chars() {
                    self.print_char(c, nl).unwrap();
                }
            }
        }
        Ok(())
    }
    #[inline]
    fn write_char(&mut self, s: char) -> fmt::Result {
        unsafe {
            let nl = get_int_par(IntPar::new_line_char);
            self.print_char(s, nl).unwrap();
        }
        Ok(())
    }
}

#[inline]
fn replace_control(s: char) -> ([u8; 4], usize) {
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

#[inline]
pub(crate) unsafe fn printnl() {
    match selector {
        Selector::TermAndLog => {
            let stdout = rust_stdout.as_mut().unwrap();
            if stdout.offset > 0 {
                stdout.write_ln().unwrap();
            }
            let lg = log_file.as_mut().unwrap();
            if lg.offset > 0 {
                lg.write_ln().unwrap();
            }
        }
        Selector::TermOnly => {
            let stdout = rust_stdout.as_mut().unwrap();
            if stdout.offset > 0 {
                stdout.write_ln().unwrap();
            }
        }
        Selector::LogOnly => {
            let lg = log_file.as_mut().unwrap();
            if lg.offset > 0 {
                lg.write_ln().unwrap();
            }
        }
        Selector::NoPrint => {}
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
#[inline]
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
