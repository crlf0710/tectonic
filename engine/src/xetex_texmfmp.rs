#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::xetex_stringpool::{
    make_string, pool_ptr, str_pool, PoolString, EMPTY_STRING, TOO_BIG_CHAR,
};
use bridge::ttstub_get_file_md5;
use std::ffi::CString;

pub(crate) type str_number = i32;
/* texmfmp.c: Hand-coded routines for TeX or Metafont in C.  Originally
written by Tim Morgan, drawing from other Unix ports of TeX.  This is
a collection of miscellany, everything that's easier (or only
possible) to do in C.

This file is public domain.  */
static mut last_source_name: String = String::new();
static mut last_lineno: i32 = 0;

static mut start_time_str: String = String::new();
/* minimum size for time_str is 24: "D:YYYYmmddHHMMSS+HH'MM'" */

use chrono::prelude::*;

trait TexTimeFormat {
    fn time_format(&self) -> String;
}

impl TexTimeFormat for DateTime<Utc> {
    fn time_format(&self) -> String {
        const TIMEFORMAT: &str = "D:%Y%m%d%H%M%S";
        format!("{}-00'00'", self.format(TIMEFORMAT))
    }
}
impl TexTimeFormat for DateTime<Local> {
    fn time_format(&self) -> String {
        const TIMEFORMAT: &str = "D:%Y%m%d%H%M%S";
        let tz = format!("{}", self.format("%z"));
        format!("{}{}'{}'", self.format(TIMEFORMAT), &tz[0..3], &tz[3..])
    }
}

use std::time::SystemTime;
pub(crate) fn get_unique_time_if_given() -> Option<SystemTime> {
    use std::time::Duration;
    std::env::var("SOURCE_DATE_EPOCH")
        .ok()
        .map(|x| {
            x.trim()
                .parse::<u64>()
                .ok()
                .map(|x| SystemTime::UNIX_EPOCH.checked_add(Duration::new(x, 0)))
        })
        .unwrap_or(None)
        .unwrap_or(None)
}

pub(crate) unsafe fn init_start_time() {
    start_time_str = match get_unique_time_if_given() {
        Some(x) => DateTime::<Utc>::from(x).time_format(),
        None => Local::now().time_format(),
    };
}

pub(crate) unsafe fn getcreationdate() -> &'static str {
    &start_time_str
}

pub(crate) fn get_date_and_time() -> (i32, i32, i32, i32) {
    use chrono::prelude::*;

    let tm: DateTime<Local> = get_unique_time_if_given()
        .unwrap_or_else(SystemTime::now)
        .into();

    let year = tm.year();
    let month = tm.month();
    let day = tm.day();
    let minutes = tm.hour() * 60 + tm.minute();

    (minutes as _, day as _, month as _, year)
}

pub(crate) fn get_seconds_and_micros(seconds: &mut i32, micros: &mut i32) {
    let tm = chrono::Local::now();
    *seconds = tm.timestamp_millis() as i32;
    *micros = tm.timestamp_subsec_micros() as i32;
}

pub(crate) fn getfilemoddate(path: &str) -> std::io::Result<String> {
    use std::fs::File;
    let f = File::open(path)?;
    let meta = f.metadata()?;
    // TODO: use_utc = FORCE_SOURCE_DATE_set && SOURCE_DATE_EPOCH_set
    let stm = match meta.modified().ok().or_else(|| {
        get_unique_time_if_given().filter(|_| std::env::var("FORCE_SOURCE_DATE").is_ok())
    }) {
        Some(x) => DateTime::<Utc>::from(x).time_format(),
        None => Local::now().time_format(),
    };
    Ok(stm)
}
pub(crate) fn getfilesize(path: &str) -> std::io::Result<String> {
    use std::fs::File;
    let f = File::open(path)?;
    let meta = f.metadata()?;
    Ok(meta.len().to_string())
}
pub(crate) fn getfiledump(path: &str, offset: i32, length: i32) -> std::io::Result<String> {
    use std::fs::File;
    use std::io::{Read, Seek, SeekFrom};
    if length == 0 {
        return Ok(String::new());
    }
    let length = length as usize;
    let mut f = File::open(path)?;
    f.seek(SeekFrom::Start(offset as _))?;
    let mut buffer = vec![0; length];
    f.read(&mut buffer)?;
    let mut s = String::with_capacity(3 * length);
    for b in buffer.iter() {
        s.push_str(&format!("{:.2X}", b)); // TODO: check format
    }
    Ok(s)
}

pub(crate) unsafe fn maketexstring(s: &str) -> i32 {
    if s.is_empty() {
        return EMPTY_STRING;
    }
    PoolString::check_capacity(s.len());
    let v: Vec<u16> = s.encode_utf16().collect();
    let len = v.len();
    str_pool[pool_ptr..pool_ptr + len].copy_from_slice(v.as_slice());
    pool_ptr += len;
    make_string()
}
pub(crate) fn gettexstring(s: str_number) -> String {
    if s >= TOO_BIG_CHAR {
        String::from_utf16(PoolString::from(s).as_slice()).unwrap()
    } else {
        String::new()
    }
}
pub(crate) unsafe fn is_new_source(srcfilename: str_number, lineno: i32) -> bool {
    use std::path::Path;
    Path::new(&gettexstring(srcfilename)) != Path::new(&last_source_name) || lineno != last_lineno
}
pub(crate) unsafe fn remember_source_info(srcfilename: str_number, lineno: i32) {
    last_source_name = gettexstring(srcfilename);
    last_lineno = lineno;
}
pub(crate) unsafe fn make_src_special(srcfilename: str_number, lineno: i32) -> String {
    // Always put a space after the number, which makes things easier to parse.
    format!("src:{} {}", lineno, &gettexstring(srcfilename))
}
/* Converts any given string in into an allowed PDF string which is
 * hexadecimal encoded;
 * sizeof(out) should be at least lin*2+1.
 */
unsafe fn convertStringToHexString(in_0: &[u8; 16], out: &mut [u8; 33]) {
    const HEXCHARS: &[u8] = b"0123456789ABCDEF";
    let mut j = 0;
    for &i in in_0 {
        let c = i;
        out[j] = HEXCHARS[(c >> 4 & 0xf) as usize];
        j += 1;
        out[j] = HEXCHARS[(c & 0xf) as usize];
        j += 1;
    }
    out[j] = 0;
}
/* Functions originating in texmfmp.c */
pub(crate) unsafe fn getmd5sum(s: &str, file: bool) -> String {
    let ret;
    let xname = CString::new(s).unwrap();
    let digest = if file {
        let mut digest: [i8; 16] = [0; 16];
        ret = ttstub_get_file_md5(xname.as_ptr(), digest.as_mut_ptr());
        std::mem::transmute(digest)
    } else {
        ret = 0;
        md5::compute(xname.as_bytes())
    };
    if ret != 0 {
        return String::new();
    }
    let mut outbuf: [u8; 33] = [0; 33];
    convertStringToHexString(&digest, &mut outbuf);
    let mut v = Vec::with_capacity(2 * 16);
    for i in 0..(2 * 16) {
        v.push(outbuf[i as usize])
    }
    String::from_utf8(v).unwrap()
}
