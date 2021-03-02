/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2018 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team.

    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/
#![allow(
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use std::io::{Read, Seek, SeekFrom};

use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_mfileio::tt_mfgets;

use bridge::{InFile, TTInputFormat};
use libc::{strchr, strlen, strtol};

/* Don't forget fontmap reading now requires information
 * from SFD files. You must initialize at least sfd_file_
 * cache before starting loading of fontmaps.
 */
/* Subfont Definition File:
 *  struct sfd_file_ is for storing subfont identifiers
 *  contained in a SFD file and for mapping string pair
 *  <SFD_file, Subfont_id> to internal code mapping table
 *  ID which is index within an array of struct sfd_rec_.
 *  We store code mapping tables in different place than
 *  struct sfd_file_.
 */
#[derive(Clone)]
#[repr(C)]
pub(crate) struct sfd_file_ {
    pub(crate) ident: String,
    pub(crate) sub_id: Vec<String>,
    pub(crate) rec_id: Vec<i32>,
}
impl sfd_file_ {
    unsafe fn new() -> Self {
        Self {
            ident: String::new(),
            sub_id: Vec::new(),
            rec_id: Vec::new(),
        }
    }
}
/* Mapping table */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct sfd_rec_ {
    pub(crate) vector: [u16; 256],
    /* 0 for undefined */
}
impl sfd_rec_ {
    pub(crate) const fn new() -> Self {
        Self { vector: [0; 256] }
    }
}

static mut verbose: i32 = 0;

pub(crate) unsafe fn subfont_set_verbose(level: i32) {
    verbose = level;
}
static mut sfd_files: Vec<sfd_file_> = Vec::new();
static mut sfd_record: Vec<sfd_rec_> = Vec::new();
static mut line_buf: [i8; 4096] = [0; 4096];
/* Each lines describes character code mapping for each
 * subfonts. '#' is start of comment.
 * SFD file format uses a '\' before newline sequence
 * for line-continuation.
 */
unsafe fn readline<R: Read + Seek>(buf: *mut i8, buf_len: usize, handle: &mut R) -> *mut i8 {
    let mut p: *mut i8 = buf;
    let mut n = 0;
    let mut c: i32 = 0;
    while n < buf_len {
        let mut q = tt_mfgets(p, buf_len - n, handle);
        if q.is_null() {
            break;
        }
        c += 1;
        let r = strchr(q, '#' as i32);
        /* Comment is converted to single wsp (followed by a newline). */
        if !r.is_null() {
            *r = ' ' as i32 as i8; /* empty line */
            *r.offset(1) = '\u{0}' as i32 as i8
        }
        let qlen = strlen(q) as usize;
        if qlen == 0 {
            break;
        }
        n += qlen;
        q = q.add(qlen - 1);
        if *q as i32 != '\\' as i32 {
            break;
        }
        /* line continued */
        n -= 1;
        p = buf.offset(n as isize)
    }
    if n >= buf_len - 1 {
        warn!(
            "Possible buffer overflow in reading SFD file (buffer full, size={} bytes)",
            buf_len - 1
        );
    }
    if c > 0 {
        buf
    } else {
        ptr::null_mut()
    }
}
/* subfont_id ( integer ':' | integer '_' integer | integer )*
 *
 *  0x32: ==> Subsequent integers are place into slots starting at 0x32.
 *    0x32: 0xA1A1 0xA1A2 ... ==> 0x32 is mappned to 0xA1A1, 0x33 to 0xA1A2
 *  0xA1A1_0xA1A5 ==> Expanded to 0xA1A1 0xA1A2 ... 0xA1A5
 */
/* subfont_id is already consumed here. */
unsafe fn read_sfd_record(rec: &mut sfd_rec_, lbuf: *const i8) -> i32 {
    let mut p: *const i8 = lbuf;
    let mut r: *mut i8 = ptr::null_mut();
    let mut v2: i32 = 0;
    let mut curpos: i32 = 0;
    let error: i32 = 0;
    while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
        p = p.offset(1)
    }
    while error == 0 && *p as i32 != 0 {
        let mut repos = 0;
        //q = p;
        let v1 = strtol(p, &mut r, 0) as i32;
        let mut q = r as *const i8;
        if q == p
            || !(*q as i32 == '\u{0}' as i32 || libc::isspace(*q as _) != 0)
                && *q as i32 != ':' as i32
                && *q as i32 != '_' as i32
        {
            warn!(
                "Unknown token in subfont mapping table: {}",
                char::from(*q as u8),
            );
            return -1;
        }
        match *q as i32 {
            58 => {
                if v1 < 0 || v1 > 0xff {
                    warn!("Invalud value for subfont table offset: {}", v1);
                    return -1;
                }
                repos = 1;
                q = q.offset(1)
            }
            95 => {
                p = q.offset(1);
                v2 = strtol(p, &mut r, 0) as i32;
                q = r;
                if v1 < 0 || v1 as i64 > 0xffff || v2 < 0 || v2 as i64 > 0xffff {
                    warn!(
                        "Invalid value in subfont mapping table: 0x{:x}_0x{:x}",
                        v1, v2,
                    );
                    return -1;
                } else {
                    if q == p || !(*q as i32 == '\u{0}' as i32 || libc::isspace(*q as _) != 0) {
                        warn!(
                            "Invalid char in subfont mapping table: {}",
                            char::from(*q as u8),
                        );
                        return -1;
                    }
                }
            }
            _ => {
                if v1 < 0 || v1 as i64 > 0xffff {
                    warn!(
                        "Invalid character code in subfont mapping table: 0x{:x}",
                        v1,
                    );
                    return -1;
                }
                v2 = v1
            }
        }
        if repos != 0 {
            curpos = v1
        } else {
            if v2 < v1 || curpos + (v2 - v1) > 0xff {
                warn!("Invalid range in subfont mapping: curpos=\"0x{:02x}\" range=\"0x{:04x},0x{:04x}\"", curpos,
                            v1, v2);
                return -1;
            }
            for c in v1..=v2 {
                if rec.vector[curpos as usize] != 0 {
                    warn!(
                        "Subfont mapping for slot=\"0x{:02x}\" already defined...",
                        curpos,
                    );
                    return -1;
                }
                assert!(curpos >= 0 && curpos <= 255);
                rec.vector[curpos as usize] = c as u16;
                curpos += 1;
            }
        }
        p = q;
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
    }
    error
}
/* Scan for subfont IDs */
unsafe fn scan_sfd_file<R: Read + Seek>(sfd: &mut sfd_file_, handle: &mut R) -> i32 {
    let mut lpos: i32 = 0;
    if verbose > 3 {
        info!("\nsubfont>> Scanning SFD file \"{}\"...\n", sfd.ident);
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    loop {
        let mut p = readline(line_buf.as_mut_ptr(), 4096, handle);
        if p.is_null() {
            break;
        }
        lpos += 1;
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        if *p as i32 == 0 {
            continue;
        }
        /* Saw non-wsp here */
        let mut n = 0;
        let q = p;
        while *p as i32 != 0 && libc::isspace(*p as _) == 0 {
            p = p.offset(1);
            n += 1
        }
        let slice = std::slice::from_raw_parts(q as *const u8, n as usize);
        let id = String::from_utf8_lossy(slice).to_string();
        if verbose > 3 {
            info!("subfont>>   id=\"{}\" at line=\"{}\"\n", id, lpos);
        }
        sfd.sub_id.push(id);
    }
    sfd.rec_id = vec![-1; sfd.sub_id.len() as usize];
    /* Not loaded yet. We do lazy loading of map definitions. */
    if verbose > 3 {
        info!(
            "subfont>> {} entries found in SFD file \"{}\".\n",
            sfd.sub_id.len(),
            sfd.ident,
        );
    }
    0
}
/* Open SFD file and gather subfont IDs. We do not read mapping tables
 * here but only read subfont IDs used in SFD file.
 */
unsafe fn find_sfd_file(sfd_name: &str) -> i32 {
    let mut id: i32 = -1;
    /* Check if we already opened SFD file */
    for (i, sfd) in sfd_files.iter().enumerate() {
        if sfd.ident == sfd_name {
            id = i as i32;
            break;
        }
    }
    if id < 0 {
        let mut sfd = sfd_file_::new();
        sfd.ident = sfd_name.to_string();
        if let Some(mut handle) = InFile::open(&sfd.ident, TTInputFormat::SFD, 0) {
            let error = scan_sfd_file(&mut sfd, &mut handle);
            if error == 0 {
                id = sfd_files.len() as i32;
                sfd_files.push(sfd);
            } else {
                warn!("Error occured while reading SFD file \"{}\"", sfd_name);
                id = -1
            }
        } else {
            return -1;
        }
    }
    id
}

pub(crate) unsafe fn sfd_get_subfont_ids(sfd_name: &str) -> &[String] {
    if sfd_name.is_empty() {
        return &[];
    }
    let sfd_id = find_sfd_file(sfd_name);
    if sfd_id < 0 {
        return &[];
    }
    &sfd_files[sfd_id as usize].sub_id
}
/* Make sure that sfd_name does not have the extension '.sfd'.
 * Mapping tables are actually read here.
 */

pub(crate) unsafe fn sfd_load_record(sfd_name: &str, subfont_id: &str) -> i32 {
    let mut rec_id: i32 = -1;
    if sfd_name.is_empty() || subfont_id.is_empty() {
        return -1;
    }
    let sfd_id = find_sfd_file(sfd_name);
    if sfd_id < 0 {
        return -1;
    }
    let sfd = &mut sfd_files[sfd_id as usize];
    /* Check if we already loaded mapping table. */
    let mut i = 0;
    while i < sfd.sub_id.len() && sfd.sub_id[i] != subfont_id {
        i += 1
    }
    if i == sfd.sub_id.len() {
        warn!(
            "Subfont id=\"{}\" not exist in SFD file \"{}\"...",
            subfont_id, sfd.ident,
        );
        return -1;
    } else {
        if sfd.rec_id[i] >= 0 {
            return sfd.rec_id[i];
        }
    }
    if verbose > 3 {
        info!(
            "\nsubfont>> Loading SFD mapping table for <{},{}>...",
            sfd.ident, subfont_id,
        );
    }
    /* reopen */
    let handle = InFile::open(&sfd.ident, TTInputFormat::SFD, 0);
    if handle.is_none() {
        return -1;
        /* panic!("Could not open SFD file \"{}\"", sfd_name); */
    }
    let mut handle = handle.unwrap();
    loop
    /* Seek to record for 'sub_name'. */
    {
        let mut p = readline(line_buf.as_mut_ptr(), 4096, &mut handle); /* empty line */
        if p.is_null() {
            break;
        }
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        if *p as i32 == 0 {
            continue;
        }
        /* q = parse_ident(&p, p + strlen(p)); */
        let q = p;
        while *p as i32 != 0 && libc::isspace(*p as _) == 0 {
            p = p.offset(1)
        }
        *p = '\u{0}' as i32 as i8;
        p = p.offset(1);
        if CStr::from_ptr(q).to_str().unwrap() == subfont_id {
            let mut rec = sfd_rec_::new();
            let error = read_sfd_record(&mut rec, p);
            if error != 0 {
                warn!(
                    "Error occured while reading SFD file: file=\"{}\" subfont_id=\"{}\"",
                    sfd.ident, subfont_id,
                );
            } else {
                rec_id = sfd_record.len() as i32;
                sfd_record.push(rec);
            }
        }
    }
    if rec_id < 0 {
        warn!(
            "Failed to load subfont mapping table for SFD=\"{}\" subfont_id=\"{}\"",
            sfd.ident, subfont_id,
        );
    }
    sfd.rec_id[i] = rec_id;
    if verbose > 3 {
        if rec_id >= 0 {
            info!(" at id=\"{}\"", rec_id);
            info!("\nsubfont>> Content of mapping table:");
            for i in 0..256 {
                if i % 16 == 0 {
                    info!("\nsubfont>>  ");
                }
                info!(
                    " {:04x}",
                    sfd_record[rec_id as usize].vector[i],
                );
            }
        }
        info!("\n");
    }
    rec_id
}
/* Lookup mapping table */

pub(crate) unsafe fn lookup_sfd_record(rec_id: i32, c: u8) -> u16 {
    if sfd_record.is_empty() || rec_id < 0 || rec_id >= sfd_record.len() as i32 {
        panic!("Invalid subfont_id: {}", rec_id);
    }
    sfd_record[rec_id as usize].vector[c as usize]
}

pub(crate) unsafe fn release_sfd_record() {
    sfd_record = Vec::new();
    sfd_files = Vec::new();
}
