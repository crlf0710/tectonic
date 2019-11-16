/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2016 by Jin-Hwan Cho and Shunsaku Hirata,
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
    non_upper_case_globals,
    unused_mut
)]

use std::io::{Seek, SeekFrom};

use crate::streq_ptr;
use crate::DisplayExt;
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_mem::{new, renew};
use super::dpx_mfileio::tt_mfgets;
use crate::{ttstub_input_close, ttstub_input_open};
use libc::{free, memcpy, strchr, strcmp, strcpy, strlen, strtol};

pub type __ssize_t = i64;

use crate::TTInputFormat;

use bridge::InputHandleWrapper;
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
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sfd_file_ {
    pub ident: *mut i8,
    pub sub_id: *mut *mut i8,
    pub rec_id: *mut i32,
    pub max_subfonts: i32,
    pub num_subfonts: i32,
}
/* Mapping table */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sfd_rec_ {
    pub vector: [u16; 256],
    /* 0 for undefined */
}
static mut verbose: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn subfont_set_verbose(mut level: i32) {
    verbose = level;
}
unsafe fn init_sfd_file_(mut sfd: *mut sfd_file_) {
    (*sfd).ident = ptr::null_mut();
    (*sfd).sub_id = 0 as *mut *mut i8;
    (*sfd).rec_id = ptr::null_mut();
    (*sfd).num_subfonts = 0i32;
    (*sfd).max_subfonts = (*sfd).num_subfonts;
}
unsafe fn clean_sfd_file_(mut sfd: *mut sfd_file_) {
    free((*sfd).ident as *mut libc::c_void);
    if !(*sfd).sub_id.is_null() {
        for i in 0..(*sfd).num_subfonts {
            free(*(*sfd).sub_id.offset(i as isize) as *mut libc::c_void);
        }
        free((*sfd).sub_id as *mut libc::c_void);
    }
    free((*sfd).rec_id as *mut libc::c_void);
    init_sfd_file_(sfd);
}
static mut sfd_files: *mut sfd_file_ = std::ptr::null_mut();
static mut num_sfd_files: i32 = 0i32;
static mut max_sfd_files: i32 = 0i32;
static mut sfd_record: *mut sfd_rec_ = std::ptr::null_mut();
static mut num_sfd_records: i32 = 0i32;
static mut max_sfd_records: i32 = 0i32;
static mut line_buf: [i8; 4096] = [0; 4096];
/* Each lines describes character code mapping for each
 * subfonts. '#' is start of comment.
 * SFD file format uses a '\' before newline sequence
 * for line-continuation.
 */
unsafe fn readline(
    mut buf: *mut i8,
    mut buf_len: i32,
    handle: &mut InputHandleWrapper,
) -> *mut i8 {
    let mut q: *mut i8 = ptr::null_mut();
    let mut p: *mut i8 = buf;
    let mut n: i32 = 0i32;
    let mut c: i32 = 0i32;
    while buf_len - n > 0i32 && {
        q = tt_mfgets(p, buf_len - n, handle);
        !q.is_null()
    } {
        c += 1;
        let r = strchr(q, '#' as i32);
        /* Comment is converted to single wsp (followed by a newline). */
        if !r.is_null() {
            *r = ' ' as i32 as i8; /* empty line */
            *r.offset(1) = '\u{0}' as i32 as i8
        }
        if strlen(q) == 0 {
            break;
        }
        n = (n as u64).wrapping_add(strlen(q) as _) as _;
        q = q.offset(strlen(q).wrapping_sub(1) as _);
        if *q as i32 != '\\' as i32 {
            break;
        }
        /* line continued */
        n -= 1i32;
        p = buf.offset(n as isize)
    }
    if n >= buf_len - 1i32 {
        warn!(
            "Possible buffer overflow in reading SFD file (buffer full, size={} bytes)",
            buf_len - 1i32
        );
    }
    if c > 0i32 {
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
unsafe fn read_sfd_record(mut rec: *mut sfd_rec_, mut lbuf: *const i8) -> i32 {
    let mut p: *const i8 = lbuf;
    let mut r: *mut i8 = ptr::null_mut();
    let mut v2: i32 = 0i32;
    let mut curpos: i32 = 0i32;
    let mut error: i32 = 0i32;
    while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
        p = p.offset(1)
    }
    while error == 0 && *p as i32 != 0 {
        let mut repos = 0i32;
        //q = p;
        let v1 = strtol(p, &mut r, 0i32) as i32;
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
            return -1i32;
        }
        match *q as i32 {
            58 => {
                if v1 < 0i32 || v1 > 0xffi32 {
                    warn!("Invalud value for subfont table offset: {}", v1);
                    return -1i32;
                }
                repos = 1i32;
                q = q.offset(1)
            }
            95 => {
                p = q.offset(1);
                v2 = strtol(p, &mut r, 0i32) as i32;
                q = r;
                if v1 < 0i32 || v1 as i64 > 0xffff || v2 < 0i32 || v2 as i64 > 0xffff {
                    warn!(
                        "Invalid value in subfont mapping table: 0x{:x}_0x{:x}",
                        v1, v2,
                    );
                    return -1i32;
                } else {
                    if q == p || !(*q as i32 == '\u{0}' as i32 || libc::isspace(*q as _) != 0) {
                        warn!(
                            "Invalid char in subfont mapping table: {}",
                            char::from(*q as u8),
                        );
                        return -1i32;
                    }
                }
            }
            _ => {
                if v1 < 0i32 || v1 as i64 > 0xffff {
                    warn!(
                        "Invalid character code in subfont mapping table: 0x{:x}",
                        v1,
                    );
                    return -1i32;
                }
                v2 = v1
            }
        }
        if repos != 0 {
            curpos = v1
        } else {
            if v2 < v1 || curpos + (v2 - v1) > 0xffi32 {
                warn!("Invalid range in subfont mapping: curpos=\"0x{:02x}\" range=\"0x{:04x},0x{:04x}\"", curpos,
                            v1, v2);
                return -1i32;
            }
            for c in v1..=v2 {
                if (*rec).vector[curpos as usize] as i32 != 0i32 {
                    warn!(
                        "Subfont mapping for slot=\"0x{:02x}\" already defined...",
                        curpos,
                    );
                    return -1i32;
                }
                assert!(curpos >= 0i32 && curpos <= 255i32);
                let fresh0 = curpos;
                curpos = curpos + 1;
                (*rec).vector[fresh0 as usize] = c as u16;
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
unsafe fn scan_sfd_file(mut sfd: *mut sfd_file_, handle: &mut InputHandleWrapper) -> i32 {
    let mut lpos: i32 = 0i32;
    assert!(!sfd.is_null());
    if verbose > 3i32 {
        info!(
            "\nsubfont>> Scanning SFD file \"{}\"...\n",
            CStr::from_ptr((*sfd).ident).display()
        );
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    (*sfd).num_subfonts = 0i32;
    (*sfd).max_subfonts = (*sfd).num_subfonts;
    loop {
        let mut p = readline(line_buf.as_mut_ptr(), 4096i32, handle);
        if p.is_null() {
            break;
        }
        lpos += 1;
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        if *p as i32 == 0i32 {
            continue;
        }
        /* Saw non-wsp here */
        let mut n = 0;
        let q = p;
        while *p as i32 != 0 && libc::isspace(*p as _) == 0 {
            p = p.offset(1);
            n += 1
        }
        let id =
            new(((n + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
                as *mut i8;
        memcpy(id as *mut libc::c_void, q as *const libc::c_void, n as _);
        *id.offset(n as isize) = '\u{0}' as i32 as i8;
        if (*sfd).num_subfonts >= (*sfd).max_subfonts {
            (*sfd).max_subfonts += 16i32;
            (*sfd).sub_id = renew(
                (*sfd).sub_id as *mut libc::c_void,
                ((*sfd).max_subfonts as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<*mut i8>() as u64) as u32,
            ) as *mut *mut i8
        }
        if verbose > 3i32 {
            info!(
                "subfont>>   id=\"{}\" at line=\"{}\"\n",
                CStr::from_ptr(id).display(),
                lpos,
            );
        }
        let ref mut fresh1 = *(*sfd).sub_id.offset((*sfd).num_subfonts as isize);
        *fresh1 = id;
        (*sfd).num_subfonts += 1
    }
    (*sfd).rec_id = new(((*sfd).num_subfonts as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<i32>() as u64) as u32) as *mut i32;
    for n in 0..(*sfd).num_subfonts {
        *(*sfd).rec_id.offset(n as isize) = -1i32;
        /* Not loaded yet. We do lazy loading of map definitions. */
    }
    if verbose > 3i32 {
        info!(
            "subfont>> {} entries found in SFD file \"{}\".\n",
            (*sfd).num_subfonts,
            CStr::from_ptr((*sfd).ident).display(),
        );
    }
    0i32
}
/* Open SFD file and gather subfont IDs. We do not read mapping tables
 * here but only read subfont IDs used in SFD file.
 */
unsafe fn find_sfd_file(mut sfd_name: *const i8) -> i32 {
    let mut id: i32 = -1i32;
    /* Check if we already opened SFD file */
    for i in 0..num_sfd_files {
        if streq_ptr((*sfd_files.offset(i as isize)).ident, sfd_name) {
            id = i;
            break;
        }
    }
    if id < 0i32 {
        if num_sfd_files >= max_sfd_files {
            max_sfd_files += 8i32;
            sfd_files = renew(
                sfd_files as *mut libc::c_void,
                (max_sfd_files as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<sfd_file_>() as u64) as u32,
            ) as *mut sfd_file_
        }
        let sfd = &mut *sfd_files.offset(num_sfd_files as isize) as *mut sfd_file_;
        init_sfd_file_(sfd);
        (*sfd).ident =
            new((strlen(sfd_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
        strcpy((*sfd).ident, sfd_name);
        let handle =
            ttstub_input_open((*sfd).ident, TTInputFormat::SFD, 0i32);
        if handle.is_none() {
            clean_sfd_file_(sfd);
            return -1i32;
        }
        let mut handle = handle.unwrap();
        let error = scan_sfd_file(sfd, &mut handle);
        ttstub_input_close(handle);
        if error == 0 {
            let fresh2 = num_sfd_files;
            num_sfd_files = num_sfd_files + 1;
            id = fresh2
        } else {
            warn!(
                "Error occured while reading SFD file \"{}\"",
                CStr::from_ptr(sfd_name).display(),
            );
            clean_sfd_file_(sfd);
            id = -1i32
        }
    }
    id
}
#[no_mangle]
pub unsafe extern "C" fn sfd_get_subfont_ids(
    mut sfd_name: *const i8,
    mut num_ids: *mut i32,
) -> *mut *mut i8 {
    if sfd_name.is_null() {
        return 0 as *mut *mut i8;
    }
    let sfd_id = find_sfd_file(sfd_name);
    if sfd_id < 0i32 {
        return 0 as *mut *mut i8;
    }
    if !num_ids.is_null() {
        *num_ids = (*sfd_files.offset(sfd_id as isize)).num_subfonts
    }
    (*sfd_files.offset(sfd_id as isize)).sub_id
}
/* Make sure that sfd_name does not have the extension '.sfd'.
 * Mapping tables are actually read here.
 */
#[no_mangle]
pub unsafe extern "C" fn sfd_load_record(
    mut sfd_name: *const i8,
    mut subfont_id: *const i8,
) -> i32 {
    let mut rec_id: i32 = -1i32;
    if sfd_name.is_null() || subfont_id.is_null() {
        return -1i32;
    }
    let sfd_id = find_sfd_file(sfd_name);
    if sfd_id < 0i32 {
        return -1i32;
    }
    let sfd = &mut *sfd_files.offset(sfd_id as isize) as *mut sfd_file_;
    /* Check if we already loaded mapping table. */
    let mut i = 0;
    while i < (*sfd).num_subfonts && strcmp(*(*sfd).sub_id.offset(i as isize), subfont_id) != 0 {
        i += 1
    }
    if i == (*sfd).num_subfonts {
        warn!(
            "Subfont id=\"{}\" not exist in SFD file \"{}\"...",
            CStr::from_ptr(subfont_id).display(),
            CStr::from_ptr((*sfd).ident).display(),
        );
        return -1i32;
    } else {
        if *(*sfd).rec_id.offset(i as isize) >= 0i32 {
            return *(*sfd).rec_id.offset(i as isize);
        }
    }
    if verbose > 3i32 {
        info!(
            "\nsubfont>> Loading SFD mapping table for <{},{}>...",
            CStr::from_ptr((*sfd).ident).display(),
            CStr::from_ptr(subfont_id).display(),
        );
    }
    /* reopen */
    let handle =
        ttstub_input_open((*sfd).ident, TTInputFormat::SFD, 0i32);
    if handle.is_none() {
        return -1i32;
        /* panic!("Could not open SFD file \"{}\"", sfd_name); */
    }
    let mut handle = handle.unwrap();
    loop
    /* Seek to record for 'sub_name'. */
    {
        let mut p = readline(line_buf.as_mut_ptr(), 4096i32, &mut handle); /* empty line */
        if p.is_null() {
            break;
        }
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        if *p as i32 == 0i32 {
            continue;
        }
        /* q = parse_ident(&p, p + strlen(p)); */
        let q = p;
        while *p as i32 != 0 && libc::isspace(*p as _) == 0 {
            p = p.offset(1)
        }
        *p = '\u{0}' as i32 as i8;
        p = p.offset(1);
        if streq_ptr(q, subfont_id) {
            if num_sfd_records >= max_sfd_records {
                max_sfd_records += 16i32;
                sfd_record = renew(
                    sfd_record as *mut libc::c_void,
                    (max_sfd_records as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<sfd_rec_>() as u64)
                        as u32,
                ) as *mut sfd_rec_
            }
            if !(*sfd_record.offset(num_sfd_records as isize))
                .vector
                .as_mut_ptr()
                .is_null()
            {
                for __i in 0..256 {
                    (*sfd_record.offset(num_sfd_records as isize)).vector[__i as usize] = 0_u16;
                }
            }
            let error = read_sfd_record(&mut *sfd_record.offset(num_sfd_records as isize), p);
            if error != 0 {
                warn!(
                    "Error occured while reading SFD file: file=\"{}\" subfont_id=\"{}\"",
                    CStr::from_ptr((*sfd).ident).display(),
                    CStr::from_ptr(subfont_id).display(),
                );
            } else {
                let fresh3 = num_sfd_records;
                num_sfd_records = num_sfd_records + 1;
                rec_id = fresh3
            }
        }
    }
    if rec_id < 0i32 {
        warn!(
            "Failed to load subfont mapping table for SFD=\"{}\" subfont_id=\"{}\"",
            CStr::from_ptr((*sfd).ident).display(),
            CStr::from_ptr(subfont_id).display(),
        );
    }
    *(*sfd).rec_id.offset(i as isize) = rec_id;
    ttstub_input_close(handle);
    if verbose > 3i32 {
        if rec_id >= 0i32 {
            info!(" at id=\"{}\"", rec_id);
            info!("\nsubfont>> Content of mapping table:");
            for __i_0 in 0..256 {
                if __i_0 % 16i32 == 0i32 {
                    info!("\nsubfont>>  ");
                }
                info!(
                    " {:04x}",
                    (*sfd_record.offset(rec_id as isize)).vector[__i_0 as usize] as i32,
                );
            }
        }
        info!("\n");
    }
    rec_id
}
/* Lookup mapping table */
#[no_mangle]
pub unsafe extern "C" fn lookup_sfd_record(mut rec_id: i32, mut c: u8) -> u16 {
    if sfd_record.is_null() || rec_id < 0i32 || rec_id >= num_sfd_records {
        panic!("Invalid subfont_id: {}", rec_id);
    }
    (*sfd_record.offset(rec_id as isize)).vector[c as usize]
}
#[no_mangle]
pub unsafe extern "C" fn release_sfd_record() {
    if !sfd_record.is_null() {
        free(sfd_record as *mut libc::c_void);
    }
    if !sfd_files.is_null() {
        for i in 0..num_sfd_files {
            clean_sfd_file_(&mut *sfd_files.offset(i as isize));
        }
        free(sfd_files as *mut libc::c_void);
    }
    sfd_record = ptr::null_mut();
    sfd_files = ptr::null_mut();
    max_sfd_records = 0i32;
    num_sfd_records = max_sfd_records;
    max_sfd_files = 0i32;
    num_sfd_files = max_sfd_files;
}
