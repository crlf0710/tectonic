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

use tectonic_bridge::ttstub_input_close;

use super::dpx_mem::{new, renew};
use super::dpx_numbers::{tt_get_unsigned_pair, tt_get_unsigned_quad};
use crate::dpx_pdfobj::{
    pdf_add_dict, pdf_add_stream, pdf_new_number, pdf_new_stream, pdf_obj, pdf_release_obj,
    STREAM_COMPRESS,
};
use crate::dpx_truetype::SfntTableInfo;
use crate::mfree;
use crate::{ttstub_input_read_exact};
use libc::{free, memcpy};

use std::io::{Seek, SeekFrom};

pub type __ssize_t = i64;
pub type size_t = u64;
use bridge::InputHandleWrapper;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sfnt_table {
    pub tag: [u8; 4],
    pub check_sum: u32,
    pub offset: u32,
    pub length: u32,
    pub data: *mut i8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct sfnt_table_directory {
    pub version: u32,
    pub num_tables: u16,
    pub search_range: u16,
    pub entry_selector: u16,
    pub range_shift: u16,
    pub num_kept_tables: u16,
    pub flags: *mut i8,
    pub tables: *mut sfnt_table,
}
#[repr(C)]
pub struct sfnt {
    pub type_0: i32,
    pub directory: *mut sfnt_table_directory,
    pub handle: InputHandleWrapper,
    pub offset: u32,
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_open(mut handle: InputHandleWrapper) -> *mut sfnt {
    handle.seek(SeekFrom::Start(0)).unwrap(); /* mbz */
    let sfont =
        new((1_u32 as u64).wrapping_mul(::std::mem::size_of::<sfnt>() as u64) as u32) as *mut sfnt; /* typefaces position */
    let type_0 = tt_get_unsigned_quad(&mut handle); /* resource id */
    if type_0 as u64 == 0x10000 || type_0 as u64 == 0x74727565 {
        (*sfont).type_0 = 1i32 << 0i32
    } else if type_0 as u64 == 0x10000 {
        (*sfont).type_0 = 1i32 << 1i32
    } else if type_0 as u64 == 0x4f54544f {
        (*sfont).type_0 = 1i32 << 2i32
    } else if type_0 as u64 == 0x74746366 {
        (*sfont).type_0 = 1i32 << 4i32
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    (*sfont).handle = handle;
    (*sfont).directory = 0 as *mut sfnt_table_directory;
    (*sfont).offset = 0u64 as u32;
    sfont
}
#[no_mangle]
pub unsafe extern "C" fn dfont_open(mut handle: InputHandleWrapper, mut index: i32) -> *mut sfnt {
    let mut types_pos: u32 = 0;
    let mut res_pos: u32 = 0;
    let mut types_num: u16 = 0;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let sfont =
        new((1_u32 as u64).wrapping_mul(::std::mem::size_of::<sfnt>() as u64) as u32) as *mut sfnt;
    let rdata_pos = tt_get_unsigned_quad(&mut handle);
    let map_pos = tt_get_unsigned_quad(&mut handle);
    handle.seek(SeekFrom::Start((map_pos + 0x18) as u64)).unwrap();
    let tags_pos = map_pos.wrapping_add(tt_get_unsigned_pair(&mut handle) as u32);
    handle.seek(SeekFrom::Start(tags_pos as u64)).unwrap();
    let tags_num = tt_get_unsigned_pair(&mut handle);
    let mut i = 0;
    while i as i32 <= tags_num as i32 {
        let tag = tt_get_unsigned_quad(&mut handle);
        types_num = tt_get_unsigned_pair(&mut handle);
        types_pos = tags_pos.wrapping_add(tt_get_unsigned_pair(&mut handle) as u32);
        if tag as u64 == 0x73666e74 {
            break;
        }
        i += 1;
    }
    if i as i32 > tags_num as i32 {
        free(sfont as *mut libc::c_void);
        ttstub_input_close(handle);
        return 0 as *mut sfnt;
    }
    handle.seek(SeekFrom::Start(types_pos as u64)).unwrap();
    if index > types_num as i32 {
        panic!("Invalid index {} for dfont.", index);
    }
    for i in 0..=types_num as i32 {
        tt_get_unsigned_pair(&mut handle);
        tt_get_unsigned_pair(&mut handle);
        res_pos = tt_get_unsigned_quad(&mut handle);
        tt_get_unsigned_quad(&mut handle);
        if i as i32 == index {
            break;
        }
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    (*sfont).handle = handle;
    (*sfont).type_0 = 1i32 << 8i32;
    (*sfont).directory = 0 as *mut sfnt_table_directory;
    (*sfont).offset = (res_pos as u64 & 0xffffff)
        .wrapping_add(rdata_pos as u64)
        .wrapping_add(4i32 as u64) as u32;
    sfont
}
unsafe fn release_directory(mut td: *mut sfnt_table_directory) {
    if !td.is_null() {
        if !(*td).tables.is_null() {
            for i in 0..(*td).num_tables as u32 {
                free((*(*td).tables.offset(i as isize)).data as *mut libc::c_void);
            }
            free((*td).tables as *mut libc::c_void);
        }
        free((*td).flags as *mut libc::c_void);
        free(td as *mut libc::c_void);
    };
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_close(mut sfont: *mut sfnt) {
    if !sfont.is_null() {
        ttstub_input_close((*sfont).handle.clone()); // TODO: use drop
        if !(*sfont).directory.is_null() {
            release_directory((*sfont).directory);
        }
        free(sfont as *mut libc::c_void);
    };
}
#[no_mangle]
pub unsafe extern "C" fn put_big_endian(mut s: *mut libc::c_void, mut q: i32, mut n: i32) -> i32 {
    let p = s as *mut i8;
    for i in (0..n).rev() {
        *p.offset(i as isize) = (q & 0xffi32) as i8;
        q >>= 8i32;
    }
    n
}

/* Convert four-byte number to big endianess
 * in a machine independent way.
 */
fn convert_tag(tag: &mut [u8; 4], u_tag: u32) {
    *tag = u_tag.to_be_bytes();
}

/*
 * Computes the max power of 2 <= n
 */
unsafe fn max2floor(mut n: u32) -> u32 {
    let mut val: i32 = 1i32;
    while n > 1_u32 {
        n = n.wrapping_div(2_u32);
        val *= 2i32
    }
    val as u32
}
/*
 * Computes the log2 of the max power of 2 <= n
 */
unsafe fn log2floor(mut n: u32) -> u32 {
    let mut val: u32 = 0_u32;
    while n > 1_u32 {
        n = n.wrapping_div(2_u32);
        val = val.wrapping_add(1)
    }
    val
}
unsafe fn sfnt_calc_checksum(mut data: *mut libc::c_void, mut length: u32) -> u32 {
    let mut chksum: u32 = 0_u32;
    let mut count: i32 = 0i32;
    let mut p = data as *mut u8;
    let endptr = p.offset(length as isize);
    while p < endptr {
        chksum = (chksum as u32)
            .wrapping_add(((*p.offset(0) as i32) << 8i32 * (3i32 - count)) as u32)
            as u32 as u32;
        count = count + 1i32 & 3i32;
        p = p.offset(1)
    }
    chksum
}

unsafe fn find_table_index(td: Option<&sfnt_table_directory>, tag: &[u8; 4]) -> i32 {
    td.and_then(|td| (0..td.num_tables).find(|&idx| tag == &(*td.tables.offset(idx as isize)).tag))
        .map(|idx| i32::from(idx))
        .unwrap_or(-1)
}

#[no_mangle]
pub unsafe extern "C" fn sfnt_set_table(
    mut sfont: *mut sfnt,
    mut tag: &[u8; 4],
    mut data: *mut libc::c_void,
    mut length: u32,
) {
    assert!(!sfont.is_null());
    let td = (*sfont).directory;
    let mut idx = find_table_index(td.as_ref(), tag);
    if idx < 0i32 {
        idx = (*td).num_tables as i32;
        (*td).num_tables = (*td).num_tables.wrapping_add(1);
        (*td).tables = renew(
            (*td).tables as *mut libc::c_void,
            ((*td).num_tables as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<sfnt_table>() as u64) as u32,
        ) as *mut sfnt_table;
        (*(*td).tables.offset(idx as isize)).tag = tag.clone();
    }
    (*(*td).tables.offset(idx as isize)).check_sum = sfnt_calc_checksum(data, length);
    (*(*td).tables.offset(idx as isize)).offset = 0i64 as u32;
    (*(*td).tables.offset(idx as isize)).length = length;
    let ref mut fresh0 = (*(*td).tables.offset(idx as isize)).data;
    *fresh0 = data as *mut i8;
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_find_table_len(mut sfont: *mut sfnt, tag: &[u8; 4]) -> u32 {
    assert!(!sfont.is_null());
    let td = (*sfont).directory;
    let idx = find_table_index(td.as_ref(), tag);
    if idx < 0i32 {
        0
    } else {
        (*(*td).tables.offset(idx as isize)).length
    }
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_find_table_pos(mut sfont: *mut sfnt, tag: &[u8; 4]) -> u32 {
    assert!(!sfont.is_null());
    let td = (*sfont).directory;
    let idx = find_table_index(td.as_ref(), tag);
    if idx < 0i32 {
        0
    } else {
        (*(*td).tables.offset(idx as isize)).offset
    }
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_locate_table(mut sfont: *mut sfnt, tag: &[u8; 4]) -> u32 {
    assert!(!sfont.is_null());
    let offset = sfnt_find_table_pos(sfont, tag);
    if offset == 0_u32 {
        panic!("sfnt: table not found...");
    }
    (*sfont).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    offset
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_read_table_directory(mut sfont: *mut sfnt, mut offset: u32) -> i32 {
    assert!(!sfont.is_null());
    if !(*sfont).directory.is_null() {
        release_directory((*sfont).directory);
    }
    let td = new(
        (1_u32 as u64).wrapping_mul(::std::mem::size_of::<sfnt_table_directory>() as u64) as u32,
    ) as *mut sfnt_table_directory;
    let handle = &mut (*sfont).handle;
    (*sfont).directory = td;
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    (*td).version = tt_get_unsigned_quad(handle);
    (*td).num_tables = tt_get_unsigned_pair(handle);
    (*td).search_range = tt_get_unsigned_pair(handle);
    (*td).entry_selector = tt_get_unsigned_pair(handle);
    (*td).range_shift = tt_get_unsigned_pair(handle);
    (*td).flags = new(
        ((*td).num_tables as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32
    ) as *mut i8;
    (*td).tables = new(((*td).num_tables as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<sfnt_table>() as u64) as u32)
        as *mut sfnt_table;
    for i in 0..(*td).num_tables as u32 {
        let u_tag = tt_get_unsigned_quad(handle);
        convert_tag(&mut (*(*td).tables.offset(i as isize)).tag, u_tag);
        (*(*td).tables.offset(i as isize)).check_sum = tt_get_unsigned_quad(handle);
        (*(*td).tables.offset(i as isize)).offset =
            tt_get_unsigned_quad(handle).wrapping_add((*sfont).offset);
        (*(*td).tables.offset(i as isize)).length = tt_get_unsigned_quad(handle);
        let ref mut fresh1 = (*(*td).tables.offset(i as isize)).data;
        *fresh1 = 0 as *mut i8;
        //fprintf(stderr, "[%4s:%x]", td->tables[i].tag, td->tables[i].offset);
        *(*td).flags.offset(i as isize) = 0_i8;
    }
    (*td).num_kept_tables = 0_u16;
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn sfnt_require_table(
    sfont: &mut sfnt,
    table: &SfntTableInfo,
) -> Result<(), ()> {
    let mut td = (*sfont).directory.as_mut().unwrap();
    let idx = find_table_index(Some(td), table.name());
    if idx < 0 {
        if table.must_exist() {
            return Err(());
        }
    } else {
        let ref mut fresh2 = *td.flags.offset(idx as isize);
        *fresh2 = (*fresh2 as i32 | 1i32 << 0i32) as i8;
        td.num_kept_tables = td.num_kept_tables + 1;
    }
    Ok(())
}
/*
 * o All tables begin on four byte boundries, and pad any remaining space
 *   between tables with zeros
 *
 * o Entries in the Table Directory must be sorted in ascending order by tag
 *
 * o The head table contains checksum of the whole font file.
 *   To compute:  first set it to 0, sum the entire font as ULONG,
 *   then store 0xB1B0AFBA - sum.
 */
static mut wbuf: [u8; 1024] = [0; 1024];
static mut padbytes: [u8; 4] = [0; 4];
/* Acoid conflict with CHAR ... from <winnt.h>.  */
/* Data Types as described in Apple's TTRefMan */
/* 16.16-bit signed fixed-point number */
/* table header */
/* table data */
/* Fixed for Win */
/* number of kept tables */
/* keep or omit */
/* sfnt resource */
/* Convert sfnt "fixed" type to double */
/* get_***_*** from numbers.h */
/* table directory */
#[no_mangle]
pub unsafe extern "C" fn sfnt_create_FontFile_stream(mut sfont: *mut sfnt) -> *mut pdf_obj {
    let mut length;
    assert!(!sfont.is_null() && !(*sfont).directory.is_null());
    let stream = pdf_new_stream(STREAM_COMPRESS);
    let td = (*sfont).directory;
    /* Header */
    let mut p = wbuf.as_mut_ptr() as *mut i8;
    p = p.offset(put_big_endian(p as *mut libc::c_void, (*td).version as i32, 4i32) as isize);
    p = p.offset(
        put_big_endian(p as *mut libc::c_void, (*td).num_kept_tables as i32, 2i32) as isize,
    );
    let mut sr = max2floor((*td).num_kept_tables as u32).wrapping_mul(16_u32) as i32;
    p = p.offset(put_big_endian(p as *mut libc::c_void, sr, 2i32) as isize);
    p = p.offset(put_big_endian(
        p as *mut libc::c_void,
        log2floor((*td).num_kept_tables as u32) as i32,
        2i32,
    ) as isize);
    put_big_endian(
        p as *mut libc::c_void,
        (*td).num_kept_tables as i32 * 16i32 - sr,
        2i32,
    );
    pdf_add_stream(&mut *stream, wbuf.as_mut_ptr() as *const libc::c_void, 12i32);
    /*
     * Compute start of actual tables (after headers).
     */
    let mut offset = 12 + 16 * (*td).num_kept_tables as i32;
    for i in 0..(*td).num_tables as i32 {
        /* This table must exist in FontFile */
        if *(*td).flags.offset(i as isize) as i32 & 1i32 << 0i32 != 0 {
            if offset % 4i32 != 0i32 {
                offset += 4i32 - offset % 4i32
            }
            p = wbuf.as_mut_ptr() as *mut i8;
            memcpy(
                p as *mut libc::c_void,
                (*(*td).tables.offset(i as isize)).tag.as_mut_ptr() as *const libc::c_void,
                4,
            );
            p = p.offset(4);
            p = p.offset(put_big_endian(
                p as *mut libc::c_void,
                (*(*td).tables.offset(i as isize)).check_sum as i32,
                4i32,
            ) as isize);
            p = p.offset(put_big_endian(p as *mut libc::c_void, offset, 4i32) as isize);
            put_big_endian(
                p as *mut libc::c_void,
                (*(*td).tables.offset(i as isize)).length as i32,
                4i32,
            );
            pdf_add_stream(&mut *stream, wbuf.as_mut_ptr() as *const libc::c_void, 16i32);
            offset = (offset as u32).wrapping_add((*(*td).tables.offset(i as isize)).length) as i32
                as i32
        }
    }
    let mut offset = 12 + 16 * (*td).num_kept_tables as i32;
    for i in 0..(*td).num_tables as i32 {
        if *(*td).flags.offset(i as isize) as i32 & 1i32 << 0i32 != 0 {
            if offset % 4i32 != 0i32 {
                length = 4i32 - offset % 4i32;
                pdf_add_stream(&mut *stream, padbytes.as_mut_ptr() as *const libc::c_void, length);
                offset += length
            }
            if (*(*td).tables.offset(i as isize)).data.is_null() {
                /*if (*sfont).handle.is_null() {
                    pdf_release_obj(stream);
                    panic!("Font file not opened or already closed...");
                }*/
                length = (*(*td).tables.offset(i as isize)).length as i32;
                (*sfont).handle.seek(SeekFrom::Start((*(*td).tables.offset(i as isize)).offset as u64)).unwrap();
                while length > 0i32 {
                    let nb_read = ttstub_input_read_exact(
                        (*sfont).handle.0.as_ptr(),
                        wbuf.as_mut_ptr() as *mut i8,
                        (if length < 1024i32 { length } else { 1024i32 }) as size_t,
                    ) as i32;
                    if nb_read < 0i32 {
                        pdf_release_obj(stream);
                        panic!("Reading file failed...");
                    } else {
                        if nb_read > 0i32 {
                            pdf_add_stream(
                                &mut *stream,
                                wbuf.as_mut_ptr() as *const libc::c_void,
                                nb_read,
                            );
                        }
                    }
                    length -= nb_read
                }
            } else {
                pdf_add_stream(
                    &mut *stream,
                    (*(*td).tables.offset(i as isize)).data as *const libc::c_void,
                    (*(*td).tables.offset(i as isize)).length as i32,
                );
                let ref mut fresh3 = (*(*td).tables.offset(i as isize)).data;
                *fresh3 =
                    mfree((*(*td).tables.offset(i as isize)).data as *mut libc::c_void) as *mut i8
            }
            /* Set offset for next table */
            offset = (offset as u32).wrapping_add((*(*td).tables.offset(i as isize)).length) as i32
                as i32
        }
    }
    let stream_dict = (*stream).as_stream_mut().get_dict_mut();
    pdf_add_dict(stream_dict, "Length1", pdf_new_number(offset as f64));
    stream
}
