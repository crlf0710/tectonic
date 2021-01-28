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
    non_upper_case_globals
)]

use super::dpx_numbers::GetFromFile;
use crate::dpx_pdfobj::{pdf_stream, STREAM_COMPRESS};
use crate::dpx_truetype::SfntTableInfo;
use std::rc::Rc;

use std::io::{Read, Seek, SeekFrom};

pub(crate) trait PutBE<T> {
    fn put_be(&mut self, data: T);
}

impl PutBE<u32> for Vec<u8> {
    fn put_be(&mut self, data: u32) {
        self.extend(&data.to_be_bytes())
    }
}

impl PutBE<u16> for Vec<u8> {
    fn put_be(&mut self, data: u16) {
        self.extend(&data.to_be_bytes())
    }
}

impl PutBE<i16> for Vec<u8> {
    fn put_be(&mut self, data: i16) {
        self.extend(&data.to_be_bytes())
    }
}

use bridge::InFile;
#[derive(Clone)]
pub(crate) struct sfnt_table {
    pub(crate) tag: [u8; 4],
    pub(crate) check_sum: u32,
    pub(crate) offset: u32,
    pub(crate) length: u32,
    pub(crate) data: Vec<u8>,
}
#[derive(Clone)]
pub(crate) struct sfnt_table_directory {
    pub(crate) version: u32,
    pub(crate) search_range: u16,
    pub(crate) entry_selector: u16,
    pub(crate) range_shift: u16,
    pub(crate) num_kept_tables: u16,
    pub(crate) flags: Vec<i8>,
    pub(crate) tables: Vec<sfnt_table>,
}
#[derive(Clone)]
pub(crate) struct sfnt {
    pub(crate) type_0: i32,
    pub(crate) directory: Option<Box<sfnt_table_directory>>,
    pub(crate) handle: Rc<InFile>,
    pub(crate) offset: u32,
}

pub(crate) fn sfnt_open(mut handle: InFile) -> sfnt {
    handle.seek(SeekFrom::Start(0)).unwrap(); /* mbz */
    /* typefaces position */
    let typ = u32::get(&mut handle); /* resource id */
    let typ = if typ as u64 == 0x10000 || typ as u64 == 0x74727565 {
        1 << 0
    } else if typ as u64 == 0x10000 {
        1 << 1
    } else if typ as u64 == 0x4f54544f {
        1 << 2
    } else if typ as u64 == 0x74746366 {
        1 << 4
    } else {
        typ
    } as i32;
    handle.seek(SeekFrom::Start(0)).unwrap();
    sfnt {
        type_0: typ,
        handle: Rc::new(handle),
        directory: None,
        offset: 0,
    }
}

pub(crate) fn dfont_open(mut handle: InFile, index: i32) -> Option<sfnt> {
    let mut types_pos: u32 = 0;
    let mut res_pos: u32 = 0;
    let mut types_num: u16 = 0;
    handle.seek(SeekFrom::Start(0)).unwrap();
    let rdata_pos = u32::get(&mut handle);
    let map_pos = u32::get(&mut handle);
    handle
        .seek(SeekFrom::Start((map_pos + 0x18) as u64))
        .unwrap();
    let tags_pos = map_pos.wrapping_add(u16::get(&mut handle) as u32);
    handle.seek(SeekFrom::Start(tags_pos as u64)).unwrap();
    let tags_num = u16::get(&mut handle);
    let mut i = 0;
    while i as i32 <= tags_num as i32 {
        let tag = u32::get(&mut handle);
        types_num = u16::get(&mut handle);
        types_pos = tags_pos.wrapping_add(u16::get(&mut handle) as u32);
        if tag as u64 == 0x73666e74 {
            break;
        }
        i += 1;
    }
    if i as i32 > tags_num as i32 {
        return None;
    }
    handle.seek(SeekFrom::Start(types_pos as u64)).unwrap();
    if index > types_num as i32 {
        panic!("Invalid index {} for dfont.", index);
    }
    for i in 0..=types_num as i32 {
        u16::get(&mut handle);
        u16::get(&mut handle);
        res_pos = u32::get(&mut handle);
        u32::get(&mut handle);
        if i as i32 == index {
            break;
        }
    }
    handle.seek(SeekFrom::Start(0)).unwrap();
    Some(sfnt {
        handle: Rc::new(handle),
        type_0: 1 << 8,
        directory: None,
        offset: ((res_pos as u64 & 0xffffff) + (rdata_pos as u64) + 4) as u32,
    })
}

/*
 * Computes the max power of 2 <= n
 */
fn max2floor(mut n: u32) -> u32 {
    let mut val: i32 = 1;
    while n > 1_u32 {
        n = n.wrapping_div(2_u32);
        val *= 2
    }
    val as u32
}
/*
 * Computes the log2 of the max power of 2 <= n
 */
fn log2floor(mut n: u32) -> u32 {
    let mut val: u32 = 0_u32;
    while n > 1_u32 {
        n = n.wrapping_div(2_u32);
        val = val.wrapping_add(1)
    }
    val
}
fn sfnt_calc_checksum(data: &[u8]) -> u32 {
    let mut chksum: u32 = 0_u32;
    let mut count: i32 = 0;
    for b in data {
        chksum =
            (chksum as u32).wrapping_add(((*b as i32) << 8 * (3 - count)) as u32) as u32 as u32;
        count = count + 1 & 3;
    }
    chksum
}

fn find_table_index(td: Option<&sfnt_table_directory>, tag: &[u8; 4]) -> i32 {
    td.and_then(|td| (0..td.tables.len()).find(|&idx| tag == &td.tables[idx].tag))
        .map(|idx| idx as i32)
        .unwrap_or(-1)
}

pub(crate) fn sfnt_set_table(sfont: &mut sfnt, tag: &[u8; 4], data: Vec<u8>) {
    let td = sfont.directory.as_mut().unwrap();
    let table = sfnt_table {
        tag: *tag,
        check_sum: sfnt_calc_checksum(&data),
        offset: 0,
        length: data.len() as u32,
        data,
    };
    let idx = find_table_index(Some(td), tag);
    if idx < 0 {
        td.tables.push(table);
    } else {
        td.tables[idx as usize] = table;
    }
}

pub(crate) fn sfnt_find_table_len(sfont: &sfnt, tag: &[u8; 4]) -> u32 {
    let idx = find_table_index(sfont.directory.as_deref(), tag);
    if idx < 0 {
        0
    } else {
        sfont.directory.as_ref().unwrap().tables[idx as usize].length
    }
}

pub(crate) fn sfnt_find_table_pos(sfont: &sfnt, tag: &[u8; 4]) -> u32 {
    let idx = find_table_index(sfont.directory.as_deref(), tag);
    if idx < 0 {
        0
    } else {
        sfont.directory.as_ref().unwrap().tables[idx as usize].offset
    }
}

pub(crate) fn sfnt_locate_table(sfont: &sfnt, tag: &[u8; 4]) -> u32 {
    let offset = sfnt_find_table_pos(sfont, tag);
    if offset == 0_u32 {
        panic!("sfnt: table not found...");
    }
    sfont
        .handle
        .as_ref()
        .seek(SeekFrom::Start(offset as u64))
        .unwrap();
    offset
}

pub(crate) fn sfnt_read_table_directory(sfont: &mut sfnt, offset: u32) -> i32 {
    let handle = &mut &*sfont.handle;
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    let version = u32::get(handle);
    let num_tables = u16::get(handle);
    let search_range = u16::get(handle);
    let entry_selector = u16::get(handle);
    let range_shift = u16::get(handle);
    let mut flags = Vec::new();
    let mut tables = Vec::new();
    for _ in 0..num_tables {
        let tag = u32::get(handle).to_be_bytes();
        let check_sum = u32::get(handle);
        let offset = u32::get(handle).wrapping_add(sfont.offset);
        let length = u32::get(handle);
        tables.push(sfnt_table {
            tag,
            check_sum,
            offset,
            length,
            data: Vec::new(),
        });
        //fprintf(stderr, "[%4s:%x]", td->tables[t].tag, td->tables[t].offset);
        flags.push(0_i8);
    }
    let num_kept_tables = 0_u16;

    sfont.directory = Some(Box::new(sfnt_table_directory {
        version,
        search_range,
        entry_selector,
        range_shift,
        num_kept_tables,
        flags,
        tables,
    }));

    0
}

pub(crate) fn sfnt_require_table(sfont: &mut sfnt, table: &SfntTableInfo) -> Result<(), ()> {
    let idx = find_table_index(sfont.directory.as_deref(), table.name());
    if idx < 0 {
        if table.must_exist() {
            return Err(());
        }
    } else {
        let mut td = sfont.directory.as_mut().unwrap();
        td.flags[idx as usize] |= 1 << 0;
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

pub(crate) unsafe fn sfnt_create_FontFile_stream(sfont: &mut sfnt) -> pdf_stream {
    assert!(sfont.directory.is_some());
    let mut stream = pdf_stream::new(STREAM_COMPRESS);
    let td = sfont.directory.as_deref_mut().unwrap();
    /* Header */
    let mut wbuf = Vec::<u8>::with_capacity(1024);
    wbuf.put_be(td.version);
    wbuf.put_be(td.num_kept_tables);
    let sr = max2floor(td.num_kept_tables as u32).wrapping_mul(16_u32) as i32;
    wbuf.put_be(sr as u16);
    wbuf.put_be(log2floor(td.num_kept_tables as u32) as u16);
    wbuf.put_be((td.num_kept_tables as i32 * 16 - sr) as u16);
    stream.add_slice(&wbuf[..]);
    wbuf.clear();
    /*
     * Compute start of actual tables (after headers).
     */
    let mut offset = 12 + 16 * td.num_kept_tables as i32;
    for i in 0..td.tables.len() {
        /* This table must exist in FontFile */
        if td.flags[i] as i32 & 1 << 0 != 0 {
            if offset % 4 != 0 {
                offset += 4 - offset % 4
            }
            let table = &td.tables[i];
            wbuf.extend(&table.tag);
            wbuf.put_be(table.check_sum);
            wbuf.put_be(offset as u32);
            wbuf.put_be(table.length as u32);

            stream.add_slice(&wbuf[..]);
            wbuf.clear();
            offset = (offset as u32).wrapping_add(table.length) as i32 as i32
        }
    }
    let mut offset = 12 + 16 * td.num_kept_tables as i32;
    for i in 0..td.tables.len() {
        if td.flags[i] as i32 & 1 << 0 != 0 {
            if offset % 4 != 0 {
                let length = 4 - offset % 4;
                stream.add_slice(&padbytes[..length as usize]);
                offset += length
            }
            if td.tables[i].data.is_empty() {
                /*if sfont.handle.is_null() {
                    panic!("Font file not opened or already closed...");
                }*/
                let mut length = td.tables[i].length as i32;
                sfont
                    .handle
                    .as_ref()
                    .seek(SeekFrom::Start(td.tables[i].offset as u64))
                    .unwrap();
                while length > 0 {
                    wbuf.resize(length.min(1024) as usize, 0);
                    let nb_read = sfont
                        .handle
                        .as_ref()
                        .read(&mut wbuf[..])
                        .expect("Reading file failed...") as i32;
                    if nb_read > 0 {
                        stream.add_slice(&wbuf[..nb_read as usize]);
                    }
                    wbuf.clear();
                    length -= nb_read
                }
            } else {
                stream.add_slice(td.tables[i].data.as_slice());
                td.tables[i].data = Vec::new();
            }
            /* Set offset for next table */
            offset += td.tables[i].length as i32;
        }
    }
    stream.get_dict_mut().set("Length1", offset as f64);
    stream
}
