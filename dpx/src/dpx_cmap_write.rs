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

use crate::warn;
use std::io::Write;

use super::dpx_cid::{CSI_IDENTITY, CSI_UNICODE};
use super::dpx_cmap::{CMap_get_CIDSysInfo, CMap_is_valid};
use super::dpx_mem::new;
use crate::dpx_pdfobj::{pdf_dict, pdf_stream, pdf_string, STREAM_COMPRESS};
use libc::{free, memcmp, memset};

use crate::bridge::size_t;

use super::dpx_cmap::mapDef;
use super::dpx_cmap::CMap;

/*
 * References:
 *
 *  PostScript Language Reference Manual, 3rd. ed. (Adobe Systems Inc.)
 *    5.11.4 CMap Dictionaries
 *    5.11.5 FMapType 9 Composite Fonts
 *  Building CMap Files for CID-Keyed Fonts, Adobe Technical Note #5099
 *  CID-Keyed Font Technology Overview, Adobe Technical Note #5092
 *  Adobe CMap and CIDFont Files Specification, Adobe Technical Specification #5014
 *
 *  Undefined Character Handling:
 *    PLRM 3rd. ed., sec. 5.11.5., "Handling Undefined Characters"
 *
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_1 {
    pub(crate) start: i32,
    pub(crate) count: i32,
}
unsafe fn block_count(mtab: *mut mapDef, mut c: i32) -> size_t {
    let mut count: size_t = 0 as size_t;
    let n = (*mtab.offset(c as isize)).len.wrapping_sub(1);
    c += 1;
    while c < 256 {
        if (*mtab.offset(c as isize)).flag & 1 << 4 != 0
            || (if (*mtab.offset(c as isize)).flag & 0xf != 0 {
                1
            } else {
                0
            }) == 0
            || (*mtab.offset(c as isize)).flag & 0xf != 1 << 0
                && (*mtab.offset(c as isize)).flag & 0xf != 1 << 2
            || (*mtab.offset((c - 1) as isize)).len != (*mtab.offset(c as isize)).len
        {
            break;
        }
        if !(memcmp(
            (*mtab.offset((c - 1) as isize)).code as *const libc::c_void,
            (*mtab.offset(c as isize)).code as *const libc::c_void,
            n as _,
        ) == 0
            && (*(*mtab.offset((c - 1) as isize)).code.offset(n as isize) as i32) < 255
            && *(*mtab.offset((c - 1) as isize)).code.offset(n as isize) as i32 + 1
                == *(*mtab.offset(c as isize)).code.offset(n as isize) as i32)
        {
            break;
        }
        count = count.wrapping_add(1);
        c += 1
    }
    count
}
unsafe fn sputx(c: u8, s: &mut Vec<u8>, lim: usize) {
    let hi: i8 = (c as i32 >> 4) as i8;
    let lo: i8 = (c as i32 & 0xf) as i8;
    if s.len() > lim - 2 {
        panic!("Buffer overflow.");
    }
    s.push(
        (if (hi as i32) < 10 {
            hi as i32 + '0' as i32
        } else {
            hi as i32 + '7' as i32
        }) as u8,
    );
    s.push(
        (if (lo as i32) < 10 {
            lo as i32 + '0' as i32
        } else {
            lo as i32 + '7' as i32
        }) as u8,
    );
}
unsafe fn write_map(
    mtab: *mut mapDef,
    mut count: size_t,
    codestr: *mut u8,
    depth: size_t,
    wbuf: &mut Vec<u8>,
    lim: usize,
    stream: &mut pdf_stream,
) -> i32 {
    /* Must be greater than 1 */
    let mut blocks: [C2RustUnnamed_1; 129] = [C2RustUnnamed_1 { start: 0, count: 0 }; 129];
    let mut num_blocks: size_t = 0 as size_t;
    let mut c = 0;
    while c < 256 as u64 {
        *codestr.offset(depth as isize) = (c & 0xff as u64) as u8;
        if (*mtab.offset(c as isize)).flag & 1 << 4 != 0 {
            let mtab1 = (*mtab.offset(c as isize)).next;
            count = write_map(
                mtab1,
                count,
                codestr,
                depth.wrapping_add(1),
                wbuf,
                lim,
                stream,
            ) as size_t
        } else if if (*mtab.offset(c as isize)).flag & 0xf != 0 {
            1
        } else {
            0
        } != 0
        {
            match (*mtab.offset(c as isize)).flag & 0xf {
                1 | 4 => {
                    let block_length = block_count(mtab, c as i32);
                    if block_length >= 2 {
                        blocks[num_blocks as usize].start = c as i32;
                        blocks[num_blocks as usize].count = block_length as i32;
                        num_blocks = num_blocks.wrapping_add(1);
                        c = (c as u64).wrapping_add(block_length as _) as _
                    } else {
                        wbuf.push(b'<');
                        for i in 0..=depth {
                            sputx(*codestr.offset(i as isize), wbuf, lim);
                        }
                        wbuf.push(b'>');
                        wbuf.push(b' ');
                        wbuf.push(b'<');
                        for i in 0..(*mtab.offset(c as isize)).len {
                            sputx(
                                *(*mtab.offset(c as isize)).code.offset(i as isize),
                                wbuf,
                                lim,
                            );
                        }
                        wbuf.push(b'>');
                        wbuf.push(b'\n');
                        count = count.wrapping_add(1)
                    }
                }
                2 => {
                    panic!("{}: Unexpected error...", "CMap");
                }
                8 => {}
                _ => {
                    panic!(
                        "{}: Unknown mapping type: {}",
                        "CMap",
                        (*mtab.offset(c as isize)).flag & 0xf,
                    );
                }
            }
        }
        /* Flush if necessary */
        if count >= 100 || wbuf.len() >= lim {
            if count > 100 {
                panic!("Unexpected error....: {}", count);
            }
            stream.add_str(&format!("{} beginbfchar\n", count));
            stream.add_slice(wbuf.as_slice());
            wbuf.clear();
            stream.add_str("endbfchar\n");
            count = 0 as size_t
        }
        c = c.wrapping_add(1)
    }
    if num_blocks > 0 {
        if count > 0 {
            stream.add_str(&format!("{} beginbfchar\n", count));
            stream.add_slice(wbuf.as_slice());
            wbuf.clear();
            stream.add_str("endbfchar\n");
            count = 0 as size_t
        }
        stream.add_str(&format!("{} beginbfrange\n", num_blocks));
        for i in 0..num_blocks {
            let c = blocks[i as usize].start as size_t;
            wbuf.push(b'<');
            for j in 0..depth {
                sputx(*codestr.offset(j as isize), wbuf, lim);
            }
            sputx(c as u8, wbuf, lim);
            wbuf.push(b'>');
            wbuf.push(b' ');
            wbuf.push(b'<');
            for j in 0..depth {
                sputx(*codestr.offset(j as isize), wbuf, lim);
            }
            sputx(
                c.wrapping_add(blocks[i as usize].count as _) as u8,
                wbuf,
                lim,
            );
            wbuf.push(b'>');
            wbuf.push(b' ');
            wbuf.push(b'<');
            for j in 0..(*mtab.offset(c as isize)).len {
                sputx(
                    *(*mtab.offset(c as isize)).code.offset(j as isize),
                    wbuf,
                    lim,
                );
            }
            wbuf.push(b'>');
            wbuf.push(b'\n');
        }
        stream.add_slice(wbuf.as_slice());
        wbuf.clear();
        stream.add_str("endbfrange\n");
    }
    count as i32
}

pub(crate) unsafe fn CMap_create_stream(cmap: *mut CMap) -> Option<pdf_stream> {
    if cmap.is_null() || !CMap_is_valid(cmap) {
        warn!("Invalid CMap");
        return None;
    }
    if (*cmap).type_0 == 0 {
        return None;
    }
    let mut stream = pdf_stream::new(STREAM_COMPRESS);
    let stream_dict = stream.get_dict_mut();
    let mut csi = CMap_get_CIDSysInfo(cmap);
    if csi.is_null() {
        csi = if (*cmap).type_0 != 2 {
            &mut CSI_IDENTITY
        } else {
            &mut CSI_UNICODE
        }
    }
    if (*cmap).type_0 != 2 {
        let mut csi_dict = pdf_dict::new();
        csi_dict.set("Registry", pdf_string::new((*csi).registry.as_bytes()));
        csi_dict.set("Ordering", pdf_string::new((*csi).ordering.as_bytes()));
        csi_dict.set("Supplement", (*csi).supplement as f64);
        stream_dict.set("Type", "CMap");
        stream_dict.set("CMapName", (*cmap).name.as_str());
        stream_dict.set("CIDSystemInfo", csi_dict);
        if (*cmap).wmode != 0 {
            stream_dict.set("WMode", (*cmap).wmode as f64);
        }
    }
    /* TODO:
     * Predefined CMaps need not to be embedded.
     */
    if !(*cmap).useCMap.is_null() {
        panic!("UseCMap found (not supported yet)...");
    }
    let mut wbuf = Vec::<u8>::with_capacity(4096);
    let codestr = new(((*cmap).profile.maxBytesIn as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32) as *mut u8;
    memset(
        codestr as *mut libc::c_void,
        0,
        (*cmap).profile.maxBytesIn as _,
    );
    let lim = 4096
        - ((2_u64).wrapping_mul(
            (*cmap)
                .profile
                .maxBytesIn
                .wrapping_add((*cmap).profile.maxBytesOut) as _,
        ) as usize)
        + 16;
    /* Start CMap */
    stream.add_str("/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n");
    writeln!(wbuf, "/CMapName /{} def", (*cmap).name).unwrap();
    writeln!(wbuf, "/CMapType {} def", (*cmap).type_0).unwrap();

    if (*cmap).wmode != 0 && (*cmap).type_0 != 2 {
        writeln!(wbuf, "/WMode {} def", (*cmap).wmode).unwrap();
    }
    writeln!(
        wbuf,
        "/CIDSystemInfo <<\n  /Registry ({})\n  /Ordering ({})\n  /Supplement {}\n>> def",
        (*csi).registry,
        (*csi).ordering,
        (*csi).supplement,
    )
    .unwrap();

    stream.add_slice(wbuf.as_slice());
    wbuf.clear();
    /* codespacerange */
    let ranges = (*cmap).codespace.ranges;
    writeln!(wbuf, "{} begincodespacerange", (*cmap).codespace.num).unwrap();
    for i in 0..(*cmap).codespace.num as u64 {
        wbuf.push(b'<');
        for j in 0..(*ranges.offset(i as isize)).dim {
            sputx(
                *(*ranges.offset(i as isize)).codeLo.offset(j as isize),
                &mut wbuf,
                lim,
            );
        }
        wbuf.push(b'>');
        wbuf.push(b' ');
        wbuf.push(b'<');
        for j in 0..(*ranges.offset(i as isize)).dim {
            sputx(
                *(*ranges.offset(i as isize)).codeHi.offset(j as isize),
                &mut wbuf,
                lim,
            );
        }
        wbuf.push(b'>');
        wbuf.push(b'\n');
    }
    stream.add_slice(wbuf.as_slice());
    wbuf.clear();
    stream.add_str("endcodespacerange\n");
    /* CMap body */
    if !(*cmap).mapTbl.is_null() {
        let count = write_map((*cmap).mapTbl, 0, codestr, 0, &mut wbuf, lim, &mut stream) as size_t; /* Top node */
        if count > 0 {
            /* Flush */
            if count > 100 {
                panic!("Unexpected error....: {}", count);
            }
            stream.add_str(&format!("{} beginbfchar\n", count));
            stream.add_slice(wbuf.as_slice());
            stream.add_str("endbfchar\n");
            wbuf.clear();
        }
    }
    /* End CMap */
    stream.add_str("endcmap\nCMapName currentdict /CMap defineresource pop\nend\nend\n");
    free(codestr as *mut libc::c_void);
    Some(stream)
}
