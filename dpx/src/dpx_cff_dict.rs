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

use crate::DisplayExt;
use std::ffi::CStr;

use super::dpx_cff::{cff_add_string, cff_get_string};
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer;
use crate::mfree;
use crate::shims::sprintf;
use crate::streq_ptr;
use crate::stub_errno as errno;
use crate::warn;
use libc::{free, memset, strcmp, strtod};

pub type s_SID = u16;
/* CFF Data Types */
/* SID SID number */
/* offset(0) */
/* size offset(0) */
/* 1-byte unsigned number */
/* 2-byte unsigned number */
/* 1-byte unsigned number specifies the size
of an Offset field or fields, range 1-4 */
/* 1, 2, 3, or 4-byte offset */
/* 2-byte string identifier  */
use super::dpx_cff::cff_dict;
use super::dpx_cff::cff_dict_entry;
/* CID-Keyed font specific */
use super::dpx_cff::cff_font;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_2 {
    pub opname: *const i8,
    pub argtype: i32,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
#[no_mangle]
pub unsafe extern "C" fn cff_new_dict() -> *mut cff_dict {
    let dict =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cff_dict>() as u64) as u32) as *mut cff_dict;
    (*dict).max = 16i32;
    (*dict).count = 0i32;
    (*dict).entries = new(((*dict).max as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<cff_dict_entry>() as u64)
        as u32) as *mut cff_dict_entry;
    dict
}
#[no_mangle]
pub unsafe extern "C" fn cff_release_dict(mut dict: *mut cff_dict) {
    if !dict.is_null() {
        if !(*dict).entries.is_null() {
            for i in 0..(*dict).count {
                free((*(*dict).entries.offset(i as isize)).values as *mut libc::c_void);
            }
            free((*dict).entries as *mut libc::c_void);
        }
        free(dict as *mut libc::c_void);
    };
}
static mut stack_top: i32 = 0i32;
static mut arg_stack: [f64; 64] = [0.; 64];
static mut dict_operator: [C2RustUnnamed_2; 61] = [
    C2RustUnnamed_2 {
        opname: b"version\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"Notice\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"FullName\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"FamilyName\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"Weight\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"FontBBox\x00" as *const u8 as *const i8,
        argtype: 1i32 << 4i32,
    },
    C2RustUnnamed_2 {
        opname: b"BlueValues\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: b"OtherBlues\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: b"FamilyBlues\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: b"FamilyOtherBlues\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: b"StdHW\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"StdVW\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: b"UniqueID\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"XUID\x00" as *const u8 as *const i8,
        argtype: 1i32 << 4i32,
    },
    C2RustUnnamed_2 {
        opname: b"charset\x00" as *const u8 as *const i8,
        argtype: 1i32 << 7i32,
    },
    C2RustUnnamed_2 {
        opname: b"Encoding\x00" as *const u8 as *const i8,
        argtype: 1i32 << 7i32,
    },
    C2RustUnnamed_2 {
        opname: b"CharStrings\x00" as *const u8 as *const i8,
        argtype: 1i32 << 7i32,
    },
    C2RustUnnamed_2 {
        opname: b"Private\x00" as *const u8 as *const i8,
        argtype: 1i32 << 8i32,
    },
    C2RustUnnamed_2 {
        opname: b"Subrs\x00" as *const u8 as *const i8,
        argtype: 1i32 << 7i32,
    },
    C2RustUnnamed_2 {
        opname: b"defaultWidthX\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"nominalWidthX\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"Copyright\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"IsFixedPitch\x00" as *const u8 as *const i8,
        argtype: 1i32 << 2i32,
    },
    C2RustUnnamed_2 {
        opname: b"ItalicAngle\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"UnderlinePosition\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"UnderlineThickness\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"PaintType\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"CharstringType\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"FontMatrix\x00" as *const u8 as *const i8,
        argtype: 1i32 << 4i32,
    },
    C2RustUnnamed_2 {
        opname: b"StrokeWidth\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"BlueScale\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"BlueShift\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"BlueFuzz\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"StemSnapH\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: b"StemSnapV\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: b"ForceBold\x00" as *const u8 as *const i8,
        argtype: 1i32 << 2i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: b"LanguageGroup\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"ExpansionFactor\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"InitialRandomSeed\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"SyntheticBase\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"PostScript\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"BaseFontName\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
    C2RustUnnamed_2 {
        opname: b"BaseFontBlend\x00" as *const u8 as *const i8,
        argtype: 1i32 << 5i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: 0 as *const i8,
        argtype: -1i32,
    },
    C2RustUnnamed_2 {
        opname: b"ROS\x00" as *const u8 as *const i8,
        argtype: 1i32 << 6i32,
    },
    C2RustUnnamed_2 {
        opname: b"CIDFontVersion\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"CIDFontRevision\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"CIDFontType\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"CIDCount\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"UIDBase\x00" as *const u8 as *const i8,
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    C2RustUnnamed_2 {
        opname: b"FDArray\x00" as *const u8 as *const i8,
        argtype: 1i32 << 7i32,
    },
    C2RustUnnamed_2 {
        opname: b"FDSelect\x00" as *const u8 as *const i8,
        argtype: 1i32 << 7i32,
    },
    C2RustUnnamed_2 {
        opname: b"FontName\x00" as *const u8 as *const i8,
        argtype: 1i32 << 3i32,
    },
];
/* Parse DICT data */
unsafe fn get_integer(mut data: *mut *mut u8, mut endptr: *mut u8, mut status: *mut i32) -> f64 {
    let mut result: i32 = 0i32;
    let fresh0 = *data;
    *data = (*data).offset(1);
    let b0 = *fresh0;
    if b0 as i32 == 28i32 && *data < endptr.offset(-2) {
        /* shortint */
        let fresh1 = *data;
        *data = (*data).offset(1);
        let b1 = *fresh1;
        let fresh2 = *data;
        *data = (*data).offset(1);
        let b2 = *fresh2;
        result = b1 as i32 * 256i32 + b2 as i32;
        if result as i64 > 0x7fff {
            result = (result as i64 - 0x10000) as i32
        }
    } else if b0 as i32 == 29i32 && *data < endptr.offset(-4) {
        /* longint */
        let fresh3 = *data;
        *data = (*data).offset(1);
        result = *fresh3 as i32;
        if result > 0x7fi32 {
            result -= 0x100i32
        }
        for _ in 0..3 {
            result = result * 256i32 + **data as i32;
            *data = (*data).offset(1);
        }
    } else if b0 as i32 >= 32i32 && b0 as i32 <= 246i32 {
        /* int (1) */
        result = b0 as i32 - 139i32
    } else if b0 as i32 >= 247i32 && b0 as i32 <= 250i32 {
        /* int (2) */
        let fresh4 = *data;
        *data = (*data).offset(1);
        let b1 = *fresh4;
        result = (b0 as i32 - 247i32) * 256i32 + b1 as i32 + 108i32
    } else if b0 as i32 >= 251i32 && b0 as i32 <= 254i32 {
        let fresh5 = *data;
        *data = (*data).offset(1);
        let b1 = *fresh5;
        result = -(b0 as i32 - 251i32) * 256i32 - b1 as i32 - 108i32
    } else {
        *status = -1i32
    }
    result as f64
}
/* Simply uses strtod */
unsafe fn get_real(mut data: *mut *mut u8, mut endptr: *mut u8, mut status: *mut i32) -> f64 {
    let mut result: f64 = 0.0f64; /* skip first byte (30) */
    let mut nibble: i32 = 0i32;
    let mut len: i32 = 0i32;
    let mut fail: i32 = 0i32;
    if **data as i32 != 30i32 || *data >= endptr.offset(-1) {
        *status = -1i32;
        return 0.0f64;
    }
    *data = (*data).offset(1);
    let mut pos = 0;
    while fail == 0 && len < 1024i32 - 2i32 && *data < endptr {
        /* get nibble */
        if pos % 2i32 != 0 {
            nibble = **data as i32 & 0xfi32;
            *data = (*data).offset(1)
        } else {
            nibble = **data as i32 >> 4i32 & 0xfi32
        }
        if nibble >= 0i32 && nibble <= 0x9i32 {
            let fresh6 = len;
            len = len + 1;
            *work_buffer.as_mut_ptr().offset(fresh6 as isize) = (nibble + '0' as i32) as i8
        } else if nibble == 0xai32 {
            /* . */
            let fresh7 = len;
            len = len + 1;
            *work_buffer.as_mut_ptr().offset(fresh7 as isize) = '.' as i32 as i8
        } else if nibble == 0xbi32 || nibble == 0xci32 {
            /* E, E- */
            let fresh8 = len;
            len = len + 1;
            *work_buffer.as_mut_ptr().offset(fresh8 as isize) = 'e' as i32 as i8;
            if nibble == 0xci32 {
                let fresh9 = len;
                len = len + 1;
                *work_buffer.as_mut_ptr().offset(fresh9 as isize) = '-' as i32 as i8
            }
        } else if nibble == 0xei32 {
            /* `-' */
            let fresh10 = len; /* invalid */
            len = len + 1;
            *work_buffer.as_mut_ptr().offset(fresh10 as isize) = '-' as i32 as i8
        } else if !(nibble == 0xdi32) {
            if nibble == 0xfi32 {
                /* end */
                let fresh11 = len;
                //len = len + 1;
                *work_buffer.as_mut_ptr().offset(fresh11 as isize) = '\u{0}' as i32 as i8;
                if pos % 2i32 == 0i32 && **data as i32 != 0xffi32 {
                    fail = 1i32
                }
                break;
            } else {
                fail = 1i32
            }
        }
        /* skip */
        /* do nothing */
        pos += 1
    }
    /* returned values */
    if fail != 0 || nibble != 0xfi32 {
        *status = -1i32
    } else {
        let mut s: *mut i8 = 0 as *mut i8;
        result = strtod(work_buffer.as_mut_ptr(), &mut s);
        if *s as i32 != 0i32 || errno::errno() == errno::ERANGE {
            *status = -1i32
        }
    }
    result
}
/* operators */
unsafe fn add_dict(
    mut dict: *mut cff_dict,
    mut data: *mut *mut u8,
    mut endptr: *mut u8,
    mut status: *mut i32,
) {
    let mut id = **data as i32;
    if id == 0xci32 {
        *data = (*data).offset(1);
        if *data >= endptr || {
            id = **data as i32 + 22i32;
            id >= 22i32 + 39i32
        } {
            *status = -1i32;
            return;
        }
    } else if id >= 22i32 {
        *status = -1i32;
        return;
    }
    let argtype = dict_operator[id as usize].argtype;
    if dict_operator[id as usize].opname.is_null() || argtype < 0i32 {
        /* YuppySC-Regular.otf from OS X for instance uses op id 37, simply ignore
        this dict instead of treat it as parsing error. */
        return;
    }
    if (*dict).count >= (*dict).max {
        (*dict).max += 16i32;
        (*dict).entries = renew(
            (*dict).entries as *mut libc::c_void,
            ((*dict).max as u32 as u64).wrapping_mul(::std::mem::size_of::<cff_dict_entry>() as u64)
                as u32,
        ) as *mut cff_dict_entry
    }
    (*(*dict).entries.offset((*dict).count as isize)).id = id;
    let ref mut fresh12 = (*(*dict).entries.offset((*dict).count as isize)).key;
    *fresh12 = dict_operator[id as usize].opname;
    if argtype == 1i32 << 0i32 | 1i32 << 1i32
        || argtype == 1i32 << 2i32
        || argtype == 1i32 << 3i32
        || argtype == 1i32 << 7i32
    {
        /* check for underflow here, as exactly one operand is expected */
        if stack_top < 1i32 {
            *status = -3i32;
            return;
        }
        stack_top -= 1;
        (*(*dict).entries.offset((*dict).count as isize)).count = 1i32;
        let ref mut fresh13 = (*(*dict).entries.offset((*dict).count as isize)).values;
        *fresh13 =
            new((1_u64).wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32) as *mut f64;
        *(*(*dict).entries.offset((*dict).count as isize))
            .values
            .offset(0) = arg_stack[stack_top as usize];
        (*dict).count += 1i32
    } else if stack_top > 0i32 {
        (*(*dict).entries.offset((*dict).count as isize)).count = stack_top;
        let ref mut fresh14 = (*(*dict).entries.offset((*dict).count as isize)).values;
        *fresh14 =
            new((stack_top as u32 as u64).wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32)
                as *mut f64;
        while stack_top > 0i32 {
            stack_top -= 1;
            *(*(*dict).entries.offset((*dict).count as isize))
                .values
                .offset(stack_top as isize) = arg_stack[stack_top as usize]
        }
        (*dict).count += 1i32
    }
    *data = (*data).offset(1);
}
/* just ignore operator if there were no operands provided;
don't treat this as underflow (e.g. StemSnapV in TemporaLGCUni-Italic.otf) */
/*
 * All operands are treated as number or array of numbers.
 *  Private: two numbers, size and offset
 *  ROS    : three numbers, SID, SID, and a number
 */
#[no_mangle]
pub unsafe extern "C" fn cff_dict_unpack(mut data: *mut u8, mut endptr: *mut u8) -> *mut cff_dict {
    let mut status: i32 = 0i32;
    stack_top = 0i32;
    let dict = cff_new_dict();
    while data < endptr && status == 0i32 {
        if (*data as i32) < 22i32 {
            /* operator */
            add_dict(dict, &mut data, endptr, &mut status);
        } else if *data as i32 == 30i32 {
            /* real - First byte of a sequence (variable) */
            if stack_top < 64i32 {
                arg_stack[stack_top as usize] = get_real(&mut data, endptr, &mut status); /* everything else are integer */
                stack_top += 1
            } else {
                status = -2i32
            }
        } else if *data as i32 == 255i32 || *data as i32 >= 22i32 && *data as i32 <= 27i32 {
            /* reserved */
            data = data.offset(1)
        } else if stack_top < 64i32 {
            arg_stack[stack_top as usize] = get_integer(&mut data, endptr, &mut status);
            stack_top += 1
        } else {
            status = -2i32
        }
    }
    if status != 0i32 {
        panic!("{}: Parsing CFF DICT failed. (error={})", "CFF", status,);
    } else {
        if stack_top != 0i32 {
            warn!("{}: Garbage in CFF DICT data.", "CFF");
            stack_top = 0i32
        }
    }
    dict
}
/* Pack DICT data */
unsafe fn pack_integer(dest: &mut [u8], mut value: i32) -> usize {
    if value >= -107 && value <= 107 {
        dest[0] = (value + 139 & 0xff) as u8;
        1
    } else if value >= 108 && value <= 1131 {
        let value = (0xf700u32 + (value as u32) - 108) as u16;
        dest[0..2].copy_from_slice(&value.to_be_bytes());
        2
    } else if value >= -1131 && value <= -108 {
        let value = (0xfb00u32.wrapping_sub(value as u32).wrapping_sub(108)) as u16;
        dest[0..2].copy_from_slice(&value.to_be_bytes());
        2
    } else if value >= -32768 && value <= 32767 {
        /* shortint */
        let value = value as i16;
        dest[0] = 28;
        dest[1..3].copy_from_slice(&value.to_be_bytes());
        3
    } else {
        dest[0] = 29;
        dest[1..5].copy_from_slice(&value.to_be_bytes());
        5
    }
}
unsafe fn pack_real(dest: &mut [u8], mut value: f64) -> usize {
    let mut pos = 2_usize;
    let mut buffer: [u8; 32] = [0; 32];
    dest[0] = 30 as u8;
    if value == 0. {
        dest[1] = 0xf as u8;
        return 2;
    }
    if value < 0. {
        dest[1] = 0xe0 as u8;
        value *= -1.;
        pos += 1
    }
    /* To avoid the problem with Mac OS X 10.4 Quartz,
     * change the presion of the real numbers
     * on June 27, 2007 for musix20.pfb */
    sprintf(
        buffer.as_mut_ptr() as *mut i8,
        b"%.13g\x00" as *const u8 as *const i8,
        value,
    );
    let mut i = 0;
    while buffer[i] != '\u{0}' as u8 {
        let mut ch = if buffer[i] == b'.' {
            0xa
        } else if buffer[i] >= b'0' && buffer[i] <= b'9' {
            buffer[i] - b'0'
        } else if buffer[i] == b'e' {
            i += 1;
            (if buffer[i] == b'-' { 0xc } else { 0xb })
        } else {
            panic!("{}: Invalid character.", "CFF")
        };
        if pos % 2 != 0 {
            dest[pos / 2] += ch;
        } else {
            dest[pos / 2] = ((ch as i32) << 4i32) as u8
        }
        pos += 1;
        i += 1
    }
    if pos % 2 != 0 {
        dest[pos / 2] += 0x0f;
        pos += 1
    } else {
        dest[pos / 2] = 0xff as u8;
        pos += 2
    }
    pos / 2
}
unsafe fn cff_dict_put_number(mut value: f64, dest: &mut [u8], mut type_0: i32) -> usize {
    let mut nearint = (value + 0.5f64).floor();
    /* set offset to longint */
    if type_0 == 1i32 << 7i32 {
        let mut lvalue = value as i32; /* integer */
        dest[0] = 29;
        dest[1..5].copy_from_slice(&lvalue.to_be_bytes());
        5
    } else if value > 0x7fffffffi32 as f64
        || value < (-0x7fffffffi32 - 1i32) as f64
        || (value - nearint).abs() > 1.0e-5f64
    {
        /* real */
        pack_real(dest, value)
    } else {
        pack_integer(dest, nearint as i32)
    }
}
unsafe fn put_dict_entry(mut de: *mut cff_dict_entry, dest: &mut [u8]) -> usize {
    let mut len = 0_usize;
    if (*de).count > 0i32 {
        let id = (*de).id;
        let mut type_0 = if dict_operator[id as usize].argtype == 1i32 << 7i32
            || dict_operator[id as usize].argtype == 1i32 << 8i32
        {
            1 << 7
        } else {
            1 << 0 | 1 << 1
        };
        for i in 0..(*de).count {
            len += cff_dict_put_number(*(*de).values.offset(i as isize), &mut dest[len..], type_0);
        }
        if id >= 0i32 && id < 22i32 {
            dest[len] = id as u8;
            len += 1;
        } else if id >= 0i32 && id < 22i32 + 39i32 {
            dest[len] = 12;
            len += 1;
            dest[len] = (id - 22i32) as u8;
            len += 1;
        } else {
            panic!("{}: Invalid CFF DICT operator ID.", "CFF",);
        }
    }
    len
}
#[no_mangle]
pub unsafe extern "C" fn cff_dict_pack(mut dict: *mut cff_dict, dest: &mut [u8]) -> usize {
    let mut len = 0_usize;
    for i in 0..(*dict).count as isize {
        if streq_ptr(
            (*(*dict).entries.offset(i)).key,
            b"ROS\x00" as *const u8 as *const i8,
        ) {
            len += put_dict_entry(&mut *(*dict).entries.offset(i), dest);
            break;
        }
    }
    for i in 0..(*dict).count as isize {
        if strcmp(
            (*(*dict).entries.offset(i)).key,
            b"ROS\x00" as *const u8 as *const i8,
        ) != 0
        {
            len += put_dict_entry(&mut *(*dict).entries.offset(i), &mut dest[len..])
        }
    }
    len
}
#[no_mangle]
pub unsafe extern "C" fn cff_dict_add(mut dict: *mut cff_dict, mut key: *const i8, mut count: i32) {
    let mut id = 0;
    while id < 22 + 39 {
        if !key.is_null()
            && !dict_operator[id as usize].opname.is_null()
            && streq_ptr(dict_operator[id as usize].opname, key) as i32 != 0
        {
            break;
        }
        id += 1
    }
    if id == 22 + 39 {
        panic!("{}: Unknown CFF DICT operator.", "CFF",);
    }
    for i in 0..(*dict).count {
        if (*(*dict).entries.offset(i as isize)).id == id {
            if (*(*dict).entries.offset(i as isize)).count != count {
                panic!("{}: Inconsistent DICT argument number.", "CFF",);
            }
            return;
        }
    }
    if (*dict).count + 1i32 >= (*dict).max {
        (*dict).max += 8i32;
        (*dict).entries = renew(
            (*dict).entries as *mut libc::c_void,
            ((*dict).max as u32 as u64).wrapping_mul(::std::mem::size_of::<cff_dict_entry>() as u64)
                as u32,
        ) as *mut cff_dict_entry
    }
    (*(*dict).entries.offset((*dict).count as isize)).id = id;
    let ref mut fresh20 = (*(*dict).entries.offset((*dict).count as isize)).key;
    *fresh20 = dict_operator[id as usize].opname;
    (*(*dict).entries.offset((*dict).count as isize)).count = count;
    if count > 0i32 {
        let ref mut fresh21 = (*(*dict).entries.offset((*dict).count as isize)).values;
        *fresh21 =
            new((count as u32 as u64).wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32)
                as *mut f64;
        memset(
            (*(*dict).entries.offset((*dict).count as isize)).values as *mut libc::c_void,
            0i32,
            (::std::mem::size_of::<f64>()).wrapping_mul(count as _),
        );
    } else {
        let ref mut fresh22 = (*(*dict).entries.offset((*dict).count as isize)).values;
        *fresh22 = 0 as *mut f64
    }
    (*dict).count += 1i32;
}
#[no_mangle]
pub unsafe extern "C" fn cff_dict_remove(mut dict: *mut cff_dict, mut key: *const i8) {
    for i in 0..(*dict).count {
        if streq_ptr(key, (*(*dict).entries.offset(i as isize)).key) {
            (*(*dict).entries.offset(i as isize)).count = 0i32;
            let ref mut fresh23 = (*(*dict).entries.offset(i as isize)).values;
            *fresh23 =
                mfree((*(*dict).entries.offset(i as isize)).values as *mut libc::c_void) as *mut f64
        }
    }
}
#[no_mangle]
pub unsafe extern "C" fn cff_dict_known(mut dict: *mut cff_dict, mut key: *const i8) -> i32 {
    for i in 0..(*dict).count {
        if streq_ptr(key, (*(*dict).entries.offset(i as isize)).key) as i32 != 0
            && (*(*dict).entries.offset(i as isize)).count > 0i32
        {
            return 1i32;
        }
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn cff_dict_get(
    mut dict: *mut cff_dict,
    mut key: *const i8,
    mut idx: i32,
) -> f64 {
    let mut value: f64 = 0.0f64;
    assert!(!key.is_null() && !dict.is_null());
    let mut i = 0;
    while i < (*dict).count {
        if streq_ptr(key, (*(*dict).entries.offset(i as isize)).key) {
            if (*(*dict).entries.offset(i as isize)).count > idx {
                value = *(*(*dict).entries.offset(i as isize))
                    .values
                    .offset(idx as isize)
            } else {
                panic!("{}: Invalid index number.", "CFF",);
            }
            break;
        } else {
            i += 1
        }
    }
    if i == (*dict).count {
        panic!(
            "{}: DICT entry \"{}\" not found.",
            "CFF",
            CStr::from_ptr(key).display(),
        );
    }
    value
}
#[no_mangle]
pub unsafe extern "C" fn cff_dict_set(
    mut dict: *mut cff_dict,
    mut key: *const i8,
    mut idx: i32,
    mut value: f64,
) {
    assert!(!dict.is_null() && !key.is_null());
    let mut i = 0;
    while i < (*dict).count {
        if streq_ptr(key, (*(*dict).entries.offset(i as isize)).key) {
            if (*(*dict).entries.offset(i as isize)).count > idx {
                *(*(*dict).entries.offset(i as isize))
                    .values
                    .offset(idx as isize) = value
            } else {
                panic!("{}: Invalid index number.", "CFF",);
            }
            break;
        } else {
            i += 1
        }
    }
    if i == (*dict).count {
        panic!(
            "{}: DICT entry \"{}\" not found.",
            "CFF",
            CStr::from_ptr(key).display(),
        );
    };
}
/* decode/encode DICT */
#[no_mangle]
pub unsafe extern "C" fn cff_dict_update(mut dict: *mut cff_dict, cff: &mut cff_font) {
    for i in 0..(*dict).count {
        if (*(*dict).entries.offset(i as isize)).count > 0i32 {
            let id = (*(*dict).entries.offset(i as isize)).id;
            if dict_operator[id as usize].argtype == 1i32 << 3i32 {
                let str = cff_get_string(
                    cff,
                    *(*(*dict).entries.offset(i as isize)).values.offset(0) as s_SID,
                );
                *(*(*dict).entries.offset(i as isize)).values.offset(0) =
                    cff_add_string(cff, str, 1i32) as f64;
                free(str as *mut libc::c_void);
            } else if dict_operator[id as usize].argtype == 1i32 << 6i32 {
                let str = cff_get_string(
                    cff,
                    *(*(*dict).entries.offset(i as isize)).values.offset(0) as s_SID,
                );
                *(*(*dict).entries.offset(i as isize)).values.offset(0) =
                    cff_add_string(cff, str, 1i32) as f64;
                free(str as *mut libc::c_void);
                let str = cff_get_string(
                    cff,
                    *(*(*dict).entries.offset(i as isize)).values.offset(1) as s_SID,
                );
                *(*(*dict).entries.offset(i as isize)).values.offset(1) =
                    cff_add_string(cff, str, 1i32) as f64;
                free(str as *mut libc::c_void);
            }
        }
    }
}
