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

use std::ptr;

use super::dpx_cff::{cff_add_string, cff_get_string};
use super::dpx_mem::{new, renew};
use super::dpx_mfileio::work_buffer;
use crate::bridge::stub_errno as errno;
use crate::mfree;
use crate::shims::sprintf;
use crate::warn;
use libc::{free, memset, strtod};

pub(crate) type s_SID = u16;
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
pub(crate) struct Operator {
    pub(crate) opname: &'static str,
    pub(crate) argtype: i32,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */

pub(crate) unsafe fn cff_new_dict() -> *mut cff_dict {
    let dict =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cff_dict>() as u64) as u32) as *mut cff_dict;
    (*dict).max = 16i32;
    (*dict).count = 0i32;
    (*dict).entries = new(((*dict).max as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<cff_dict_entry>() as u64)
        as u32) as *mut cff_dict_entry;
    dict
}

pub(crate) unsafe fn cff_release_dict(dict: &mut cff_dict) {
    if !dict.entries.is_null() {
        for i in 0..dict.count {
            free((*dict.entries.offset(i as isize)).values as *mut libc::c_void);
        }
        free(dict.entries as *mut libc::c_void);
    }
    free(dict as *mut cff_dict as *mut libc::c_void);
}
static mut stack_top: i32 = 0i32;
static mut arg_stack: [f64; 64] = [0.; 64];
static mut dict_operator: [Operator; 61] = [
    Operator {
        opname: "version",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "Notice",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "FullName",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "FamilyName",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "Weight",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "FontBBox",
        argtype: 1i32 << 4i32,
    },
    Operator {
        opname: "BlueValues",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "OtherBlues",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "FamilyBlues",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "FamilyOtherBlues",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "StdHW",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "StdVW",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "UniqueID",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "XUID",
        argtype: 1i32 << 4i32,
    },
    Operator {
        opname: "charset",
        argtype: 1i32 << 7i32,
    },
    Operator {
        opname: "Encoding",
        argtype: 1i32 << 7i32,
    },
    Operator {
        opname: "CharStrings",
        argtype: 1i32 << 7i32,
    },
    Operator {
        opname: "Private",
        argtype: 1i32 << 8i32,
    },
    Operator {
        opname: "Subrs",
        argtype: 1i32 << 7i32,
    },
    Operator {
        opname: "defaultWidthX",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "nominalWidthX",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "Copyright",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "IsFixedPitch",
        argtype: 1i32 << 2i32,
    },
    Operator {
        opname: "ItalicAngle",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "UnderlinePosition",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "UnderlineThickness",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "PaintType",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "CharstringType",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "FontMatrix",
        argtype: 1i32 << 4i32,
    },
    Operator {
        opname: "StrokeWidth",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "BlueScale",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "BlueShift",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "BlueFuzz",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "StemSnapH",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "StemSnapV",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "ForceBold",
        argtype: 1i32 << 2i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "LanguageGroup",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "ExpansionFactor",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "InitialRandomSeed",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "SyntheticBase",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "PostScript",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "BaseFontName",
        argtype: 1i32 << 3i32,
    },
    Operator {
        opname: "BaseFontBlend",
        argtype: 1i32 << 5i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "",
        argtype: -1i32,
    },
    Operator {
        opname: "ROS",
        argtype: 1i32 << 6i32,
    },
    Operator {
        opname: "CIDFontVersion",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "CIDFontRevision",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "CIDFontType",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "CIDCount",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "UIDBase",
        argtype: 1i32 << 0i32 | 1i32 << 1i32,
    },
    Operator {
        opname: "FDArray",
        argtype: 1i32 << 7i32,
    },
    Operator {
        opname: "FDSelect",
        argtype: 1i32 << 7i32,
    },
    Operator {
        opname: "FontName",
        argtype: 1i32 << 3i32,
    },
];
/* Parse DICT data */
unsafe fn get_integer(data: *mut *mut u8, endptr: *mut u8, status: *mut i32) -> f64 {
    let mut result: i32 = 0i32;
    let b0 = **data;
    *data = (*data).offset(1);
    if b0 as i32 == 28i32 && *data < endptr.offset(-2) {
        /* shortint */
        let b1 = **data;
        *data = (*data).offset(1);
        let b2 = **data;
        *data = (*data).offset(1);
        result = b1 as i32 * 256 + b2 as i32;
        if result as i64 > 0x7fff {
            result = (result as i64 - 0x10000) as i32
        }
    } else if b0 as i32 == 29 && *data < endptr.offset(-4) {
        /* longint */
        result = **data as i32;
        *data = (*data).offset(1);
        if result > 0x7f {
            result -= 0x100
        }
        for _ in 0..3 {
            result = result * 256 + **data as i32;
            *data = (*data).offset(1);
        }
    } else if b0 as i32 >= 32 && b0 as i32 <= 246 {
        /* int (1) */
        result = b0 as i32 - 139
    } else if b0 as i32 >= 247 && b0 as i32 <= 250 {
        /* int (2) */
        let b1 = **data;
        *data = (*data).offset(1);
        result = (b0 as i32 - 247) * 256 + b1 as i32 + 108
    } else if b0 as i32 >= 251 && b0 as i32 <= 254 {
        let b1 = **data;
        *data = (*data).offset(1);
        result = -(b0 as i32 - 251) * 256 - b1 as i32 - 108
    } else {
        *status = -1;
    }
    result as f64
}
/* Simply uses strtod */
unsafe fn get_real(data: *mut *mut u8, endptr: *mut u8, status: *mut i32) -> f64 {
    let mut result: f64 = 0.; /* skip first byte (30) */
    let mut nibble: i32 = 0;
    let mut len: i32 = 0;
    let mut fail: i32 = 0;
    if **data as i32 != 30 || *data >= endptr.offset(-1) {
        *status = -1;
        return 0.;
    }
    *data = (*data).offset(1);
    let mut pos = 0;
    while fail == 0 && len < 1024 - 2 && *data < endptr {
        /* get nibble */
        if pos % 2 != 0 {
            nibble = **data as i32 & 0xf;
            *data = (*data).offset(1)
        } else {
            nibble = **data as i32 >> 4 & 0xf;
        }
        if nibble >= 0 && nibble <= 0x9 {
            *work_buffer.as_mut_ptr().offset(len as isize) = (nibble + '0' as i32) as i8;
            len += 1;
        } else if nibble == 0xa {
            /* . */
            *work_buffer.as_mut_ptr().offset(len as isize) = '.' as i32 as i8;
            len += 1;
        } else if nibble == 0xb || nibble == 0xc {
            /* E, E- */
            *work_buffer.as_mut_ptr().offset(len as isize) = 'e' as i32 as i8;
            len += 1;
            if nibble == 0xc {
                *work_buffer.as_mut_ptr().offset(len as isize) = '-' as i32 as i8;
                len += 1;
            }
        } else if nibble == 0xe {
            /* `-' */
            /* invalid */
            *work_buffer.as_mut_ptr().offset(len as isize) = '-' as i32 as i8;
            len += 1;
        } else if !(nibble == 0xd) {
            if nibble == 0xfi32 {
                /* end */
                *work_buffer.as_mut_ptr().offset(len as isize) = '\u{0}' as i32 as i8;
                if pos % 2 == 0 && **data as i32 != 0xff {
                    fail = 1
                }
                break;
            } else {
                fail = 1;
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
        let mut s: *mut i8 = ptr::null_mut();
        result = strtod(work_buffer.as_mut_ptr(), &mut s);
        if *s as i32 != 0i32 || errno::errno() == errno::ERANGE {
            *status = -1i32
        }
    }
    result
}
/* operators */
unsafe fn add_dict(mut dict: *mut cff_dict, data: *mut *mut u8, endptr: *mut u8, status: *mut i32) {
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
    if dict_operator[id as usize].opname.is_empty() || argtype < 0i32 {
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
    (*(*dict).entries.offset((*dict).count as isize)).key = dict_operator[id as usize].opname;
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
        (*(*dict).entries.offset((*dict).count as isize)).values =
            new((1_u64).wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32) as *mut f64;
        *(*(*dict).entries.offset((*dict).count as isize))
            .values
            .offset(0) = arg_stack[stack_top as usize];
        (*dict).count += 1i32
    } else if stack_top > 0i32 {
        (*(*dict).entries.offset((*dict).count as isize)).count = stack_top;
        (*(*dict).entries.offset((*dict).count as isize)).values = new((stack_top as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<f64>() as u64)
            as u32) as *mut f64;
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

pub(crate) unsafe fn cff_dict_unpack(mut data: *mut u8, endptr: *mut u8) -> *mut cff_dict {
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
        panic!("{}: Parsing CFF DICT failed. (error={})", "CFF", status);
    } else {
        if stack_top != 0i32 {
            warn!("{}: Garbage in CFF DICT data.", "CFF");
            stack_top = 0i32
        }
    }
    dict
}
/* Pack DICT data */
unsafe fn pack_integer(dest: &mut [u8], value: i32) -> usize {
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
        let ch = if buffer[i] == b'.' {
            0xa
        } else if buffer[i] >= b'0' && buffer[i] <= b'9' {
            buffer[i] - b'0'
        } else if buffer[i] == b'e' {
            i += 1;
            if buffer[i] == b'-' {
                0xc
            } else {
                0xb
            }
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
unsafe fn cff_dict_put_number(value: f64, dest: &mut [u8], type_0: i32) -> usize {
    let nearint = (value + 0.5f64).floor();
    /* set offset to longint */
    if type_0 == 1i32 << 7i32 {
        let lvalue = value as i32; /* integer */
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
unsafe fn put_dict_entry(de: &cff_dict_entry, dest: &mut [u8]) -> usize {
    let mut len = 0_usize;
    if (*de).count > 0i32 {
        let id = (*de).id;
        let type_0 = if dict_operator[id as usize].argtype == 1i32 << 7i32
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
            panic!("{}: Invalid CFF DICT operator ID.", "CFF");
        }
    }
    len
}

impl cff_dict {
    pub(crate) unsafe fn pack(&self, dest: &mut [u8]) -> usize {
        let mut len = 0_usize;
        for i in 0..self.count as isize {
            if (*self.entries.offset(i)).key == "ROS" {
                len += put_dict_entry(&*self.entries.offset(i), dest);
                break;
            }
        }
        for i in 0..self.count as isize {
            if (*self.entries.offset(i)).key != "ROS" {
                len += put_dict_entry(&*self.entries.offset(i), &mut dest[len..])
            }
        }
        len
    }

    pub(crate) unsafe fn add(&mut self, key: &str, count: i32) {
        let mut id = 0;
        while id < 22 + 39 {
            if !key.is_empty()
                && !dict_operator[id as usize].opname.is_empty()
                && dict_operator[id as usize].opname == key
            {
                break;
            }
            id += 1
        }
        if id == 22 + 39 {
            panic!("{}: Unknown CFF DICT operator.", "CFF");
        }
        for i in 0..self.count {
            if (*self.entries.offset(i as isize)).id == id {
                if (*self.entries.offset(i as isize)).count != count {
                    panic!("{}: Inconsistent DICT argument number.", "CFF");
                }
                return;
            }
        }
        if self.count + 1i32 >= self.max {
            self.max += 8i32;
            self.entries = renew(
                self.entries as *mut libc::c_void,
                (self.max as u32 as u64)
                    .wrapping_mul(::std::mem::size_of::<cff_dict_entry>() as u64)
                    as u32,
            ) as *mut cff_dict_entry
        }
        (*self.entries.offset(self.count as isize)).id = id;
        (*self.entries.offset(self.count as isize)).key = dict_operator[id as usize].opname;
        (*self.entries.offset(self.count as isize)).count = count;
        if count > 0i32 {
            (*self.entries.offset(self.count as isize)).values =
                new((count as u32 as u64).wrapping_mul(::std::mem::size_of::<f64>() as u64) as u32)
                    as *mut f64;
            memset(
                (*self.entries.offset(self.count as isize)).values as *mut libc::c_void,
                0i32,
                (::std::mem::size_of::<f64>()).wrapping_mul(count as _),
            );
        } else {
            (*self.entries.offset(self.count as isize)).values = ptr::null_mut()
        }
        self.count += 1i32;
    }

    pub(crate) unsafe fn remove(&mut self, key: &str) {
        for i in 0..self.count {
            if key == (*self.entries.offset(i as isize)).key {
                (*self.entries.offset(i as isize)).count = 0i32;
                (*self.entries.offset(i as isize)).values =
                    mfree((*self.entries.offset(i as isize)).values as *mut libc::c_void)
                        as *mut f64
            }
        }
    }

    pub(crate) unsafe fn contains_key(&self, key: &str) -> bool {
        for i in 0..self.count {
            if key == (*self.entries.offset(i as isize)).key
                && (*self.entries.offset(i as isize)).count > 0i32
            {
                return true;
            }
        }
        false
    }

    pub(crate) unsafe fn get(&self, key: &str, idx: i32) -> f64 {
        let mut value: f64 = 0.0f64;
        assert!(!key.is_empty());
        let mut i = 0;
        while i < self.count {
            if key == (*self.entries.offset(i as isize)).key {
                if (*self.entries.offset(i as isize)).count > idx {
                    value = *(*self.entries.offset(i as isize))
                        .values
                        .offset(idx as isize)
                } else {
                    panic!("{}: Invalid index number.", "CFF");
                }
                break;
            } else {
                i += 1
            }
        }
        if i == self.count {
            panic!("{}: DICT entry \"{}\" not found.", "CFF", key,);
        }
        value
    }

    pub(crate) unsafe fn set(&mut self, key: &str, idx: i32, value: f64) {
        assert!(!key.is_empty());
        let mut i = 0;
        while i < self.count {
            if key == (*self.entries.offset(i as isize)).key {
                if (*self.entries.offset(i as isize)).count > idx {
                    *(*self.entries.offset(i as isize))
                        .values
                        .offset(idx as isize) = value
                } else {
                    panic!("{}: Invalid index number.", "CFF");
                }
                break;
            } else {
                i += 1
            }
        }
        if i == self.count {
            panic!("{}: DICT entry \"{}\" not found.", "CFF", key,);
        };
    }

    /* decode/encode DICT */
    pub(crate) unsafe fn update(&mut self, cff: &mut cff_font) {
        for i in 0..self.count {
            if (*self.entries.offset(i as isize)).count > 0i32 {
                let id = (*self.entries.offset(i as isize)).id;
                if dict_operator[id as usize].argtype == 1i32 << 3i32 {
                    let s = cff_get_string(
                        cff,
                        *(*self.entries.offset(i as isize)).values.offset(0) as s_SID,
                    );
                    *(*self.entries.offset(i as isize)).values.offset(0) =
                        cff_add_string(cff, &s, 1i32) as f64;
                } else if dict_operator[id as usize].argtype == 1i32 << 6i32 {
                    let s = cff_get_string(
                        cff,
                        *(*self.entries.offset(i as isize)).values.offset(0) as s_SID,
                    );
                    *(*self.entries.offset(i as isize)).values.offset(0) =
                        cff_add_string(cff, &s, 1i32) as f64;
                    let s = cff_get_string(
                        cff,
                        *(*self.entries.offset(i as isize)).values.offset(1) as s_SID,
                    );
                    *(*self.entries.offset(i as isize)).values.offset(1) =
                        cff_add_string(cff, &s, 1i32) as f64;
                }
            }
        }
    }
}
