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
use crate::mfree;
use crate::shims::sprintf;
use crate::warn;
use libc::{free, memset};

pub(crate) type s_SID = u16;

/* CFF Data Types */

//const CFF_TYPE_UNKNOWN: i32 = 0;
const CFF_TYPE_INTEGER: i32 = 1 << 0;
const CFF_TYPE_REAL: i32 = 1 << 1;
const CFF_TYPE_NUMBER: i32 = CFF_TYPE_INTEGER | CFF_TYPE_REAL;
const CFF_TYPE_BOOLEAN: i32 = 1 << 2;
const CFF_TYPE_SID: i32 = 1 << 3;
const CFF_TYPE_ARRAY: i32 = 1 << 4;
const CFF_TYPE_DELTA: i32 = 1 << 5;

/* SID SID number */
const CFF_TYPE_ROS: i32 = 1 << 6;
/* offset(0) */
const CFF_TYPE_OFFSET: i32 = 1 << 7;
/* size offset(0) */
const CFF_TYPE_SZOFF: i32 = 1 << 8;

#[derive(Clone, Copy, Debug)]
enum CffError {
    ParseError,
    StackOverflow,
    StackUnderflow,
    #[allow(unused)]
    StackRangecheck,
}

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

const CFF_DICT_STACK_LIMIT: usize = 64;
static mut stack_top: i32 = 0i32;
static mut arg_stack: [f64; CFF_DICT_STACK_LIMIT] = [0.; CFF_DICT_STACK_LIMIT];

const CFF_LAST_DICT_OP1: usize = 22;
const CFF_LAST_DICT_OP2: usize = 39;
const CFF_LAST_DICT_OP: usize = CFF_LAST_DICT_OP1 + CFF_LAST_DICT_OP2;

static mut dict_operator: [Operator; CFF_LAST_DICT_OP] = [
    Operator {
        opname: "version",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "Notice",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "FullName",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "FamilyName",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "Weight",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "FontBBox",
        argtype: CFF_TYPE_ARRAY,
    },
    Operator {
        opname: "BlueValues",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "OtherBlues",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "FamilyBlues",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "FamilyOtherBlues",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "StdHW",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "StdVW",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "UniqueID",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "XUID",
        argtype: CFF_TYPE_ARRAY,
    },
    Operator {
        opname: "charset",
        argtype: CFF_TYPE_OFFSET,
    },
    Operator {
        opname: "Encoding",
        argtype: CFF_TYPE_OFFSET,
    },
    Operator {
        opname: "CharStrings",
        argtype: CFF_TYPE_OFFSET,
    },
    Operator {
        opname: "Private",
        argtype: CFF_TYPE_SZOFF,
    },
    Operator {
        opname: "Subrs",
        argtype: CFF_TYPE_OFFSET,
    },
    Operator {
        opname: "defaultWidthX",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "nominalWidthX",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "Copyright",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "IsFixedPitch",
        argtype: CFF_TYPE_BOOLEAN,
    },
    Operator {
        opname: "ItalicAngle",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "UnderlinePosition",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "UnderlineThickness",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "PaintType",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "CharstringType",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "FontMatrix",
        argtype: CFF_TYPE_ARRAY,
    },
    Operator {
        opname: "StrokeWidth",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "BlueScale",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "BlueShift",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "BlueFuzz",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "StemSnapH",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "StemSnapV",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "ForceBold",
        argtype: CFF_TYPE_BOOLEAN,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "LanguageGroup",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "ExpansionFactor",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "InitialRandomSeed",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "SyntheticBase",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "PostScript",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "BaseFontName",
        argtype: CFF_TYPE_SID,
    },
    Operator {
        opname: "BaseFontBlend",
        argtype: CFF_TYPE_DELTA,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "",
        argtype: -1,
    },
    Operator {
        opname: "ROS",
        argtype: CFF_TYPE_ROS,
    },
    Operator {
        opname: "CIDFontVersion",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "CIDFontRevision",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "CIDFontType",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "CIDCount",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "UIDBase",
        argtype: CFF_TYPE_NUMBER,
    },
    Operator {
        opname: "FDArray",
        argtype: CFF_TYPE_OFFSET,
    },
    Operator {
        opname: "FDSelect",
        argtype: CFF_TYPE_OFFSET,
    },
    Operator {
        opname: "FontName",
        argtype: CFF_TYPE_SID,
    },
];
/* Parse DICT data */
fn get_integer(data: &mut &[u8]) -> Result<f64, CffError> {
    let b0 = data[0];
    *data = &data[1..];
    Ok(if b0 as i32 == 28i32 && data.len() > 2 {
        /* shortint */
        let b1 = data[0];
        *data = &data[1..];
        let b2 = data[0];
        *data = &data[1..];
        let mut result = b1 as i32 * 256 + b2 as i32;
        if result as i64 > 0x7fff {
            result = (result as i64 - 0x10000) as i32
        }
        result
    } else if b0 as i32 == 29 && data.len() > 4 {
        /* longint */
        let mut result = data[0] as i32;
        *data = &data[1..];
        if result > 0x7f {
            result -= 0x100
        }
        for _ in 0..3 {
            result = result * 256 + data[0] as i32;
            *data = &data[1..];
        }
        result
    } else if b0 as i32 >= 32 && b0 as i32 <= 246 {
        /* int (1) */
        b0 as i32 - 139
    } else if b0 as i32 >= 247 && b0 as i32 <= 250 {
        /* int (2) */
        let b1 = data[0];
        *data = &data[1..];
        (b0 as i32 - 247) * 256 + b1 as i32 + 108
    } else if b0 as i32 >= 251 && b0 as i32 <= 254 {
        let b1 = data[0];
        *data = &data[1..];
        -(b0 as i32 - 251) * 256 - b1 as i32 - 108
    } else {
        return Err(CffError::ParseError);
    } as f64)
}
/* Simply uses strtod */
unsafe fn get_real(data: &mut &[u8]) -> Result<f64, CffError> {
    let mut nibble: i32 = 0;
    let mut fail: i32 = 0;
    /* skip first byte (30) */
    if data[0] as i32 != 30 || data.len() <= 1 {
        return Err(CffError::ParseError);
    }
    *data = &data[1..];
    let mut pos = 0;
    let mut buf = String::new();
    while fail == 0 && buf.len() < 1024 - 2 && !data.is_empty() {
        /* get nibble */
        if pos % 2 != 0 {
            nibble = data[0] as i32 & 0xf;
            *data = &data[1..];
        } else {
            nibble = data[0] as i32 >> 4 & 0xf;
        }
        if nibble >= 0 && nibble <= 0x9 {
            buf.push(char::from((nibble + '0' as i32) as u8));
        } else if nibble == 0xa {
            /* . */
            buf.push('.');
        } else if nibble == 0xb || nibble == 0xc {
            /* E, E- */
            buf.push('e');
            if nibble == 0xc {
                buf.push('-');
            }
        } else if nibble == 0xe {
            /* `-' */
            /* invalid */
            buf.push('-');
        } else if !(nibble == 0xd) {
            if nibble == 0xfi32 {
                /* end */
                if pos % 2 == 0 && data[0] as i32 != 0xff {
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
        return Err(CffError::ParseError);
    } else {
        buf.parse::<f64>().map_err(|_| CffError::ParseError)
    }
}
/* operators */
unsafe fn add_dict(mut dict: *mut cff_dict, data: &mut &[u8]) -> Result<(), CffError> {
    let mut id = data[0] as i32;
    if id == 0xci32 {
        *data = &data[1..];
        if data.is_empty() || {
            id = data[0] as i32 + CFF_LAST_DICT_OP1 as i32;
            id >= CFF_LAST_DICT_OP as i32
        } {
            return Err(CffError::ParseError);
        }
    } else if id >= CFF_LAST_DICT_OP1 as i32 {
        return Err(CffError::ParseError);
    }
    let argtype = dict_operator[id as usize].argtype;
    if dict_operator[id as usize].opname.is_empty() || argtype < 0i32 {
        /* YuppySC-Regular.otf from OS X for instance uses op id 37, simply ignore
        this dict instead of treat it as parsing error. */
        return Ok(());
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
    if argtype == CFF_TYPE_NUMBER
        || argtype == CFF_TYPE_BOOLEAN
        || argtype == CFF_TYPE_SID
        || argtype == CFF_TYPE_OFFSET
    {
        /* check for underflow here, as exactly one operand is expected */
        if stack_top < 1i32 {
            return Err(CffError::StackUnderflow);
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
    *data = &data[1..];
    Ok(())
}
/* just ignore operator if there were no operands provided;
don't treat this as underflow (e.g. StemSnapV in TemporaLGCUni-Italic.otf) */
/*
 * All operands are treated as number or array of numbers.
 *  Private: two numbers, size and offset
 *  ROS    : three numbers, SID, SID, and a number
 */

pub(crate) unsafe fn cff_dict_unpack(mut data: &[u8]) -> *mut cff_dict {
    fn expect<T>(res: Result<T, CffError>) -> T {
        match res {
            Ok(res) => res,
            Err(e) => panic!("{}: Parsing CFF DICT failed. (error={:?})", "CFF", e),
        }
    }

    stack_top = 0i32;
    let dict = cff_new_dict();
    while !data.is_empty() {
        if (data[0] as i32) < 22 {
            /* operator */
            expect(add_dict(dict, &mut data));
        } else if data[0] as i32 == 30 {
            /* real - First byte of a sequence (variable) */
            if stack_top < CFF_DICT_STACK_LIMIT as i32 {
                arg_stack[stack_top as usize] = expect(get_real(&mut data)); /* everything else are integer */
                stack_top += 1
            } else {
                expect(Result::<(), _>::Err(CffError::StackOverflow));
            }
        } else if data[0] as i32 == 255 || data[0] as i32 >= 22 && data[0] as i32 <= 27 {
            /* reserved */
            data = &data[1..];
        } else if stack_top < CFF_DICT_STACK_LIMIT as i32 {
            arg_stack[stack_top as usize] = expect(get_integer(&mut data));
            stack_top += 1
        } else {
            expect(Result::<(), _>::Err(CffError::StackOverflow));
        }
    }
    if stack_top != 0 {
        warn!("{}: Garbage in CFF DICT data.", "CFF");
        stack_top = 0
    }
    dict
}
/* Pack DICT data */
fn pack_integer(dest: &mut [u8], value: i32) -> usize {
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
    if type_0 == CFF_TYPE_OFFSET {
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
        let type_0 = if dict_operator[id as usize].argtype == CFF_TYPE_OFFSET
            || dict_operator[id as usize].argtype == CFF_TYPE_SZOFF
        {
            CFF_TYPE_OFFSET
        } else {
            CFF_TYPE_NUMBER
        };
        for i in 0..(*de).count {
            len += cff_dict_put_number(*(*de).values.offset(i as isize), &mut dest[len..], type_0);
        }
        if id >= 0 && id < CFF_LAST_DICT_OP1 as i32 {
            dest[len] = id as u8;
            len += 1;
        } else if id >= 0 && id < CFF_LAST_DICT_OP as i32 {
            dest[len] = 12;
            len += 1;
            dest[len] = (id - CFF_LAST_DICT_OP1 as i32) as u8;
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
        while id < CFF_LAST_DICT_OP as i32 {
            if !key.is_empty()
                && !dict_operator[id as usize].opname.is_empty()
                && dict_operator[id as usize].opname == key
            {
                break;
            }
            id += 1
        }
        if id == CFF_LAST_DICT_OP as i32 {
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
                if dict_operator[id as usize].argtype == CFF_TYPE_SID {
                    let s = cff_get_string(
                        cff,
                        *(*self.entries.offset(i as isize)).values.offset(0) as s_SID,
                    );
                    *(*self.entries.offset(i as isize)).values.offset(0) =
                        cff_add_string(cff, &s, 1i32) as f64;
                } else if dict_operator[id as usize].argtype == CFF_TYPE_ROS {
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
