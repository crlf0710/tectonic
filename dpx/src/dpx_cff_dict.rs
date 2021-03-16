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

use super::dpx_cff::{cff_add_string, cff_get_string};
use crate::shims::sprintf;
use crate::warn;

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
/* CID-Keyed font specific */
use super::dpx_cff::cff_font;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct Operator {
    pub(crate) opname: &'static str,
    pub(crate) argtype: i32,
}

impl Operator {
    const fn new(opname: &'static str, argtype: i32) -> Self {
        Self { opname, argtype }
    }
}

#[derive(Clone, Default)]
#[repr(C)]
pub(crate) struct cff_dict {
    pub(crate) entries: Vec<cff_dict_entry>,
}

impl cff_dict {
    pub(crate) fn new() -> Self {
        Self {
            entries: Vec::with_capacity(16),
        }
    }
}

/* Dictionary */
#[derive(Clone)]
pub(crate) struct cff_dict_entry {
    pub(crate) id: i32,
    pub(crate) key: &'static str,
    pub(crate) values: Box<[f64]>,
    /* values                                  */
}

const CFF_DICT_STACK_LIMIT: usize = 64;
static mut stack_top: i32 = 0;
static mut arg_stack: [f64; CFF_DICT_STACK_LIMIT] = [0.; CFF_DICT_STACK_LIMIT];

const CFF_LAST_DICT_OP1: usize = 22;
const CFF_LAST_DICT_OP2: usize = 39;
const CFF_LAST_DICT_OP: usize = CFF_LAST_DICT_OP1 + CFF_LAST_DICT_OP2;

static mut dict_operator: [Operator; CFF_LAST_DICT_OP] = [
    Operator::new("version", CFF_TYPE_SID),
    Operator::new("Notice", CFF_TYPE_SID),
    Operator::new("FullName", CFF_TYPE_SID),
    Operator::new("FamilyName", CFF_TYPE_SID),
    Operator::new("Weight", CFF_TYPE_SID),
    Operator::new("FontBBox", CFF_TYPE_ARRAY),
    Operator::new("BlueValues", CFF_TYPE_DELTA),
    Operator::new("OtherBlues", CFF_TYPE_DELTA),
    Operator::new("FamilyBlues", CFF_TYPE_DELTA),
    Operator::new("FamilyOtherBlues", CFF_TYPE_DELTA),
    Operator::new("StdHW", CFF_TYPE_NUMBER),
    Operator::new("StdVW", CFF_TYPE_NUMBER),
    Operator::new("", -1),
    Operator::new("UniqueID", CFF_TYPE_NUMBER),
    Operator::new("XUID", CFF_TYPE_ARRAY),
    Operator::new("charset", CFF_TYPE_OFFSET),
    Operator::new("Encoding", CFF_TYPE_OFFSET),
    Operator::new("CharStrings", CFF_TYPE_OFFSET),
    Operator::new("Private", CFF_TYPE_SZOFF),
    Operator::new("Subrs", CFF_TYPE_OFFSET),
    Operator::new("defaultWidthX", CFF_TYPE_NUMBER),
    Operator::new("nominalWidthX", CFF_TYPE_NUMBER),
    Operator::new("Copyright", CFF_TYPE_SID),
    Operator::new("IsFixedPitch", CFF_TYPE_BOOLEAN),
    Operator::new("ItalicAngle", CFF_TYPE_NUMBER),
    Operator::new("UnderlinePosition", CFF_TYPE_NUMBER),
    Operator::new("UnderlineThickness", CFF_TYPE_NUMBER),
    Operator::new("PaintType", CFF_TYPE_NUMBER),
    Operator::new("CharstringType", CFF_TYPE_NUMBER),
    Operator::new("FontMatrix", CFF_TYPE_ARRAY),
    Operator::new("StrokeWidth", CFF_TYPE_NUMBER),
    Operator::new("BlueScale", CFF_TYPE_NUMBER),
    Operator::new("BlueShift", CFF_TYPE_NUMBER),
    Operator::new("BlueFuzz", CFF_TYPE_NUMBER),
    Operator::new("StemSnapH", CFF_TYPE_DELTA),
    Operator::new("StemSnapV", CFF_TYPE_DELTA),
    Operator::new("ForceBold", CFF_TYPE_BOOLEAN),
    Operator::new("", -1),
    Operator::new("", -1),
    Operator::new("LanguageGroup", CFF_TYPE_NUMBER),
    Operator::new("ExpansionFactor", CFF_TYPE_NUMBER),
    Operator::new("InitialRandomSeed", CFF_TYPE_NUMBER),
    Operator::new("SyntheticBase", CFF_TYPE_NUMBER),
    Operator::new("PostScript", CFF_TYPE_SID),
    Operator::new("BaseFontName", CFF_TYPE_SID),
    Operator::new("BaseFontBlend", CFF_TYPE_DELTA),
    Operator::new("", -1),
    Operator::new("", -1),
    Operator::new("", -1),
    Operator::new("", -1),
    Operator::new("", -1),
    Operator::new("", -1),
    Operator::new("ROS", CFF_TYPE_ROS),
    Operator::new("CIDFontVersion", CFF_TYPE_NUMBER),
    Operator::new("CIDFontRevision", CFF_TYPE_NUMBER),
    Operator::new("CIDFontType", CFF_TYPE_NUMBER),
    Operator::new("CIDCount", CFF_TYPE_NUMBER),
    Operator::new("UIDBase", CFF_TYPE_NUMBER),
    Operator::new("FDArray", CFF_TYPE_OFFSET),
    Operator::new("FDSelect", CFF_TYPE_OFFSET),
    Operator::new("FontName", CFF_TYPE_SID),
];
/* Parse DICT data */
fn get_integer(data: &mut &[u8]) -> Result<f64, CffError> {
    let b0 = data[0];
    *data = &data[1..];
    Ok(if b0 as i32 == 28 && data.len() > 2 {
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
            if nibble == 0xf {
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
    if fail != 0 || nibble != 0xf {
        return Err(CffError::ParseError);
    } else {
        buf.parse::<f64>().map_err(|_| CffError::ParseError)
    }
}
/* operators */
unsafe fn add_dict(dict: &mut cff_dict, data: &mut &[u8]) -> Result<(), CffError> {
    let mut id = data[0] as i32;
    if id == 0xc {
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
    if dict_operator[id as usize].opname.is_empty() || argtype < 0 {
        /* YuppySC-Regular.otf from OS X for instance uses op id 37, simply ignore
        this dict instead of treat it as parsing error. */
        return Ok(());
    }
    if argtype == CFF_TYPE_NUMBER
        || argtype == CFF_TYPE_BOOLEAN
        || argtype == CFF_TYPE_SID
        || argtype == CFF_TYPE_OFFSET
    {
        /* check for underflow here, as exactly one operand is expected */
        if stack_top < 1 {
            return Err(CffError::StackUnderflow);
        }
        stack_top -= 1;
        dict.entries.push(cff_dict_entry {
            id,
            key: dict_operator[id as usize].opname,
            values: vec![arg_stack[stack_top as usize]].into_boxed_slice(),
        });
    } else if stack_top > 0 {
        dict.entries.push(cff_dict_entry {
            id,
            key: dict_operator[id as usize].opname,
            values: Vec::from(&arg_stack[..stack_top as usize]).into_boxed_slice(),
        });
        stack_top = 0;
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

pub(crate) unsafe fn cff_dict_unpack(mut data: &[u8]) -> cff_dict {
    fn expect<T>(res: Result<T, CffError>) -> T {
        match res {
            Ok(res) => res,
            Err(e) => panic!("{}: Parsing CFF DICT failed. (error={:?})", "CFF", e),
        }
    }

    stack_top = 0;
    let mut dict = cff_dict::new();
    while !data.is_empty() {
        if (data[0] as i32) < 22 {
            /* operator */
            expect(add_dict(&mut dict, &mut data));
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
        let value = (0xf700 + (value as u32) - 108) as u16;
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
            dest[pos / 2] = ((ch as i32) << 4) as u8
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
    } else if value > 0x7fffffff as f64
        || value < (-0x7fffffff - 1) as f64
        || (value - nearint).abs() > 1.0e-5f64
    {
        /* real */
        pack_real(dest, value)
    } else {
        pack_integer(dest, nearint as i32)
    }
}
unsafe fn put_dict_entry(de: &cff_dict_entry, dest: &mut [u8]) -> usize {
    let mut len = 0;
    if !de.values.is_empty() {
        let id = de.id;
        let type_0 = if dict_operator[id as usize].argtype == CFF_TYPE_OFFSET
            || dict_operator[id as usize].argtype == CFF_TYPE_SZOFF
        {
            CFF_TYPE_OFFSET
        } else {
            CFF_TYPE_NUMBER
        };
        for &val in &*de.values {
            len += cff_dict_put_number(val, &mut dest[len..], type_0);
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
        for e in &self.entries {
            if e.key == "ROS" {
                len += put_dict_entry(&e, dest);
                break;
            }
        }
        for e in &self.entries {
            if e.key != "ROS" {
                len += put_dict_entry(&e, &mut dest[len..])
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
        for e in &self.entries {
            if e.id == id {
                if e.values.len() != count as usize {
                    panic!("{}: Inconsistent DICT argument number.", "CFF");
                }
                return;
            }
        }
        self.entries.push(cff_dict_entry {
            id,
            key: dict_operator[id as usize].opname,
            values: if count > 0 {
                vec![0.; count as usize].into_boxed_slice()
            } else {
                Vec::new().into_boxed_slice()
            },
        });
    }

    pub(crate) unsafe fn remove(&mut self, key: &str) {
        for e in &mut self.entries {
            if key == e.key {
                e.values = Vec::new().into_boxed_slice();
            }
        }
    }

    pub(crate) unsafe fn contains_key(&self, key: &str) -> bool {
        for e in &self.entries {
            if key == e.key && !e.values.is_empty() {
                return true;
            }
        }
        false
    }

    pub(crate) unsafe fn get(&self, key: &str, idx: i32) -> f64 {
        assert!(!key.is_empty());
        for e in &self.entries {
            if key == e.key {
                if e.values.len() > idx as usize {
                    return e.values[idx as usize];
                } else {
                    panic!("{}: Invalid index number.", "CFF");
                }
            }
        }
        panic!("{}: DICT entry \"{}\" not found.", "CFF", key)
    }

    pub(crate) unsafe fn set(&mut self, key: &str, idx: i32, value: f64) {
        assert!(!key.is_empty());
        for e in &mut self.entries {
            if key == e.key {
                if e.values.len() > idx as usize {
                    e.values[idx as usize] = value
                } else {
                    panic!("{}: Invalid index number.", "CFF");
                }
                return;
            }
        }
        panic!("{}: DICT entry \"{}\" not found.", "CFF", key);
    }

    /* decode/encode DICT */
    pub(crate) unsafe fn update(&mut self, cff: &mut cff_font) {
        for e in &mut self.entries {
            if !e.values.is_empty() {
                let id = e.id;
                if dict_operator[id as usize].argtype == CFF_TYPE_SID {
                    let s = cff_get_string(cff, e.values[0] as s_SID);
                    e.values[0] = cff_add_string(cff, &s, 1) as f64;
                } else if dict_operator[id as usize].argtype == CFF_TYPE_ROS {
                    let s = cff_get_string(cff, e.values[0] as s_SID);
                    e.values[0] = cff_add_string(cff, &s, 1) as f64;
                    let s = cff_get_string(cff, e.values[1] as s_SID);
                    e.values[1] = cff_add_string(cff, &s, 1) as f64;
                }
            }
        }
    }
}
