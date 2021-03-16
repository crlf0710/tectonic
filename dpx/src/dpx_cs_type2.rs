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

use crate::warn;
use libc::memmove;

use std::ptr;

use crate::dpx_cff::CffIndex;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cs_ginfo {
    pub(crate) flags: i32,
    pub(crate) wx: f64,
    pub(crate) wy: f64,
    pub(crate) bbox: C2RustUnnamed_0,
    pub(crate) seac: C2RustUnnamed,
}

impl cs_ginfo {
    pub(crate) fn new() -> Self {
        Self {
            flags: 0,
            wx: 0.,
            wy: 0.,
            bbox: C2RustUnnamed_0 {
                llx: 0.,
                lly: 0.,
                urx: 0.,
                ury: 0.,
            },
            seac: C2RustUnnamed {
                asb: 0.,
                adx: 0.,
                ady: 0.,
                bchar: 0,
                achar: 0,
            },
        }
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed {
    pub(crate) asb: f64,
    pub(crate) adx: f64,
    pub(crate) ady: f64,
    pub(crate) bchar: u8,
    pub(crate) achar: u8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_0 {
    pub(crate) llx: f64,
    pub(crate) lly: f64,
    pub(crate) urx: f64,
    pub(crate) ury: f64,
}
static mut status: i32 = -1;
/* hintmask and cntrmask need number of stem zones */
static mut num_stems: i32 = 0;
static mut phase: i32 = 0;
/* subroutine nesting */
static mut nest: i32 = 0;
/* advance width */
static mut have_width: i32 = 0;
static mut width: f64 = 0.0f64;
/* Operand stack and Transient array */
static mut stack_top: i32 = 0;
static mut arg_stack: [f64; 48] = [0.; 48];
static mut trn_array: [f64; 32] = [0.; 32];
/*
 * clear_stack() put all operands sotred in operand stack to dest.
 */
unsafe fn clear_stack(dest: &mut *mut u8, limit: *mut u8) {
    for i in 0..stack_top {
        let value = arg_stack[i as usize];
        /* Nearest integer value */
        let mut ivalue = (value + 0.5f64).floor() as i32;
        if value >= 0x8000i64 as f64 || value <= (-0x8000 - 1 as i64) as f64 {
            /*
             * This number cannot be represented as a single operand.
             * We must use `a b mul ...' or `a c div' to represent large values.
             */
            panic!(
                "{}: Argument value too large. (This is bug)",
                "Type2 Charstring Parser",
            );
        } else {
            if (value - ivalue as f64).abs() > 3.0e-5f64 {
                /* 16.16-bit signed fixed value  */
                if limit < (*dest).offset(5) {
                    status = -3;
                    return;
                }
                **dest = 255;
                *dest = (*dest).offset(1);
                /* Everything else are integers. */
                ivalue = value.floor() as i32; /* mantissa */
                /* fraction */
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1); /* Shouldn't come here */
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
                ivalue = ((value - ivalue as f64) * 0x10000i64 as f64) as i32;
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1);
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
            } else if ivalue >= -107 && ivalue <= 107 {
                if limit < (*dest).offset(1) {
                    status = -3;
                    return;
                }
                **dest = (ivalue + 139) as u8;
                *dest = (*dest).offset(1);
            } else if ivalue >= 108 && ivalue <= 1131 {
                if limit < (*dest).offset(2) {
                    status = -3;
                    return;
                }
                ivalue = 0xf700u32.wrapping_add(ivalue as u32).wrapping_sub(108_u32) as i32;
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1);
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
            } else if ivalue >= -1131 && ivalue <= -108 {
                if limit < (*dest).offset(2) {
                    status = -3;
                    return;
                }
                ivalue = 0xfb00u32.wrapping_sub(ivalue as u32).wrapping_sub(108_u32) as i32;
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1);
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
            } else if ivalue >= -32768 && ivalue <= 32767 {
                /* shortint */
                if limit < (*dest).offset(3) {
                    status = -3; /* clear stack */
                    return;
                }
                **dest = 28;
                *dest = (*dest).offset(1);
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1);
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
            } else {
                panic!("{}: Unexpected error.", "Type2 Charstring Parser");
            }
        }
    }
    stack_top = 0;
}
/*
 * Single byte operators:
 *  Path construction, Operator for finishing a path, Hint operators.
 *
 * phase:
 *  0: inital state
 *  1: hint declaration, first stack-clearing operator appeared
 *  2: in path construction
 */
unsafe fn do_operator1(
    dest: &mut *mut u8,
    limit: *mut u8,
    data: &mut *const u8,
    endptr: *const u8,
) {
    let op: u8 = **data;
    *data = (*data).offset(1);
    match op as i32 {
        18 | 23 | 1 | 3 => {
            /* charstring may have hintmask if above operator have seen */
            if phase == 0 && stack_top % 2 != 0 {
                have_width = 1;
                width = arg_stack[0]
            }
            num_stems += stack_top / 2;
            clear_stack(dest, limit);
            if limit < (*dest).offset(1) {
                status = -3;
                return;
            }
            **dest = op;
            *dest = (*dest).offset(1);
            phase = 1;
        }
        19 | 20 => {
            if phase < 2 {
                if phase == 0 && stack_top % 2 != 0 {
                    have_width = 1;
                    width = arg_stack[0]
                }
                num_stems += stack_top / 2
            }
            clear_stack(dest, limit);
            if limit < (*dest).offset(1) {
                status = -3;
                return;
            }
            **dest = op;
            *dest = (*dest).offset(1);
            if num_stems > 0 {
                let masklen: i32 = (num_stems + 7) / 8;
                if limit < (*dest).offset(masklen as isize) {
                    status = -3;
                    return;
                }
                if endptr < (*data).offset(masklen as isize) {
                    status = -1;
                    return;
                }
                memmove(
                    *dest as *mut libc::c_void,
                    *data as *const libc::c_void,
                    masklen as _,
                );
                *data = (*data).offset(masklen as isize);
                *dest = (*dest).offset(masklen as isize)
            }
            phase = 2;
        }
        21 => {
            if phase == 0 && stack_top % 2 != 0 {
                have_width = 1;
                width = arg_stack[0]
            }
            clear_stack(dest, limit);
            if limit < (*dest).offset(1) {
                status = -3;
                return;
            }
            **dest = op;
            *dest = (*dest).offset(1);
            phase = 2;
        }
        22 | 4 => {
            if phase == 0 && stack_top % 2 == 0 {
                have_width = 1;
                width = arg_stack[0];
            }
            clear_stack(dest, limit);
            if limit < (*dest).offset(1) {
                status = -3;
                return;
            }
            **dest = op;
            *dest = (*dest).offset(1);
            phase = 2;
        }
        14 => {
            if stack_top == 1 {
                have_width = 1;
                width = arg_stack[0];
                clear_stack(dest, limit);
            } else if stack_top == 4 || stack_top == 5 {
                warn!("\"seac\" character deprecated in Type 2 charstring.");
                status = -1;
                return;
            } else {
                if stack_top > 0 {
                    warn!("{}: Operand stack not empty.", "Type2 Charstring Parser");
                }
            }
            if limit < (*dest).offset(1) {
                status = -3;
                return;
            }
            **dest = op;
            *dest = (*dest).offset(1);
            status = 3;
        }
        5 | 6 | 7 | 8 | 24 | 25 | 26 | 27 | 30 | 31 => {
            /* above oprators are candidate for first stack-clearing operator */
            if phase < 2 {
                warn!("{}: Broken Type 2 charstring.", "Type2 Charstring Parser");
                status = -1;
                return;
            }
            clear_stack(dest, limit);
            if limit < (*dest).offset(1) {
                status = -3;
                return;
            }
            **dest = op;
            *dest = (*dest).offset(1);
        }
        11 | 29 | 10 => {
            /* all operotors above are stack-clearing operator */
            /* no output */
            panic!(
                "{}: Unexpected call(g)subr/return",
                "Type2 Charstring Parser",
            );
        }
        _ => {
            /* no-op ? */
            warn!(
                "{}: Unknown charstring operator: 0x{:02x}",
                "Type2 Charstring Parser", op,
            );
            status = -1
        }
    };
}
/*
 * Double byte operators:
 *  Flex, arithmetic, conditional, and storage operators.
 *
 * Following operators are not supported:
 *  random: How random ?
 */
unsafe fn do_operator2(
    dest: &mut *mut u8,
    limit: *mut u8,
    data: &mut *const u8,
    endptr: *const u8,
) {
    *data = (*data).offset(1);
    if endptr < (*data).offset(1) {
        status = -1;
        return;
    }
    let op = **data;
    *data = (*data).offset(1);
    match op as i32 {
        0 => {
            /* deprecated */
            warn!("Operator \"dotsection\" deprecated in Type 2 charstring.");
            status = -1;
            return;
        }
        34 | 35 | 36 | 37 => {
            if phase < 2 {
                warn!("{}: Broken Type 2 charstring.", "Type2 Charstring Parser");
                status = -1;
                return;
            }
            clear_stack(dest, limit);
            if limit < (*dest).offset(2) {
                status = -3;
                return;
            }
            **dest = 12;
            *dest = (*dest).offset(1);
            **dest = op;
            *dest = (*dest).offset(1);
        }
        3 => {
            /* all operator above are stack-clearing */
            /* no output */
            if stack_top < 2 {
                status = -2;
                return;
            }
            stack_top -= 1;
            if arg_stack[stack_top as usize] != 0. && arg_stack[(stack_top - 1) as usize] != 0. {
                arg_stack[(stack_top - 1) as usize] = 1.0f64
            } else {
                arg_stack[(stack_top - 1) as usize] = 0.0f64
            }
        }
        4 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            stack_top -= 1;
            if arg_stack[stack_top as usize] != 0. || arg_stack[(stack_top - 1) as usize] != 0. {
                arg_stack[(stack_top - 1) as usize] = 1.0f64
            } else {
                arg_stack[(stack_top - 1) as usize] = 0.0f64
            }
        }
        5 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            if arg_stack[(stack_top - 1) as usize] != 0. {
                arg_stack[(stack_top - 1) as usize] = 0.0f64
            } else {
                arg_stack[(stack_top - 1) as usize] = 1.0f64
            }
        }
        9 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 1) as usize] = (arg_stack[(stack_top - 1) as usize]).abs()
        }
        10 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 2) as usize] += arg_stack[(stack_top - 1) as usize];
            stack_top -= 1
        }
        11 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 2) as usize] -= arg_stack[(stack_top - 1) as usize];
            stack_top -= 1
        }
        12 => {
            /* doesn't check overflow */
            if stack_top < 2 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 2) as usize] /= arg_stack[(stack_top - 1) as usize];
            stack_top -= 1
        }
        14 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 1) as usize] *= -1.0f64
        }
        15 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            stack_top -= 1;
            if arg_stack[stack_top as usize] == arg_stack[(stack_top - 1) as usize] {
                arg_stack[(stack_top - 1) as usize] = 1.0f64
            } else {
                arg_stack[(stack_top - 1) as usize] = 0.0f64
            }
        }
        18 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            stack_top -= 1
        }
        20 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            stack_top -= 1;
            let idx: i32 = arg_stack[stack_top as usize] as i32;
            if 32 < idx {
                status = -2;
                return;
            }
            stack_top -= 1;
            trn_array[idx as usize] = arg_stack[stack_top as usize]
        }
        21 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            let idx_0: i32 = arg_stack[(stack_top - 1) as usize] as i32;
            if 32 < idx_0 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 1) as usize] = trn_array[idx_0 as usize]
        }
        22 => {
            if stack_top < 4 {
                status = -2;
                return;
            }
            stack_top -= 3;
            if arg_stack[(stack_top + 1) as usize] > arg_stack[(stack_top + 2) as usize] {
                arg_stack[(stack_top - 1) as usize] = arg_stack[stack_top as usize]
            }
        }
        24 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 2) as usize] =
                arg_stack[(stack_top - 2) as usize] * arg_stack[(stack_top - 1) as usize];
            stack_top -= 1
        }
        26 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            arg_stack[(stack_top - 1) as usize] = (arg_stack[(stack_top - 1) as usize]).sqrt()
        }
        27 => {
            if stack_top < 1 {
                status = -2;
                return;
            }
            if 48 < stack_top + 1 {
                status = -2;
                return;
            }
            arg_stack[stack_top as usize] = arg_stack[(stack_top - 1) as usize];
            stack_top += 1
        }
        28 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            let save: f64 = arg_stack[(stack_top - 2) as usize];
            arg_stack[(stack_top - 2) as usize] = arg_stack[(stack_top - 1) as usize];
            arg_stack[(stack_top - 1) as usize] = save
        }
        29 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            /* need two arguments at least */
            let idx_1: i32 = arg_stack[(stack_top - 1) as usize] as i32;
            if idx_1 < 0 {
                arg_stack[(stack_top - 1) as usize] = arg_stack[(stack_top - 2) as usize]
            } else {
                if stack_top < idx_1 + 2 {
                    status = -2;
                    return;
                }
                arg_stack[(stack_top - 1) as usize] = arg_stack[(stack_top - idx_1 - 2) as usize]
            }
        }
        30 => {
            if stack_top < 2 {
                status = -2;
                return;
            }
            stack_top -= 1;
            let J = arg_stack[stack_top as usize] as i32;
            stack_top -= 1;
            let N = arg_stack[stack_top as usize] as i32;
            if stack_top < N {
                status = -2;
                return;
            }
            if J > 0 {
                for _ in 0..(J % N) {
                    let save_0: f64 = arg_stack[(stack_top - 1) as usize];
                    let mut i: i32 = stack_top - 1;
                    while i > stack_top - N {
                        arg_stack[i as usize] = arg_stack[(i - 1) as usize];
                        i -= 1;
                    }
                    arg_stack[i as usize] = save_0
                }
            } else {
                for _ in 0..(-J % N) {
                    let save_1: f64 = arg_stack[(stack_top - N) as usize];
                    let mut i_0: i32 = stack_top - N;
                    while i_0 < stack_top - 1 {
                        arg_stack[i_0 as usize] = arg_stack[(i_0 + 1) as usize];
                        i_0 += 1;
                    }
                    arg_stack[i_0 as usize] = save_1;
                }
            }
        }
        23 => {
            warn!(
                "{}: Charstring operator \"random\" found.",
                "Type2 Charstring Parser"
            );
            if 48 < stack_top + 1 {
                status = -2;
                return;
            }
            arg_stack[stack_top as usize] = 1.;
            stack_top = stack_top + 1;
        }
        _ => {
            /* no-op ? */
            warn!(
                "{}: Unknown charstring operator: 0x0c{:02x}",
                "Type2 Charstring Parser", op,
            );
            status = -1
        }
    };
}
/*
 * integer:
 *  exactly the same as the DICT encoding (except 29)
 */
unsafe fn get_integer(data: &mut *const u8, endptr: *const u8) {
    let mut result;
    let b0: u8 = **data;
    *data = (*data).offset(1);
    if b0 as i32 == 28 {
        /* shortint */
        if endptr < (*data).offset(2) {
            status = -1;
            return;
        }
        let b1 = **data;
        let b2 = *(*data).offset(1);
        result = b1 as i32 * 256 + b2 as i32;
        if result > 0x7fff {
            result = (result as i64 - 0x10000) as i32
        }
        *data = (*data).offset(2)
    } else if b0 as i32 >= 32 && b0 as i32 <= 246 {
        /* int (1) */
        result = b0 as i32 - 139
    } else if b0 as i32 >= 247 && b0 as i32 <= 250 {
        /* int (2) */
        if endptr < (*data).offset(1) {
            status = -1;
            return;
        }
        let b1 = **data;
        result = (b0 as i32 - 247) * 256 + b1 as i32 + 108;
        *data = (*data).offset(1)
    } else if b0 as i32 >= 251 && b0 as i32 <= 254 {
        if endptr < (*data).offset(1) {
            status = -1;
            return;
        }
        let b1 = **data;
        result = -(b0 as i32 - 251) * 256 - b1 as i32 - 108;
        *data = (*data).offset(1)
    } else {
        status = -1;
        return;
    }
    if 48 < stack_top + 1 {
        status = -2;
        return;
    }
    arg_stack[stack_top as usize] = result as f64;
    stack_top += 1;
}
/*
 * Signed 16.16-bits fixed number for Type 2 charstring encoding
 */
unsafe fn get_fixed(data: &mut *const u8, endptr: *const u8) {
    *data = (*data).offset(1);
    if endptr < (*data).offset(4) {
        status = -1;
        return;
    }
    let ivalue = **data as i32 * 0x100 + *(*data).offset(1) as i32;
    let mut rvalue = (if ivalue as i64 > 0x7fff {
        ivalue as i64 - 0x10000
    } else {
        ivalue as i64
    }) as f64;
    let ivalue = *(*data).offset(2) as i32 * 0x100 + *(*data).offset(3) as i32;
    rvalue += ivalue as f64 / 0x10000i64 as f64;
    if 48 < stack_top + 1 {
        status = -2;
        return;
    }
    arg_stack[stack_top as usize] = rvalue;
    stack_top += 1;
    *data = (*data).offset(4);
}
/*
 * Subroutines:
 *  The bias for subroutine number is introduced in type 2 charstrings.
 *
 * subr:     set to a pointer to the subroutine charstring.
 * len:      set to the length of subroutine charstring.
 * subr_idx: CFF INDEX data that contains subroutines.
 * id:       biased subroutine number.
 */
unsafe fn get_subr(
    subr: &mut *const u8,
    len: *mut i32,
    subr_idx: &Option<Box<CffIndex>>,
    mut id: i32,
) {
    let subr_idx = subr_idx.as_ref().unwrap_or_else(|| {
        panic!(
            "{}: Subroutine called but no subroutine found.",
            "Type2 Charstring Parser",
        )
    });
    let count = subr_idx.count;
    /* Adding bias number */
    if (count as i32) < 1240 {
        id += 107
    } else if (count as i32) < 33900 {
        id += 1131
    } else {
        id += 32768
    }
    if id > count as i32 {
        panic!(
            "{}: Invalid Subr index: {} (max={})",
            "Type2 Charstring Parser", id, count,
        );
    }
    *len = (subr_idx.offset[(id + 1) as usize] - subr_idx.offset[id as usize]) as i32;
    *subr = subr_idx.data[subr_idx.offset[id as usize] as usize - 1..].as_ptr();
}
/*
 * NOTE:
 *  The Type 2 interpretation of a number encoded in five-bytes (those with
 *  an initial byte value of 255) differs from how it is interpreted in the
 *  Type 1 format.
 */
unsafe fn do_charstring(
    dest: &mut *mut u8,
    limit: *mut u8,
    data: &mut *const u8,
    endptr: *const u8,
    gsubr_idx: &Option<Box<CffIndex>>,
    subr_idx: &Option<Box<CffIndex>>,
) {
    let mut len: i32 = 0;
    if nest > 10 {
        panic!(
            "{}: Subroutine nested too deeply.",
            "Type2 Charstring Parser",
        );
    }
    nest += 1;
    while *data < endptr && status == 0 {
        let b0 = **data;
        if b0 as i32 == 255 {
            /* 16-bit.16-bit fixed signed number */
            get_fixed(data, endptr);
        } else if b0 as i32 == 11 {
            status = 2
        } else if b0 as i32 == 29 {
            if stack_top < 1 {
                status = -2
            } else {
                stack_top -= 1;
                let mut subr: *const u8 = ptr::null();
                get_subr(
                    &mut subr,
                    &mut len,
                    gsubr_idx,
                    arg_stack[stack_top as usize] as i32,
                );
                if (*dest).offset(len as isize) > limit {
                    panic!("{}: Possible buffer overflow.", "Type2 Charstring Parser");
                }
                let endptr = subr.offset(len as isize);
                do_charstring(dest, limit, &mut subr, endptr, gsubr_idx, subr_idx);
                *data = (*data).offset(1)
            }
        } else if b0 as i32 == 10 {
            if stack_top < 1 {
                status = -2
            } else {
                stack_top -= 1;
                let mut subr: *const u8 = ptr::null();
                get_subr(
                    &mut subr,
                    &mut len,
                    subr_idx,
                    arg_stack[stack_top as usize] as i32,
                );
                if limit < (*dest).offset(len as isize) {
                    panic!("{}: Possible buffer overflow.", "Type2 Charstring Parser");
                }
                let endptr = subr.offset(len as isize);
                do_charstring(dest, limit, &mut subr, endptr, gsubr_idx, subr_idx);
                *data = (*data).offset(1)
            }
        } else if b0 as i32 == 12 {
            do_operator2(dest, limit, data, endptr);
        } else if (b0 as i32) < 32 && b0 as i32 != 28 {
            /* 19, 20 need mask */
            do_operator1(dest, limit, data, endptr);
        } else if b0 as i32 >= 22 && b0 as i32 <= 27 || b0 as i32 == 31 {
            /* reserved */
            status = -1
        /* not an error ? */
        } else {
            get_integer(data, endptr);
        }
    }
    if status == 2 {
        status = 0
    } else if status == 3 && *data < endptr {
        warn!("{}: Garbage after endchar.", "Type2 Charstring Parser");
    } else if status < 0 {
        /* error */
        panic!(
            "{}: Parsing charstring failed: (status={}, stack={})",
            "Type2 Charstring Parser", status, stack_top,
        );
    }
    nest -= 1;
}
unsafe fn cs_parse_init() {
    status = 0;
    nest = 0;
    phase = 0;
    num_stems = 0;
    stack_top = 0;
}
/* unused in Type 2 charstring */
/* unused in Type 2 charstring */
/*
 * Not just copying...
 */

pub(crate) unsafe fn cs_copy_charstring(
    mut dst: *mut u8,
    dstlen: i32,
    mut src: *const u8,
    srclen: i32,
    gsubr: &Option<Box<CffIndex>>,
    subr: &Option<Box<CffIndex>>,
    default_width: f64,
    nominal_width: f64,
    mut ginfo: *mut cs_ginfo,
) -> i32 {
    let save: *mut u8 = dst;
    cs_parse_init();
    width = 0.0f64;
    have_width = 0;
    /* expand call(g)subrs */
    let dstend = dst.offset(dstlen as isize);
    let srcend = src.offset(srclen as isize);
    do_charstring(&mut dst, dstend, &mut src, srcend, gsubr, subr); /* not used */
    if !ginfo.is_null() {
        (*ginfo).flags = 0;
        if have_width != 0 {
            (*ginfo).wx = nominal_width + width
        } else {
            (*ginfo).wx = default_width
        }
    }
    dst.offset_from(save) as i64 as i32
}
