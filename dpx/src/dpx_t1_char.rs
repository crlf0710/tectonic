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

use super::dpx_mem::new;
use crate::mfree;
use crate::warn;
use libc::{free, memcpy, memset};
use std::cmp::Ordering;
use std::ptr;

use super::dpx_cff::CffIndex;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct t1_ginfo {
    pub(crate) use_seac: i32,
    pub(crate) wx: f64,
    pub(crate) wy: f64,
    pub(crate) bbox: C2RustUnnamed_0,
    pub(crate) seac: C2RustUnnamed,
}
impl t1_ginfo {
    pub(crate) fn new() -> Self {
        Self {
            use_seac: 0,
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
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct t1_chardesc {
    pub(crate) flags: i32,
    pub(crate) sbw: C2RustUnnamed_3,
    pub(crate) bbox: C2RustUnnamed_2,
    pub(crate) seac: C2RustUnnamed_1,
    pub(crate) num_stems: i32,
    pub(crate) stems: [t1_stem; 96],
    pub(crate) charpath: *mut t1_cpath,
    pub(crate) lastpath: *mut t1_cpath,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct t1_cpath {
    pub(crate) type_0: i32,
    pub(crate) num_args: i32,
    pub(crate) args: [f64; 48],
    pub(crate) next: *mut t1_cpath,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct t1_stem {
    pub(crate) id: i32,
    pub(crate) dir: i32,
    pub(crate) pos: f64,
    pub(crate) del: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_1 {
    pub(crate) asb: f64,
    pub(crate) adx: f64,
    pub(crate) ady: f64,
    pub(crate) bchar: u8,
    pub(crate) achar: u8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_2 {
    pub(crate) llx: f64,
    pub(crate) lly: f64,
    pub(crate) urx: f64,
    pub(crate) ury: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_3 {
    pub(crate) sbx: f64,
    pub(crate) sby: f64,
    pub(crate) wx: f64,
    pub(crate) wy: f64,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct t1_stemgroup {
    pub(crate) num_stems: i32,
    pub(crate) stems: [f64; 96],
}

static mut status: i32 = -1;
static mut phase: i32 = -1;
static mut nest: i32 = -1;
static mut cs_stack_top: i32 = 0;
static mut ps_stack_top: i32 = 0;
/* [vh]stem support require one more stack size. */
static mut cs_arg_stack: [f64; 49] = [0.; 49];
static mut ps_arg_stack: [f64; 194] = [0.; 194];
/*
 * Stem:
 *
 *   1. Stems must be sorted in the increasing bottom/left edge order.
 *   2. The encoded values are all relative; The value x(y) of the first
 *      stem is relative to 0 for Type 2 charstring and is relative to
 *      the left(bottom) side-bearing for Type 1 charstring.
 *   3. A width of -20(-21) specifies the top/right(bottom/left) edge
 *      of an edge hint in Type 2 charstring. But the width of 'ghost'
 *      hint in Type 1 charstring is positive with value 20 or 21.
 *   4. The h(v)stemhm MUST be used instead of h(v)stem if charstring
 *      contains hintmask operator.
 *
 * TODO:
 *
 *  Convert ghost hint to edge hint, Counter control for hstem3/vstem3.
 */
#[inline]
fn stem_compare(s1: &t1_stem, s2: &t1_stem) -> Ordering {
    // the order of comparing : dir, pos, del
    if s1.dir == s2.dir {
        s1.pos
            .partial_cmp(&s2.pos)
            .unwrap()
            .then_with(|| s1.del.partial_cmp(&s2.del).unwrap())
    } else if s1.dir == 0 {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}
unsafe fn get_stem(cd: *mut t1_chardesc, stem_id: i32) -> i32 {
    let mut i = 0;
    while i < (*cd).num_stems {
        if (*cd).stems[i as usize].id == stem_id {
            break;
        }
        i += 1
    }
    if i < (*cd).num_stems {
        i
    } else {
        -1
    }
}
unsafe fn add_stem(mut cd: *mut t1_chardesc, mut pos: f64, del: f64, dir: i32) -> i32 {
    assert!(!cd.is_null());
    pos += if dir == 0 {
        (*cd).sbw.sby
    } else {
        (*cd).sbw.sbx
    };
    let mut i = 0;
    while i < (*cd).num_stems as usize {
        if (*cd).stems[i].dir == dir && (*cd).stems[i].pos == pos && (*cd).stems[i].del == del {
            break;
        }
        i += 1
    }
    if i == (*cd).num_stems as usize {
        if (*cd).num_stems == 96 {
            return -1;
        }
        (*cd).stems[i].dir = dir;
        (*cd).stems[i].pos = pos;
        (*cd).stems[i].del = del;
        (*cd).stems[i].id = (*cd).num_stems;
        (*cd).num_stems += 1
    }
    (*cd).stems[i].id
}
unsafe fn copy_args(mut args1: *mut f64, mut args2: *mut f64, count: i32) {
    for _ in 0..count {
        *args1 = *args2;
        args1 = args1.offset(1);
        args2 = args2.offset(1)
    }
}
/*
 * Stack:
 */
/*
 * Path construction:
 */
/* Get operands from cs_arg_stack[] */
unsafe fn add_charpath(mut cd: *mut t1_chardesc, type_0: i32, argv: *mut f64, argn: i32) {
    assert!(!cd.is_null());
    assert!(argn <= 48);
    let mut p =
        new((1_u64).wrapping_mul(::std::mem::size_of::<t1_cpath>() as u64) as u32) as *mut t1_cpath;
    (*p).type_0 = type_0;
    (*p).num_args = argn;
    (*p).next = ptr::null_mut();
    for i in (0..argn).rev() {
        (*p).args[i as usize] = *argv.offset(i as isize)
    }
    if (*cd).charpath.is_null() {
        (*cd).charpath = p
    }
    if !(*cd).lastpath.is_null() {
        (*(*cd).lastpath).next = p
    }
    (*cd).lastpath = p;
    if type_0 >= 0
        && phase != 3
        && (type_0 >= 4 && type_0 <= 9
            || type_0 >= 21 && type_0 <= 31 && type_0 != 23 && type_0 != 29 && type_0 != 28)
    {
        phase = 2
    };
}
unsafe fn init_charpath(mut cd: *mut t1_chardesc) {
    (*cd).flags = 0;
    (*cd).num_stems = 0;
    (*cd).sbw.wy = 0.0f64;
    (*cd).sbw.wx = (*cd).sbw.wy;
    (*cd).sbw.sby = 0.0f64;
    (*cd).sbw.sbx = (*cd).sbw.sby;
    (*cd).bbox.ury = 0.0f64;
    (*cd).bbox.urx = (*cd).bbox.ury;
    (*cd).bbox.lly = (*cd).bbox.urx;
    (*cd).bbox.llx = (*cd).bbox.lly;
    (*cd).lastpath = ptr::null_mut();
    (*cd).charpath = (*cd).lastpath;
}
unsafe fn release_charpath(mut cd: *mut t1_chardesc) {
    assert!(!cd.is_null());
    let mut curr = (*cd).charpath;
    while !curr.is_null() {
        let next = (*curr).next;
        free(curr as *mut libc::c_void);
        curr = next
    }
    (*cd).lastpath = ptr::null_mut();
    (*cd).charpath = (*cd).lastpath;
}
/*
 * Type 1 charstring operators:
 */
/*
 * Single byte operators:
 */
unsafe fn do_operator1(mut cd: *mut t1_chardesc, data: &mut *const u8) {
    let mut op: u8 = **data;
    *data = (*data).offset(1);
    match op as i32 {
        9 => {
            /*
             * From T1 spec.:
             *  Note that, unlike the closepath command in the PostScript language,
             *  this command does not reposition the current point. Any subsequent
             *  rmoveto must be relative to the current point in force before the
             *  Type 1 font format closepath command was given.
             */
            /* noop */
            cs_stack_top = 0;
        }
        13 => {
            if cs_stack_top < 2 {
                status = -2;
                return;
            }
            cs_stack_top -= 1;
            (*cd).sbw.wx = cs_arg_stack[cs_stack_top as usize];
            (*cd).sbw.wy = 0.;
            cs_stack_top -= 1;
            (*cd).sbw.sbx = cs_arg_stack[cs_stack_top as usize];
            (*cd).sbw.sby = 0.;
            cs_stack_top = 0;
        }
        1 | 3 => {
            if cs_stack_top < 2 {
                status = -2;
                return;
            }
            let stem_id = add_stem(
                cd,
                cs_arg_stack[(cs_stack_top - 2) as usize],
                cs_arg_stack[(cs_stack_top - 1) as usize],
                if op as i32 == 1 { 0 } else { 1 },
            );
            if stem_id < 0 {
                warn!("Too many hints...");
                status = -1;
                return;
            }
            /* Put stem_id onto the stack... */
            cs_arg_stack[cs_stack_top as usize] = stem_id as f64;
            cs_stack_top += 1;
            add_charpath(
                cd,
                -1,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - 1) as isize),
                1,
            );
            cs_stack_top = 0
        }
        21 => {
            /*
             * Reference point is (0, 0) in Type 2 charstring.
             */
            if cs_stack_top < 2 {
                status = -2;
                return;
            }
            if phase < 2 {
                cs_arg_stack[(cs_stack_top - 2) as usize] += (*cd).sbw.sbx;
                cs_arg_stack[(cs_stack_top - 1) as usize] += (*cd).sbw.sby
            }
            add_charpath(
                cd,
                op as i32,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - 2) as isize),
                2,
            );
            cs_stack_top = 0
        }
        22 | 4 => {
            if cs_stack_top < 1 {
                status = -2;
                return;
            }
            let mut argn: i32 = 1;
            if phase < 2 {
                /*
                 * The reference point for the first moveto operator is diferrent
                 * between Type 1 charstring and Type 2 charstring. We compensate it.
                 */
                if op as i32 == 22 {
                    cs_arg_stack[(cs_stack_top - 1) as usize] += (*cd).sbw.sbx;
                    if (*cd).sbw.sby != 0. {
                        cs_arg_stack[cs_stack_top as usize] = (*cd).sbw.sby;
                        cs_stack_top += 1;
                        argn = 2;
                        op = 21;
                    }
                } else {
                    cs_arg_stack[(cs_stack_top - 1) as usize] += (*cd).sbw.sby;
                    if (*cd).sbw.sbx != 0. {
                        cs_arg_stack[cs_stack_top as usize] =
                            cs_arg_stack[(cs_stack_top - 1) as usize];
                        cs_arg_stack[(cs_stack_top - 1) as usize] = (*cd).sbw.sbx;
                        cs_stack_top += 1;
                        argn = 2;
                        op = 21;
                    }
                }
            }
            add_charpath(
                cd,
                op as i32,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - argn) as isize),
                argn,
            );
            cs_stack_top = 0
        }
        14 => {
            status = 3;
            cs_stack_top = 0
        }
        5 => {
            /* above oprators are candidate for first stack-clearing operator */
            if cs_stack_top < 2 {
                status = -2;
                return;
            }
            add_charpath(
                cd,
                op as i32,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - 2) as isize),
                2,
            );
            cs_stack_top = 0
        }
        6 | 7 => {
            if cs_stack_top < 1 {
                status = -2;
                return;
            }
            add_charpath(
                cd,
                op as i32,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - 1) as isize),
                1,
            );
            cs_stack_top = 0
        }
        8 => {
            if cs_stack_top < 6 {
                status = -2;
                return;
            }
            add_charpath(
                cd,
                op as i32,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - 6) as isize),
                6,
            );
            cs_stack_top = 0
        }
        30 | 31 => {
            if cs_stack_top < 4 {
                status = -2;
                return;
            }
            add_charpath(
                cd,
                op as i32,
                &mut *cs_arg_stack
                    .as_mut_ptr()
                    .offset((cs_stack_top - 4) as isize),
                4,
            );
            cs_stack_top = 0
        }
        11 => {}
        10 => {
            panic!("Unexpected callsubr.");
        }
        _ => {
            /* no-op ? */
            warn!("Unknown charstring operator: 0x{:02x}", op as i32);
            status = -1
        }
    };
}
/*
 * OtherSubrs:
 *
 *  arg0 arg1 ... argn n othersubr# callothersubr
 *
 *   0: Build flex:
 *      fd x y 3 0 callothersubr
 *      Othersubr #0 pushes x and y to PostScript interpreter operand stack.
 *   1: Start flex
 *      0 1 callothersubr
 *   2: Mark flex control points
 *      0 2 callothersubr
 *   3: Discard hint
 *      subr# 1 3 callothersubr pop callsubr
 *      Subroutine subr# (only) contains stem declaration.
 *      Othersubr #3 pushes subr# to PostScript interpreter operand stack.
 *  12: Counter control
 *      A subr to avoid stack overflow.
 *  13: Counter control
 */
/*
 * Convert six control points marked as CS_FLEX_CTRL to a flex path.
 */
unsafe fn do_othersubr0(mut cd: *mut t1_chardesc) {
    if ps_stack_top < 1 {
        status = -1;
        return;
    }
    /* Seek first CS_FLEX_CTRL mark */
    let mut cur = (*cd).charpath;
    while !cur.is_null() && (*cur).type_0 != -2 {
        cur = (*cur).next
    }
    let flex = cur;
    cur = (*cur).next;
    for i in 1..7 {
        if cur.is_null() || (*cur).type_0 != -2 || (*cur).num_args != 2 {
            status = -1;
            return;
        }
        if i == 1_u32 {
            (*flex).args[0] += (*cur).args[0];
            (*flex).args[1] += (*cur).args[1]
        } else {
            copy_args(
                &mut *(*flex)
                    .args
                    .as_mut_ptr()
                    .offset((2_u32).wrapping_mul(i).wrapping_sub(2_u32) as isize),
                (*cur).args.as_mut_ptr(),
                2,
            );
        }
        let next = (*cur).next;
        free(cur as *mut libc::c_void);
        cur = next;
    }
    if !cur.is_null() {
        status = -1;
        return;
    }
    /*
     * Now 'flex' have all six control points, the first pair is relative
     * from starting point.
     */
    (*flex).type_0 = 35; /* flex depth */
    ps_stack_top -= 1;
    (*flex).args[12] = ps_arg_stack[ps_stack_top as usize];
    (*flex).num_args = 13;
    (*flex).next = ptr::null_mut();
    (*cd).lastpath = flex;
    phase = 2;
}
/* Start flex */
unsafe fn do_othersubr1() {
    phase = 3;
}
/* Mark flex control point */
unsafe fn do_othersubr2(mut cd: *mut t1_chardesc) {
    if phase != 3 || (*cd).lastpath.is_null() {
        status = -1;
        return;
    }
    match (*(*cd).lastpath).type_0 {
        21 => {}
        22 => {
            (*(*cd).lastpath).num_args = 2;
            (*(*cd).lastpath).args[1] = 0.0f64
        }
        4 => {
            (*(*cd).lastpath).num_args = 2;
            (*(*cd).lastpath).args[1] = (*(*cd).lastpath).args[0];
            (*(*cd).lastpath).args[0] = 0.0f64
        }
        _ => {
            status = -1;
            return;
        }
    }
    (*(*cd).lastpath).type_0 = -2;
}
/*
 * Hint Replacement:
 *  "Adobe Type 1 Font Format", Chapter 8.
 */
unsafe fn do_othersubr3(mut cd: *mut t1_chardesc) {
    (*cd).flags |= 1 << 0;
}
unsafe fn do_othersubr12() {
    /* Othersubr12 call must immediately follow the hsbw or sbw. */
    if phase != 0 {
        status = -1;
        return;
    };
    /* noop */
}
unsafe fn do_othersubr13(mut cd: *mut t1_chardesc) {
    let mut stemgroups: [t1_stemgroup; 96] = [t1_stemgroup {
        num_stems: 0,
        stems: [0.; 96],
    }; 96];
    /* After #12 callothersubr or hsbw or sbw. */
    if phase != 0 {
        status = -1;
        return;
    }
    for n in 0..96 {
        stemgroups[n as usize].num_stems = 0;
    }
    ps_stack_top -= 1;
    let num_hgroups = ps_arg_stack[ps_stack_top as usize] as i32;
    if num_hgroups < 0 || num_hgroups > 96 {
        status = -1;
        return;
    }
    let mut n = 0;
    let mut pos = 0.;
    while ps_stack_top >= 2 && n < num_hgroups {
        /* add_stem() add sidebearing */
        ps_stack_top -= 1;
        pos += ps_arg_stack[ps_stack_top as usize];
        ps_stack_top -= 1;
        let del = ps_arg_stack[ps_stack_top as usize];
        let stem_id = add_stem(
            cd,
            if del < 0.0f64 { pos + del } else { pos },
            if del < 0.0f64 { -del } else { del },
            0,
        );
        stemgroups[n as usize].stems[stemgroups[n as usize].num_stems as usize] = stem_id as f64;
        stemgroups[n as usize].num_stems += 1;
        pos += del;
        if del < 0.0f64 {
            pos = 0.0f64;
            n += 1
        }
    }
    if n != num_hgroups {
        status = -2;
        return;
    }
    ps_stack_top -= 1;
    let num_vgroups = ps_arg_stack[ps_stack_top as usize] as i32;
    if num_vgroups < 0 || num_vgroups > 96 {
        status = -1;
        return;
    }
    let mut n = 0;
    let mut pos = 0.;
    while ps_stack_top >= 2 && n < num_vgroups {
        /* add_stem() add sidebearing */
        ps_stack_top -= 1;
        pos += ps_arg_stack[ps_stack_top as usize];
        ps_stack_top -= 1;
        let del = ps_arg_stack[ps_stack_top as usize];
        let stem_id = add_stem(
            cd,
            if del < 0.0f64 { pos + del } else { pos },
            if del < 0.0f64 { -del } else { del },
            1,
        );
        stemgroups[n as usize].stems[stemgroups[n as usize].num_stems as usize] = stem_id as f64;
        stemgroups[n as usize].num_stems += 1;
        pos += del;
        if del < 0.0f64 {
            pos = 0.0f64;
            n += 1
        }
    }
    if n != num_vgroups {
        status = -2;
        return;
    }
    for n in 0..(if num_hgroups > num_vgroups {
        num_hgroups
    } else {
        num_vgroups
    }) {
        add_charpath(
            cd,
            20,
            stemgroups[n as usize].stems.as_mut_ptr(),
            stemgroups[n as usize].num_stems,
        );
    }
    (*cd).flags |= 1 << 1;
}
unsafe fn do_callothersubr(cd: *mut t1_chardesc) {
    if cs_stack_top < 2 {
        status = -2;
        return;
    }
    cs_stack_top -= 1;
    let subrno = cs_arg_stack[cs_stack_top as usize] as i32;
    cs_stack_top -= 1;
    let argn = cs_arg_stack[cs_stack_top as usize] as i32;
    if cs_stack_top < argn {
        status = -2;
        return;
    }
    if ps_stack_top + argn > 96 * 2 + 2 {
        status = -1;
        return;
    }
    for _ in 0..argn {
        cs_stack_top -= 1;
        ps_arg_stack[ps_stack_top as usize] = cs_arg_stack[cs_stack_top as usize];
        ps_stack_top += 1;
    }
    match subrno {
        0 => {
            do_othersubr0(cd);
        }
        1 => {
            do_othersubr1();
        }
        2 => {
            do_othersubr2(cd);
        }
        3 => {
            do_othersubr3(cd);
        }
        12 => {
            do_othersubr12();
        }
        13 => {
            do_othersubr13(cd);
        }
        _ => {
            panic!("Unknown othersubr #{}.", subrno);
        }
    };
}
/*
 * Double byte operators:
 */
unsafe fn do_operator2(mut cd: *mut t1_chardesc, data: &mut *const u8, endptr: *const u8) {
    *data = (*data).offset(1);
    if endptr < (*data).offset(1) {
        status = -1;
        return;
    }
    let op = **data;
    *data = (*data).offset(1);
    match op as i32 {
        7 => {
            if cs_stack_top < 4 {
                status = -2;
                return;
            }
            cs_stack_top -= 1;
            (*cd).sbw.wy = cs_arg_stack[cs_stack_top as usize];
            cs_stack_top -= 1;
            (*cd).sbw.wx = cs_arg_stack[cs_stack_top as usize];
            cs_stack_top -= 1;
            (*cd).sbw.sby = cs_arg_stack[cs_stack_top as usize];
            cs_stack_top -= 1;
            (*cd).sbw.sbx = cs_arg_stack[cs_stack_top as usize];
            cs_stack_top = 0
        }
        2 | 1 => {
            /*
             * TODO:
             *  The counter control can be used for hstem3 and vstem3
             *  operator if LanguageGroup is not equal to 1.
             */
            if cs_stack_top < 6 {
                status = -2;
                return;
            }
            let mut i = 2;
            while i >= 0 {
                let stem_id = add_stem(
                    cd,
                    cs_arg_stack[(cs_stack_top - 2 * i - 2) as usize],
                    cs_arg_stack[(cs_stack_top - 2 * i - 1) as usize],
                    if op as i32 == 2 { 0 } else { 1 },
                );
                if stem_id < 0 {
                    warn!("Too many hints...");
                    status = -1;
                    return;
                }
                /* Put stem_id onto the stack... */
                cs_arg_stack[cs_stack_top as usize] = stem_id as f64;
                cs_stack_top += 1;
                add_charpath(
                    cd,
                    -1,
                    &mut *cs_arg_stack
                        .as_mut_ptr()
                        .offset((cs_stack_top - 1) as isize),
                    1,
                );
                cs_stack_top -= 1;
                i -= 1
            }
            cs_stack_top = 0;
        }
        33 => {
            if cs_stack_top < 2 {
                status = -2;
                return;
            }
            /* noop */
            cs_stack_top = 0
        }
        17 => {
            /* all operator above are stack-clearing */
            /*
             * Transfer a operand from PS interpreter operand stack to BuildChar
             * operand stack.
             */
            if ps_stack_top < 1 {
                status = -1;
                return;
            }
            if cs_stack_top + 1 > 48 {
                status = -2;
                return;
            }
            ps_stack_top -= 1;
            cs_arg_stack[cs_stack_top as usize] = ps_arg_stack[ps_stack_top as usize];
            cs_stack_top += 1;
        }
        0 => {}
        12 => {
            /* TODO: check overflow */
            if cs_stack_top < 2 {
                status = -2;
                return;
            }
            cs_arg_stack[(cs_stack_top - 2) as usize] /= cs_arg_stack[(cs_stack_top - 1) as usize];
            cs_stack_top -= 1
        }
        16 => {
            do_callothersubr(cd);
        }
        6 => {
            if cs_stack_top < 5 {
                status = -2;
                return;
            }
            (*cd).flags |= 1 << 2;
            cs_stack_top -= 1;
            (*cd).seac.achar = cs_arg_stack[cs_stack_top as usize] as u8;
            cs_stack_top -= 1;
            (*cd).seac.bchar = cs_arg_stack[cs_stack_top as usize] as u8;
            cs_stack_top -= 1;
            (*cd).seac.ady = cs_arg_stack[cs_stack_top as usize];
            cs_stack_top -= 1;
            (*cd).seac.adx = cs_arg_stack[cs_stack_top as usize];
            /* We must compensate the difference of the glyph origin. */
            (*cd).seac.ady += (*cd).sbw.sby;
            cs_stack_top -= 1;
            (*cd).seac.adx += (*cd).sbw.sbx - cs_arg_stack[cs_stack_top as usize];
            cs_stack_top = 0;
        }
        _ => {
            /* no-op ? */
            warn!("Unknown charstring operator: 0x0c{:02x}", op as i32);
            status = -1;
        }
    };
}
/*
 * Charstring encoding:
 *  Copied from cs_type2.c
 *  Note:
 *   The Type 2 interpretation of a number encoded in five-bytes (those with
 *   an initial byte value of 255) differs from how it is interpreted in the
 *   Type 1 format.
 */
/* Type 2 5-bytes encoding used. */
unsafe fn put_numbers(argv: *mut f64, argn: i32, dest: &mut *mut u8, limit: *mut u8) {
    for i in 0..argn {
        let value = *argv.offset(i as isize);
        /* Nearest integer value */
        let mut ivalue = (value + 0.5).floor() as i32;
        if value >= 0x8000i64 as f64 || value <= (-0x8000 - 1 as i64) as f64 {
            /*
             * This number cannot be represented as a single operand.
             * We must use `a b mul ...' or `a c div' to represent large values.
             */
            panic!("Argument value too large. (This is bug)");
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
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1); /* Shouldn't come here */
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
                ivalue = ((value - ivalue as f64) * 0x10000 as f64) as i32;
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
                ivalue = 0xfb00u32.wrapping_sub(ivalue as u32).wrapping_sub(108) as i32;
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1);
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
            } else if ivalue >= -32768 && ivalue <= 32767 {
                /* shortint */
                if limit < (*dest).offset(3) {
                    status = -3;
                    return;
                }
                **dest = 28;
                *dest = (*dest).offset(1);
                **dest = (ivalue >> 8 & 0xff) as u8;
                *dest = (*dest).offset(1);
                **dest = (ivalue & 0xff) as u8;
                *dest = (*dest).offset(1);
            } else {
                panic!("Unexpected error.");
            }
        }
    }
}
unsafe fn get_integer(data: &mut *const u8, endptr: *const u8) {
    let mut result;
    let b0: u8 = **data;
    let b1;
    *data = (*data).offset(1);
    if b0 as i32 == 28 {
        /* shortint */
        if endptr < (*data).offset(2) {
            status = -1;
            return;
        }
        b1 = **data;
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
        b1 = **data;
        result = (b0 as i32 - 247) * 256 + b1 as i32 + 108;
        *data = (*data).offset(1)
    } else if b0 as i32 >= 251 && b0 as i32 <= 254 {
        if endptr < (*data).offset(1) {
            status = -1;
            return;
        }
        b1 = **data;
        result = -(b0 as i32 - 251) * 256 - b1 as i32 - 108;
        *data = (*data).offset(1)
    } else {
        status = -1;
        return;
    }
    if cs_stack_top + 1 > 48 {
        status = -2;
        return;
    }
    cs_arg_stack[cs_stack_top as usize] = result as f64;
    cs_stack_top += 1;
}
/* Type 1 */
unsafe fn get_longint(data: &mut *const u8, endptr: *const u8) {
    *data = (*data).offset(1);
    if endptr < (*data).offset(4) {
        status = -1;
        return;
    }
    let mut result = **data as i32;
    if result as i64 >= 0x80 {
        result = (result as i64 - 0x100) as i32
    }
    *data = (*data).offset(1);
    for _ in 1..4 {
        result = result * 256 + **data as i32;
        *data = (*data).offset(1);
    }
    if cs_stack_top + 1 > 48 {
        status = -2;
        return;
    }
    cs_arg_stack[cs_stack_top as usize] = result as f64;
    cs_stack_top += 1;
}
/*
 * TODO:
 *  Check "seac"
 *   We cannot do backword parsing due to subroutine, div etc.
 */
/* Parse charstring and build charpath. */
unsafe fn t1char_build_charpath(
    cd: *mut t1_chardesc,
    data: &mut *const u8,
    endptr: *const u8,
    subrs: &Option<Box<CffIndex>>,
) {
    if nest > 10 {
        panic!("Subroutine nested too deeply.");
    }
    nest += 1;
    while *data < endptr && status == 0 {
        let b0 = **data;
        if b0 as i32 == 255 {
            get_longint(data, endptr);
        /* Type 1 */
        } else if b0 as i32 == 11 {
            status = 2
        } else if b0 as i32 == 10 {
            if cs_stack_top < 1 {
                status = -2
            } else {
                cs_stack_top -= 1;
                let idx = cs_arg_stack[cs_stack_top as usize] as i32;
                match subrs.as_ref() {
                    Some(subrs0) if idx < subrs0.count as i32 => {
                        let mut subr =
                            subrs0.data[subrs0.offset[idx as usize] as usize - 1..].as_ptr();
                        let len = (subrs0.offset[(idx + 1) as usize] - subrs0.offset[idx as usize])
                            as i32;
                        let endptr = subr.offset(len as isize);
                        t1char_build_charpath(cd, &mut subr, endptr, subrs);
                        *data = (*data).offset(1);
                    }
                    _ => panic!("Invalid Subr#."),
                }
            }
        } else if b0 as i32 == 12 {
            do_operator2(cd, data, endptr);
        } else if (b0 as i32) < 32 && b0 as i32 != 28 {
            /* 19, 20 need mask */
            do_operator1(cd, data);
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
        if !(*data == endptr.offset(-1) && **data as i32 == 11) {
            warn!(
                "Garbage after endchar. ({} bytes)",
                endptr.offset_from(*data) as i64 as i32
            );
        }
    } else if status < 0 {
        /* error */
        panic!(
            "Parsing charstring failed: (status={}, stack={})",
            status, cs_stack_top
        );
    }
    nest -= 1;
}
/*
 * Calculate BoundingBox and compress path.
 *  The essentials of PDF size reduction is not Type 2 charstring compression
 *  but Type 1 charstring encryption. Encryption makes lossless compression
 *  useless. We will only do very simple charstring compression.
 */
unsafe fn do_postproc(mut cd: *mut t1_chardesc) {
    let mut next;
    if (*cd).charpath.is_null() {
        return;
    }
    /* Set dummy large value. */
    (*cd).bbox.lly = 100000.0f64;
    (*cd).bbox.llx = (*cd).bbox.lly;
    (*cd).bbox.ury = -100000.0f64;
    (*cd).bbox.urx = (*cd).bbox.ury;
    let mut cur = (*cd).charpath;
    let mut prev = ptr::null_mut::<t1_cpath>();
    let mut y = 0.0f64;
    let mut x = y;
    while !cur.is_null() {
        next = (*cur).next;
        match (*cur).type_0 {
            21 => {
                x += (*cur).args[0];
                y += (*cur).args[1];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
            }
            5 => {
                x += (*cur).args[0];
                y += (*cur).args[1];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                if !prev.is_null() && !cur.is_null() && (*prev).num_args + (*cur).num_args < 48 {
                    if (*prev).type_0 == 5 {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    } else if (*prev).type_0 == 8 {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).type_0 = 24;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    }
                }
            }
            22 => {
                x += (*cur).args[0];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
            }
            6 => {
                x += (*cur).args[0];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                if !prev.is_null() && !cur.is_null() && (*prev).num_args + (*cur).num_args < 48 {
                    if (*prev).type_0 == 7 && (*prev).num_args % 2 == 1
                        || (*prev).type_0 == 6 && (*prev).num_args % 2 == 0
                    {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    }
                }
            }
            4 => {
                y += (*cur).args[0];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
            }
            7 => {
                y += (*cur).args[0];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                if !prev.is_null() && !cur.is_null() && (*prev).num_args + (*cur).num_args < 48 {
                    if (*prev).type_0 == 6 && (*prev).num_args % 2 == 1
                        || (*prev).type_0 == 7 && (*prev).num_args % 2 == 0
                    {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    }
                }
            }
            8 => {
                for i in 0..3 {
                    x += (*cur).args[(2_u32).wrapping_mul(i) as usize];
                    y += (*cur).args[(2_u32).wrapping_mul(i).wrapping_add(1_u32) as usize];
                    if (*cd).bbox.llx > x {
                        (*cd).bbox.llx = x
                    }
                    if (*cd).bbox.urx < x {
                        (*cd).bbox.urx = x
                    }
                    if (*cd).bbox.lly > y {
                        (*cd).bbox.lly = y
                    }
                    if (*cd).bbox.ury < y {
                        (*cd).bbox.ury = y
                    }
                }
                if !prev.is_null() && !cur.is_null() && (*prev).num_args + (*cur).num_args < 48 {
                    if (*prev).type_0 == 8 {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    } else if (*prev).type_0 == 5 {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).type_0 = 25;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    }
                }
            }
            30 => {
                y += (*cur).args[0];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                x += (*cur).args[1];
                y += (*cur).args[2];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                x += (*cur).args[3];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                if !prev.is_null() && !cur.is_null() && (*prev).num_args + (*cur).num_args < 48 {
                    if (*prev).type_0 == 31 && (*prev).num_args / 4 % 2 == 1
                        || (*prev).type_0 == 30 && (*prev).num_args / 4 % 2 == 0
                    {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    }
                }
            }
            31 => {
                x += (*cur).args[0];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                x += (*cur).args[1];
                y += (*cur).args[2];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                y += (*cur).args[3];
                if (*cd).bbox.llx > x {
                    (*cd).bbox.llx = x
                }
                if (*cd).bbox.urx < x {
                    (*cd).bbox.urx = x
                }
                if (*cd).bbox.lly > y {
                    (*cd).bbox.lly = y
                }
                if (*cd).bbox.ury < y {
                    (*cd).bbox.ury = y
                }
                if !prev.is_null() && !cur.is_null() && (*prev).num_args + (*cur).num_args < 48 {
                    if (*prev).type_0 == 30 && (*prev).num_args / 4 % 2 == 1
                        || (*prev).type_0 == 31 && (*prev).num_args / 4 % 2 == 0
                    {
                        copy_args(
                            (*prev).args.as_mut_ptr().offset((*prev).num_args as isize),
                            (*cur).args.as_mut_ptr(),
                            (*cur).num_args,
                        );
                        (*prev).num_args += (*cur).num_args;
                        (*prev).next = next;
                        cur = mfree(cur as *mut libc::c_void) as *mut t1_cpath
                    }
                }
            }
            35 => {
                for i in 0..6 {
                    x += (*cur).args[(2_u32).wrapping_mul(i) as usize];
                    y += (*cur).args[(2 * 1 + 1) as usize];
                    if (*cd).bbox.llx > x {
                        (*cd).bbox.llx = x
                    }
                    if (*cd).bbox.urx < x {
                        (*cd).bbox.urx = x
                    }
                    if (*cd).bbox.lly > y {
                        (*cd).bbox.lly = y
                    }
                    if (*cd).bbox.ury < y {
                        (*cd).bbox.ury = y
                    }
                }
                if (*cur).args[12] == 50.0f64 {
                    if (*cur).args[1] == 0.0f64
                        && (*cur).args[11] == 0.0f64
                        && (*cur).args[5] == 0.0f64
                        && (*cur).args[7] == 0.0f64
                        && (*cur).args[3] + (*cur).args[9] == 0.0f64
                    {
                        /* cur->args[0] = cur->args[0];  dx1 */
                        (*cur).args[1] = (*cur).args[2]; /* dx2 */
                        (*cur).args[2] = (*cur).args[3]; /* dy2 */
                        (*cur).args[3] = (*cur).args[4]; /* dx3 */
                        (*cur).args[4] = (*cur).args[6]; /* dx4 */
                        (*cur).args[5] = (*cur).args[8]; /* dx5 */
                        (*cur).args[6] = (*cur).args[10]; /* dx6 */
                        (*cur).num_args = 7;
                        (*cur).type_0 = 34
                    } else if (*cur).args[5] == 0.0f64
                        && (*cur).args[7] == 0.0f64
                        && (*cur).args[1] + (*cur).args[3] + (*cur).args[9] + (*cur).args[11]
                            == 0 as f64
                    {
                        /* cur->args[0] = cur->args[0];  dx1 */
                        /* cur->args[1] = cur->args[1];  dy1 */
                        /* cur->args[2] = cur->args[2];  dx2 */
                        /* cur->args[3] = cur->args[3];  dy2 */
                        /* cur->args[4] = cur->args[4];  dx3 */
                        (*cur).args[5] = (*cur).args[6]; /* dx4 */
                        (*cur).args[6] = (*cur).args[8]; /* dx5 */
                        (*cur).args[7] = (*cur).args[9]; /* dy5 */
                        (*cur).args[8] = (*cur).args[10]; /* dx6 */
                        (*cur).num_args = 9;
                        (*cur).type_0 = 36
                    }
                }
            }
            -1 | 20 => {}
            _ => {
                panic!("Unexpected Type 2 charstring command {}.", (*cur).type_0);
            }
        }
        if !cur.is_null() {
            prev = cur
        }
        cur = next
    }
    /* Had no path. Fix lower-left point. */
    if (*cd).bbox.llx > (*cd).bbox.urx {
        (*cd).bbox.urx = (*cd).sbw.wx;
        (*cd).bbox.llx = (*cd).bbox.urx
    }
    if (*cd).bbox.lly > (*cd).bbox.ury {
        (*cd).bbox.ury = (*cd).sbw.wy;
        (*cd).bbox.lly = (*cd).bbox.ury
    };
}

pub(crate) unsafe fn t1char_get_metrics(
    mut src: *const u8,
    srclen: i32,
    subrs: &Option<Box<CffIndex>>,
    mut ginfo: *mut t1_ginfo,
) -> i32 {
    let mut t1char: t1_chardesc = t1_chardesc {
        flags: 0,
        sbw: C2RustUnnamed_3 {
            sbx: 0.,
            sby: 0.,
            wx: 0.,
            wy: 0.,
        },
        bbox: C2RustUnnamed_2 {
            llx: 0.,
            lly: 0.,
            urx: 0.,
            ury: 0.,
        },
        seac: C2RustUnnamed_1 {
            asb: 0.,
            adx: 0.,
            ady: 0.,
            bchar: 0,
            achar: 0,
        },
        num_stems: 0,
        stems: [t1_stem {
            id: 0,
            dir: 0,
            pos: 0.,
            del: 0.,
        }; 96],
        charpath: ptr::null_mut(),
        lastpath: ptr::null_mut(),
    };
    let cd = &mut t1char;
    init_charpath(cd);
    status = 0;
    phase = 0;
    nest = 0;
    ps_stack_top = 0;
    cs_stack_top = 0;
    let endptr = src.offset(srclen as isize);
    t1char_build_charpath(cd, &mut src, endptr, subrs);
    if cs_stack_top != 0 || ps_stack_top != 0 {
        warn!("Stack not empty. ({}, {})", cs_stack_top, ps_stack_top);
    }
    do_postproc(cd);
    if !ginfo.is_null() {
        (*ginfo).wx = (*cd).sbw.wx;
        (*ginfo).wy = (*cd).sbw.wy;
        (*ginfo).bbox.llx = (*cd).bbox.llx;
        (*ginfo).bbox.lly = (*cd).bbox.lly;
        (*ginfo).bbox.urx = (*cd).bbox.urx;
        (*ginfo).bbox.ury = (*cd).bbox.ury;
        if (*cd).flags & 1 << 2 != 0 {
            (*ginfo).use_seac = 1;
            (*ginfo).seac.adx = (*cd).seac.adx;
            (*ginfo).seac.ady = (*cd).seac.ady;
            (*ginfo).seac.bchar = (*cd).seac.bchar;
            (*ginfo).seac.achar = (*cd).seac.achar
        } else {
            (*ginfo).use_seac = 0
        }
    }
    release_charpath(cd);
    0
}
/*
 * Encode Charpath as a Type 2 Charstring
 */
unsafe fn t1char_encode_charpath(
    cd: *mut t1_chardesc,
    default_width: f64,
    nominal_width: f64,
    mut dst: *mut u8,
    endptr: *mut u8,
) -> i32 {
    assert!(!cd.is_null());
    let save = dst;
    let mut curr = (*cd).charpath;
    status = 0;
    phase = 0;
    nest = 0;
    ps_stack_top = 0;
    cs_stack_top = 0;
    /*
     * Advance Width
     */
    if (*cd).sbw.wx != default_width {
        let mut wx: f64 = (*cd).sbw.wx - nominal_width;
        put_numbers(&mut wx, 1, &mut dst, endptr);
        if status != 0 {
            panic!("Charstring encoder error: {}", status);
        }
    }
    /*
     * Hint Declaration
     */
    let mut num_hstems: i32 = 0;
    let mut num_vstems: i32 = 0;
    let mut reset: i32 = 1;
    let mut stem: [f64; 2] = [0.; 2];
    let mut i = 0;
    while i < (*cd).num_stems && (*cd).stems[i as usize].dir == 0 {
        num_hstems += 1;
        stem[0] = if reset != 0 {
            (*cd).stems[i as usize].pos
        } else {
            (*cd).stems[i as usize].pos
                - ((*cd).stems[(i - 1) as usize].pos + (*cd).stems[(i - 1) as usize].del)
        };
        stem[1] = (*cd).stems[i as usize].del;
        put_numbers(stem.as_mut_ptr(), 2, &mut dst, endptr);
        if status != 0 {
            panic!("Charstring encoder error: {}", status);
        }
        reset = 0;
        if 2 * num_hstems > 48 - 3 {
            if dst.offset(1) >= endptr {
                panic!("Buffer overflow.");
            }
            *dst = if (*cd).flags & 1 << 0 != 0 { 18 } else { 1 };
            dst = dst.offset(1);
            reset = 1;
        }
        i += 1
    }
    if reset == 0 {
        if dst.offset(1) >= endptr {
            panic!("Buffer overflow.");
        }
        *dst = if (*cd).flags & 1 << 0 != 0 { 18 } else { 1 };
        dst = dst.offset(1);
    }
    reset = 1;
    if (*cd).num_stems - num_hstems > 0 {
        for i in num_hstems..(*cd).num_stems {
            num_vstems += 1;
            stem[0] = if reset != 0 {
                (*cd).stems[i as usize].pos
            } else {
                (*cd).stems[i as usize].pos
                    - ((*cd).stems[(i - 1) as usize].pos + (*cd).stems[(i - 1) as usize].del)
            };
            stem[1] = (*cd).stems[i as usize].del;
            put_numbers(stem.as_mut_ptr(), 2, &mut dst, endptr);
            if status != 0 {
                panic!("Charstring encoder error: {}", status);
            }
            reset = 0;
            if 2 * num_vstems > 48 - 3 {
                if dst.offset(1) >= endptr {
                    panic!("Buffer overflow.");
                }
                *dst = if (*cd).flags & 1 << 0 != 0 { 23 } else { 3 };
                dst = dst.offset(1);
                reset = 1
            }
        }
        if reset == 0 {
            if dst.offset(1) >= endptr {
                panic!("Buffer overflow.");
            }
            if (*cd).flags & 1 << 0 != 0 || (*cd).flags & 1 << 1 != 0 {
                /*
                 * The vstem hint operator can be ommited if hstem and vstem hints
                 * are both declared at the beginning of a charstring, and is
                 * followed directly by the hintmask or cntrmask operators.
                 */
                if (*curr).type_0 != -1 && (*curr).type_0 != 20 {
                    *dst = 23;
                    dst = dst.offset(1);
                }
            } else {
                *dst = 3;
                dst = dst.offset(1);
            }
        }
    }
    /*
     * Path Construction and Hint Replacement
     */
    while !curr.is_null() && (*curr).type_0 != 14 {
        match (*curr).type_0 {
            -1 => {
                let mut hintmask: [u8; 12] = [0; 12];
                memset(
                    hintmask.as_mut_ptr() as *mut libc::c_void,
                    0,
                    (((*cd).num_stems + 7) / 8) as _,
                );
                while !curr.is_null() && (*curr).type_0 == -1 {
                    let stem_idx = get_stem(cd, (*curr).args[0] as i32);
                    assert!(stem_idx < (*cd).num_stems);
                    hintmask[(stem_idx / 8) as usize] =
                        (hintmask[(stem_idx / 8) as usize] as i32 | 1 << 7 - stem_idx % 8) as u8;
                    curr = (*curr).next
                }
                if (*cd).flags & 1 << 0 != 0 {
                    if dst.offset((((*cd).num_stems + 7) / 8 + 1) as isize) >= endptr {
                        panic!("Buffer overflow.");
                    }
                    *dst = 19;
                    dst = dst.offset(1);
                    memcpy(
                        dst as *mut libc::c_void,
                        hintmask.as_mut_ptr() as *const libc::c_void,
                        (((*cd).num_stems + 7) / 8) as _,
                    );
                    dst = dst.offset((((*cd).num_stems + 7) / 8) as isize)
                }
            }
            20 => {
                let mut cntrmask: [u8; 12] = [0; 12];
                memset(
                    cntrmask.as_mut_ptr() as *mut libc::c_void,
                    0,
                    (((*cd).num_stems + 7) / 8) as _,
                );
                for i_0 in 0..(*curr).num_args {
                    let stem_idx_0 = get_stem(cd, (*curr).args[i_0 as usize] as i32);
                    assert!(stem_idx_0 < (*cd).num_stems);
                    cntrmask[(stem_idx_0 / 8) as usize] =
                        (cntrmask[(stem_idx_0 / 8) as usize] as i32 | 1 << 7 - stem_idx_0 % 8)
                            as u8;
                }
                if dst.offset((((*cd).num_stems + 7) / 8 + 1) as isize) >= endptr {
                    panic!("Buffer overflow.");
                }
                *dst = 20;
                dst = dst.offset(1);
                memcpy(
                    dst as *mut libc::c_void,
                    cntrmask.as_mut_ptr() as *const libc::c_void,
                    (((*cd).num_stems + 7) / 8) as _,
                );
                dst = dst.offset((((*cd).num_stems + 7) / 8) as isize);
                curr = (*curr).next
            }
            21 | 22 | 4 | 5 | 6 | 7 | 8 | 31 | 30 | 25 | 24 => {
                put_numbers(
                    (*curr).args.as_mut_ptr(),
                    (*curr).num_args,
                    &mut dst,
                    endptr,
                );
                if status != 0 {
                    panic!("Charstring encoder error: {}", status);
                }
                if dst.offset(1) >= endptr {
                    panic!("Buffer overflow.");
                }
                *dst = (*curr).type_0 as u8;
                dst = dst.offset(1);
                curr = (*curr).next
            }
            35 | 34 | 36 => {
                put_numbers(
                    (*curr).args.as_mut_ptr(),
                    (*curr).num_args,
                    &mut dst,
                    endptr,
                );
                if status != 0 {
                    panic!("Charstring encoder error: {}", status);
                }
                if dst.offset(2) >= endptr {
                    panic!("Buffer overflow.");
                }
                *dst = 12;
                dst = dst.offset(1);
                *dst = (*curr).type_0 as u8;
                dst = dst.offset(1);
                curr = (*curr).next
            }
            _ => {
                panic!("Unknown Type 2 charstring command: {}", (*curr).type_0);
            }
        }
    }
    /*
     * (adx ady bchar achar) endchar
     */
    if (*cd).flags & 1 << 2 != 0 {
        let mut seac: [f64; 4] = [0.; 4];
        seac[0] = (*cd).seac.adx;
        seac[1] = (*cd).seac.ady;
        seac[2] = (*cd).seac.bchar as f64;
        seac[3] = (*cd).seac.achar as f64;
        put_numbers(seac.as_mut_ptr(), 4, &mut dst, endptr);
        if status != 0 {
            panic!("Charstring encoder error: {}", status);
        }
        if dst.offset(2) >= endptr {
            panic!("Buffer overflow.");
        }
        warn!("Obsolete four arguments of \"endchar\" will be used for Type 1 \"seac\" operator.");
    }
    if dst.offset(1) >= endptr {
        panic!("Buffer overflow.");
    }
    *dst = 14;
    dst = dst.offset(1);
    dst.offset_from(save) as i64 as i32
}

pub(crate) unsafe fn t1char_convert_charstring(
    dst: *mut u8,
    dstlen: i32,
    mut src: *const u8,
    srclen: i32,
    subrs: &Option<Box<CffIndex>>,
    default_width: f64,
    nominal_width: f64,
    mut ginfo: *mut t1_ginfo,
) -> i32 {
    let mut t1char: t1_chardesc = t1_chardesc {
        flags: 0,
        sbw: C2RustUnnamed_3 {
            sbx: 0.,
            sby: 0.,
            wx: 0.,
            wy: 0.,
        },
        bbox: C2RustUnnamed_2 {
            llx: 0.,
            lly: 0.,
            urx: 0.,
            ury: 0.,
        },
        seac: C2RustUnnamed_1 {
            asb: 0.,
            adx: 0.,
            ady: 0.,
            bchar: 0,
            achar: 0,
        },
        num_stems: 0,
        stems: [t1_stem {
            id: 0,
            dir: 0,
            pos: 0.,
            del: 0.,
        }; 96],
        charpath: ptr::null_mut(),
        lastpath: ptr::null_mut(),
    };
    let cd = &mut t1char;
    init_charpath(cd);
    status = 0;
    phase = 0;
    nest = 0;
    ps_stack_top = 0;
    cs_stack_top = 0;
    let endptr = src.offset(srclen as isize);
    t1char_build_charpath(cd, &mut src, endptr, subrs);
    if cs_stack_top != 0 || ps_stack_top != 0 {
        warn!("Stack not empty. ({}, {})", cs_stack_top, ps_stack_top);
    }
    do_postproc(cd);
    cd.stems[..cd.num_stems as usize].sort_unstable_by(stem_compare);
    let length = t1char_encode_charpath(
        cd,
        default_width,
        nominal_width,
        dst,
        dst.offset(dstlen as isize),
    );
    if !ginfo.is_null() {
        (*ginfo).wx = (*cd).sbw.wx;
        (*ginfo).wy = (*cd).sbw.wy;
        (*ginfo).bbox.llx = (*cd).bbox.llx;
        (*ginfo).bbox.lly = (*cd).bbox.lly;
        (*ginfo).bbox.urx = (*cd).bbox.urx;
        (*ginfo).bbox.ury = (*cd).bbox.ury;
        if (*cd).flags & 1 << 2 != 0 {
            (*ginfo).use_seac = 1;
            (*ginfo).seac.adx = (*cd).seac.adx;
            (*ginfo).seac.ady = (*cd).seac.ady;
            (*ginfo).seac.bchar = (*cd).seac.bchar;
            (*ginfo).seac.achar = (*cd).seac.achar
        } else {
            (*ginfo).use_seac = 0
        }
    }
    release_charpath(cd);
    length
}
