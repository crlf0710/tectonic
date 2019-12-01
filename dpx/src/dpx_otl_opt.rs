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
)]

use crate::warn;
use crate::bridge::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_mem::new;
use libc::{free, memcpy, memset, strlen};

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct otl_opt {
    pub(crate) rule: *mut bt_node,
    /* _OTL_OPT_H_ */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct bt_node {
    pub(crate) flag: i32,
    pub(crate) left: *mut bt_node,
    pub(crate) right: *mut bt_node,
    pub(crate) data: [i8; 4],
}
unsafe fn match_expr(expr: *mut bt_node, key: *const i8) -> i32 {
    let mut retval: i32 = 1i32;
    if !expr.is_null() {
        if (*expr).left.is_null() && (*expr).right.is_null() {
            for i in 0..4 {
                if (*expr).data[i as usize] as i32 != '?' as i32
                    && (*expr).data[i as usize] as i32 != *key.offset(i as isize) as i32
                {
                    retval = 0i32;
                    break;
                }
            }
        } else {
            if !(*expr).left.is_null() {
                retval = match_expr((*expr).left, key)
            }
            if !(*expr).right.is_null() {
                if retval != 0 && (*expr).flag & 1i32 << 1i32 != 0 {
                    retval &= match_expr((*expr).right, key)
                } else if retval == 0 && (*expr).flag & 1i32 << 1i32 == 0 {
                    retval = match_expr((*expr).right, key)
                }
            }
        }
        if (*expr).flag & 1i32 << 0i32 != 0 {
            retval = if retval != 0 { 0i32 } else { 1i32 }
        }
    }
    retval
}
unsafe fn bt_new_tree() -> *mut bt_node {
    let expr =
        new((1_u64).wrapping_mul(::std::mem::size_of::<bt_node>() as u64) as u32) as *mut bt_node;
    (*expr).flag = 0i32;
    (*expr).left = ptr::null_mut();
    (*expr).right = ptr::null_mut();
    memset((*expr).data.as_mut_ptr() as *mut libc::c_void, 0i32, 4);
    expr
}
unsafe fn bt_release_tree(tree: *mut bt_node) {
    if !tree.is_null() {
        if !(*tree).left.is_null() {
            bt_release_tree((*tree).left);
        }
        if !(*tree).right.is_null() {
            bt_release_tree((*tree).right);
        }
        free(tree as *mut libc::c_void);
    };
}
unsafe fn parse_expr(pp: *mut *const i8, endptr: *const i8) -> *mut bt_node {
    if *pp >= endptr {
        return ptr::null_mut();
    }
    let mut curr = bt_new_tree();
    let mut root = curr;
    while *pp < endptr {
        match **pp as i32 {
            33 => {
                if (*curr).flag & 2i32 != 0 {
                    (*curr).flag &= !(1i32 << 0i32)
                } else {
                    (*curr).flag |= 1i32 << 0i32
                }
                *pp = (*pp).offset(1)
            }
            40 => {
                *pp = (*pp).offset(1);
                if *pp < endptr {
                    let expr = parse_expr(pp, endptr);
                    if expr.is_null() {
                        warn!("Syntax error: {}\n", CStr::from_ptr(*pp).display());
                        return ptr::null_mut();
                    }
                    if **pp as i32 != ')' as i32 {
                        warn!("Syntax error: Unbalanced ()\n");
                        return ptr::null_mut();
                    }
                    (*curr).left = (*expr).left;
                    (*curr).right = (*expr).right;
                    memcpy(
                        (*curr).data.as_mut_ptr() as *mut libc::c_void,
                        (*expr).data.as_mut_ptr() as *const libc::c_void,
                        4,
                    );
                    free(expr as *mut libc::c_void);
                } else {
                    warn!("Syntax error: Unbalanced ()\n");
                    bt_release_tree(root);
                    return ptr::null_mut();
                }
                *pp = (*pp).offset(1)
            }
            41 => return root,
            124 | 38 => {
                if *pp >= endptr {
                    warn!("Syntax error: {}\n", CStr::from_ptr(*pp).display());
                    bt_release_tree(root);
                    return ptr::null_mut();
                } else {
                    let tmp = bt_new_tree();
                    (*tmp).left = root;
                    curr = bt_new_tree();
                    (*tmp).right = curr;
                    if **pp as i32 == '&' as i32 {
                        (*tmp).flag = 1i32
                    } else {
                        (*tmp).flag = 0i32
                    }
                    root = tmp
                }
                *pp = (*pp).offset(1)
            }
            42 => {
                memset(
                    (*curr).data.as_mut_ptr() as *mut libc::c_void,
                    '?' as i32,
                    4,
                );
                *pp = (*pp).offset(1)
            }
            _ => {
                if (*pp).offset(4) <= endptr {
                    for i in 0..4 {
                        if **pp as i32 == ' ' as i32
                            || **pp as i32 == '?' as i32
                            || (**pp as u8).is_ascii_alphanumeric()
                        {
                            (*curr).data[i as usize] = **pp
                        } else if **pp as i32 == '_' as i32 {
                            (*curr).data[i as usize] = ' ' as i32 as i8
                        } else {
                            warn!("Invalid char in tag: {}\n", char::from(**pp as u8),);
                            bt_release_tree(root);
                            return ptr::null_mut();
                        }
                        *pp = (*pp).offset(1);
                    }
                } else {
                    warn!("Syntax error: {}\n", CStr::from_ptr(*pp).display());
                    bt_release_tree(root);
                    return ptr::null_mut();
                }
            }
        }
    }
    root
}

pub(crate) unsafe fn otl_new_opt() -> *mut otl_opt {
    let opt =
        new((1_u64).wrapping_mul(::std::mem::size_of::<otl_opt>() as u64) as u32) as *mut otl_opt;
    (*opt).rule = ptr::null_mut();
    opt as *mut otl_opt
}

pub(crate) unsafe fn otl_release_opt(mut opt: *mut otl_opt) {
    if !(*opt).rule.is_null() {
        bt_release_tree((*opt).rule);
    }
    (*opt).rule = ptr::null_mut();
    free(opt as *mut libc::c_void);
}

pub(crate) unsafe fn otl_parse_optstring(mut opt: *mut otl_opt, optstr: *const i8) -> i32 {
    assert!(!opt.is_null());
    if !optstr.is_null() {
        let mut p = optstr as *const i8;
        let endptr = p.offset(strlen(optstr) as isize);
        (*opt).rule = parse_expr(&mut p, endptr)
    }
    0i32
}

pub(crate) unsafe fn otl_match_optrule(opt: *mut otl_opt, tag: *const i8) -> i32 {
    assert!(!tag.is_null());
    if opt.is_null() || (*opt).rule.is_null() {
        return 1i32;
    }
    match_expr((*opt).rule, tag)
}
