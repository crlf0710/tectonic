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

use crate::streq_ptr;
use crate::warn;
use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_mem::{new, renew};
use crate::dpx_pdfobj::{pdf_link_obj, pdf_obj, pdf_ref_obj, pdf_release_obj};
use crate::mfree;
use libc::{free, strcpy, strlen};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_res {
    pub ident: *mut i8,
    pub flags: i32,
    pub category: i32,
    pub cdata: *mut libc::c_void,
    pub object: *mut pdf_obj,
    pub reference: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct res_cache {
    pub count: i32,
    pub capacity: i32,
    pub resources: *mut pdf_res,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed {
    pub name: *const i8,
    pub cat_id: i32,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut pdf_resource_categories: [C2RustUnnamed; 9] = [
    C2RustUnnamed {
        name: b"Font\x00" as *const u8 as *const i8,
        cat_id: 0i32,
    },
    C2RustUnnamed {
        name: b"CIDFont\x00" as *const u8 as *const i8,
        cat_id: 1i32,
    },
    C2RustUnnamed {
        name: b"Encoding\x00" as *const u8 as *const i8,
        cat_id: 2i32,
    },
    C2RustUnnamed {
        name: b"CMap\x00" as *const u8 as *const i8,
        cat_id: 3i32,
    },
    C2RustUnnamed {
        name: b"XObject\x00" as *const u8 as *const i8,
        cat_id: 4i32,
    },
    C2RustUnnamed {
        name: b"ColorSpace\x00" as *const u8 as *const i8,
        cat_id: 5i32,
    },
    C2RustUnnamed {
        name: b"Shading\x00" as *const u8 as *const i8,
        cat_id: 6i32,
    },
    C2RustUnnamed {
        name: b"Pattern\x00" as *const u8 as *const i8,
        cat_id: 7i32,
    },
    C2RustUnnamed {
        name: b"ExtGState\x00" as *const u8 as *const i8,
        cat_id: 8i32,
    },
];
static mut resources: [res_cache; 9] = [res_cache {
    count: 0,
    capacity: 0,
    resources: std::ptr::null_mut(),
}; 9];
unsafe fn pdf_init_resource(mut res: *mut pdf_res) {
    assert!(!res.is_null());
    (*res).ident = ptr::null_mut();
    (*res).category = -1i32;
    (*res).flags = 0i32;
    (*res).cdata = ptr::null_mut();
    (*res).object = ptr::null_mut();
    (*res).reference = ptr::null_mut();
}
unsafe fn pdf_flush_resource(mut res: *mut pdf_res) {
    if !res.is_null() {
        pdf_release_obj((*res).reference);
        pdf_release_obj((*res).object);
        (*res).reference = ptr::null_mut();
        (*res).object = ptr::null_mut()
    };
}
unsafe fn pdf_clean_resource(mut res: *mut pdf_res) {
    if !res.is_null() {
        if !(*res).reference.is_null() || !(*res).object.is_null() {
            warn!("Trying to release un-flushed object.");
        }
        pdf_release_obj((*res).reference);
        pdf_release_obj((*res).object);
        (*res).ident = mfree((*res).ident as *mut libc::c_void) as *mut i8;
        (*res).category = -1i32;
        (*res).flags = 0i32
    };
}

pub unsafe fn pdf_init_resources() {
    for i in 0..(::std::mem::size_of::<[C2RustUnnamed; 9]>() as u64)
        .wrapping_div(::std::mem::size_of::<C2RustUnnamed>() as u64) as usize
    {
        resources[i].count = 0i32;
        resources[i].capacity = 0i32;
        resources[i].resources = ptr::null_mut();
    }
}

pub unsafe fn pdf_close_resources() {
    for i in 0..(::std::mem::size_of::<[C2RustUnnamed; 9]>() as u64)
        .wrapping_div(::std::mem::size_of::<C2RustUnnamed>() as u64)
    {
        let rc = &mut *resources.as_mut_ptr().offset(i as isize) as *mut res_cache;
        for j in 0..(*rc).count {
            pdf_flush_resource(&mut *(*rc).resources.offset(j as isize));
            pdf_clean_resource(&mut *(*rc).resources.offset(j as isize));
        }
        free((*rc).resources as *mut libc::c_void);
        (*rc).count = 0i32;
        (*rc).capacity = 0i32;
        (*rc).resources = ptr::null_mut();
    }
}
unsafe fn get_category(category: *const i8) -> i32 {
    for i in 0..(::std::mem::size_of::<[C2RustUnnamed; 9]>() as u64)
        .wrapping_div(::std::mem::size_of::<C2RustUnnamed>() as u64) as usize
    {
        if streq_ptr(category, pdf_resource_categories[i].name) {
            return pdf_resource_categories[i].cat_id;
        }
    }
    -1i32
}

pub unsafe fn pdf_defineresource(
    category: *const i8,
    resname: *const i8,
    object: *mut pdf_obj,
    flags: i32,
) -> i32 {
    let mut res_id;
    assert!(!category.is_null() && !object.is_null());
    let cat_id = get_category(category);
    if cat_id < 0i32 {
        panic!(
            "Unknown resource category: {}",
            CStr::from_ptr(category).display(),
        );
    }
    let rc = &mut *resources.as_mut_ptr().offset(cat_id as isize) as *mut res_cache;
    if !resname.is_null() {
        res_id = 0i32;
        while res_id < (*rc).count {
            let res = &mut *(*rc).resources.offset(res_id as isize) as *mut pdf_res;
            if streq_ptr(resname, (*res).ident) {
                warn!(
                    "Resource {} (category: {}) already defined...",
                    CStr::from_ptr(resname).display(),
                    CStr::from_ptr(category).display(),
                );
                pdf_flush_resource(res);
                (*res).flags = flags;
                if flags & 1i32 != 0 {
                    (*res).reference = pdf_ref_obj(object);
                    pdf_release_obj(object);
                } else {
                    (*res).object = object
                }
                return cat_id << 16i32 | res_id;
            }
            res_id += 1
        }
    } else {
        res_id = (*rc).count
    }
    if res_id == (*rc).count {
        if (*rc).count >= (*rc).capacity {
            (*rc).capacity = ((*rc).capacity as u32).wrapping_add(16u32) as i32 as i32;
            (*rc).resources = renew(
                (*rc).resources as *mut libc::c_void,
                ((*rc).capacity as u32 as u64).wrapping_mul(::std::mem::size_of::<pdf_res>() as u64)
                    as u32,
            ) as *mut pdf_res
        }
        let res = &mut *(*rc).resources.offset(res_id as isize) as *mut pdf_res;
        pdf_init_resource(res);
        if !resname.is_null() && *resname.offset(0) as i32 != '\u{0}' as i32 {
            (*res).ident = new(
                (strlen(resname).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
            ) as *mut i8;
            strcpy((*res).ident, resname);
        }
        (*res).category = cat_id;
        (*res).flags = flags;
        if flags & 1i32 != 0 {
            (*res).reference = pdf_ref_obj(object);
            pdf_release_obj(object);
        } else {
            (*res).object = object
        }
        (*rc).count += 1
    }
    cat_id << 16i32 | res_id
}

pub unsafe fn pdf_findresource(category: *const i8, resname: *const i8) -> i32 {
    assert!(!resname.is_null() && !category.is_null());
    let cat_id = get_category(category);
    if cat_id < 0i32 {
        panic!(
            "Unknown resource category: {}",
            CStr::from_ptr(category).display(),
        );
    }
    let rc = &mut *resources.as_mut_ptr().offset(cat_id as isize) as *mut res_cache;
    for res_id in 0..(*rc).count {
        let res = &mut *(*rc).resources.offset(res_id as isize) as *mut pdf_res;
        if streq_ptr(resname, (*res).ident) {
            return cat_id << 16i32 | res_id;
        }
    }
    -1i32
}

pub unsafe fn pdf_get_resource_reference(rc_id: i32) -> *mut pdf_obj {
    let cat_id = rc_id >> 16i32 & 0xffffi32;
    let res_id = rc_id & 0xffffi32;
    if cat_id < 0i32
        || cat_id as u64
            >= (::std::mem::size_of::<[C2RustUnnamed; 9]>() as u64)
                .wrapping_div(::std::mem::size_of::<C2RustUnnamed>() as u64)
    {
        panic!("Invalid category ID: {}", cat_id);
    }
    let rc = &mut *resources.as_mut_ptr().offset(cat_id as isize) as *mut res_cache;
    if res_id < 0i32 || res_id >= (*rc).count {
        panic!("Invalid resource ID: {}", res_id);
    }
    let res = &mut *(*rc).resources.offset(res_id as isize) as *mut pdf_res;
    if (*res).reference.is_null() {
        if (*res).object.is_null() {
            panic!("Undefined object...");
        } else {
            (*res).reference = pdf_ref_obj((*res).object)
        }
    }
    pdf_link_obj((*res).reference)
}
