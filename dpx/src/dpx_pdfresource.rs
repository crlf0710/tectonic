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
use std::ptr;

use crate::dpx_pdfobj::{pdf_link_obj, pdf_obj, pdf_ref_obj, IntoObj, IntoRef, Object};

#[derive(Clone)]
#[repr(C)]
pub(crate) struct pdf_res {
    pub(crate) ident: String,
    pub(crate) flags: i32,
    pub(crate) category: i32,
    pub(crate) cdata: *mut libc::c_void,
    pub(crate) object: *mut pdf_obj,
    pub(crate) reference: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct Category {
    pub(crate) name: &'static str,
    pub(crate) cat_id: i32,
}
impl Category {
    const fn new(name: &'static str, cat_id: i32) -> Self {
        Self { name, cat_id }
    }
}

static mut pdf_resource_categories: [Category; 9] = [
    Category::new("Font", 0),
    Category::new("CIDFont", 1),
    Category::new("Encoding", 2),
    Category::new("CMap", 3),
    Category::new("XObject", 4),
    Category::new("ColorSpace", 5),
    Category::new("Shading", 6),
    Category::new("Pattern", 7),
    Category::new("ExtGState", 8),
];
const ZEROVEC: Vec<pdf_res> = Vec::new();
static mut resources: [Vec<pdf_res>; 9] = [ZEROVEC; 9];

impl pdf_res {
    unsafe fn new(ident: String, cat_id: i32, flags: i32, object: Object) -> Self {
        let mut res = Self {
            ident,
            category: cat_id,
            flags: flags,
            cdata: ptr::null_mut(),
            object: ptr::null_mut(),
            reference: ptr::null_mut(),
        };
        if flags & 1 != 0 {
            res.reference = object.into_ref().into_obj();
        } else {
            res.object = object.into_obj();
        }
        res
    }
}

impl Drop for pdf_res {
    fn drop(&mut self) {
        unsafe {
            crate::release!(self.reference);
            self.reference = ptr::null_mut();
            crate::release!(self.object);
            self.object = ptr::null_mut();
        };
    }
}

pub(crate) unsafe fn pdf_init_resources() {
    for rc in &mut resources {
        rc.clear();
    }
}

pub(crate) unsafe fn pdf_close_resources() {
    for rc in &mut resources {
        rc.clear();
    }
}
unsafe fn get_category(category: &str) -> Option<i32> {
    for cat in &pdf_resource_categories {
        if category == cat.name {
            return Some(cat.cat_id);
        }
    }
    None
}

pub(crate) unsafe fn pdf_defineresource(
    category: &str,
    resname: &str,
    object: Object,
    flags: i32,
) -> i32 {
    let cat_id =
        get_category(category).unwrap_or_else(|| panic!("Unknown resource category: {}", category));
    let rc = &mut resources[cat_id as usize];
    for (res_id, res) in rc.iter_mut().enumerate() {
        if resname == res.ident {
            warn!(
                "Resource {} (category: {}) already defined...",
                resname, category,
            );
            let ident = std::mem::replace(&mut res.ident, String::new());
            *res = pdf_res::new(ident, res.category, flags, object);
            return cat_id << 16 | (res_id as i32);
        }
    }

    let res_id = rc.len();
    rc.push(pdf_res::new(
        if !resname.is_empty() {
            resname.to_string()
        } else {
            String::new()
        },
        cat_id,
        flags,
        object,
    ));

    cat_id << 16 | (res_id as i32)
}

pub(crate) unsafe fn pdf_findresource(category: &str, resname: &str) -> Option<i32> {
    let cat_id =
        get_category(category).unwrap_or_else(|| panic!("Unknown resource category: {}", category));
    let rc = &mut resources[cat_id as usize];
    for res_id in 0..rc.len() {
        let res = &mut rc[res_id];
        if resname == res.ident {
            return Some(cat_id << 16 | (res_id as i32));
        }
    }
    None
}

pub(crate) unsafe fn pdf_get_resource_reference(rc_id: i32) -> *mut pdf_obj {
    let cat_id = rc_id >> 16 & 0xffff;
    let res_id = rc_id & 0xffff;
    if cat_id < 0 || cat_id as u64 >= 9 {
        panic!("Invalid category ID: {}", cat_id);
    }
    let rc = &mut resources[cat_id as usize];
    if res_id < 0 || res_id >= rc.len() as i32 {
        panic!("Invalid resource ID: {}", res_id);
    }
    let res = &mut rc[res_id as usize];
    if res.reference.is_null() {
        if res.object.is_null() {
            panic!("Undefined object...");
        } else {
            res.reference = pdf_ref_obj(res.object)
        }
    }
    pdf_link_obj(res.reference)
}
