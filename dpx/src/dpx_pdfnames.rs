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

use crate::mfree;
use crate::warn;
use std::cmp::Ordering;
use std::fmt::Write;
use std::ptr;
use std::slice;

use super::dpx_dpxutil::{
    ht_append_table, ht_clear_iter, ht_clear_table, ht_init_table, ht_iter_getval, ht_iter_next,
    ht_lookup_table, ht_set_iter,
};
use super::dpx_mem::new;
use crate::dpx_pdfobj::{
    pdf_dict, pdf_link_obj, pdf_new_ref, pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_string,
    pdf_transfer_label, IntoObj, Object, PushObj,
};
use libc::free;

use crate::bridge::size_t;

use super::dpx_dpxutil::ht_iter;
use super::dpx_dpxutil::ht_table;
/* Hash */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct obj_data {
    pub(crate) object: *mut pdf_obj,
    pub(crate) closed: i32,
    /* 1 if object is closed */
}
#[derive(Copy, Clone)]
pub(crate) struct named_object {
    pub(crate) key: *const u8,
    pub(crate) keylen: i32,
    pub(crate) value: *mut pdf_obj,
}
impl Default for named_object {
    fn default() -> Self {
        Self {
            key: ptr::null_mut(),
            keylen: 0,
            value: ptr::null_mut(),
        }
    }
}

unsafe fn printable_key(key: &[u8]) -> String {
    let mut printable = String::with_capacity(key.len() * 2);
    for &b in key.iter() {
        if b.is_ascii_graphic() {
            write!(&mut printable, "{}", b as char).expect("Failed to write String");
        } else {
            write!(&mut printable, "#{:02X}", b).expect("Failed to write String");
        }
    }
    printable
}
#[inline]
unsafe fn hval_free(hval: *mut libc::c_void) {
    let value = hval as *mut obj_data;
    if !(*value).object.is_null() {
        pdf_release_obj((*value).object);
        (*value).object = ptr::null_mut()
    }
    free(value as *mut libc::c_void);
}

pub(crate) unsafe fn pdf_new_name_tree() -> *mut ht_table {
    let names =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ht_table>() as u64) as u32) as *mut ht_table;
    ht_init_table(
        names,
        Some(hval_free as unsafe fn(_: *mut libc::c_void) -> ()),
    );
    names
}
unsafe fn check_objects_defined(ht_tab: *mut ht_table) {
    let mut iter: ht_iter = ht_iter {
        index: 0,
        curr: ptr::null_mut(),
        hash: ptr::null_mut(),
    };
    if ht_set_iter(ht_tab, &mut iter) >= 0 {
        loop {
            let key = iter.get_key();
            let value = ht_iter_getval(&iter) as *const obj_data;
            assert!(!(*value).object.is_null());
            if !(*value).object.is_null() && (*(*value).object).is_undefined() {
                pdf_names_add_object(&mut *ht_tab, key, &mut *Object::Null.into_obj());
                warn!(
                    "Object @{} used, but not defined. Replaced by null.",
                    printable_key(key),
                );
            }
            if !(ht_iter_next(&mut iter) >= 0) {
                break;
            }
        }
        ht_clear_iter(&mut iter);
    };
}

pub(crate) unsafe fn pdf_delete_name_tree(names: *mut *mut ht_table) {
    assert!(!names.is_null() && !(*names).is_null());
    check_objects_defined(*names);
    ht_clear_table(*names);
    *names = mfree(*names as *mut libc::c_void) as *mut ht_table;
}

pub(crate) unsafe fn pdf_names_add_object(
    names: &mut ht_table,
    key: &[u8],
    object: &mut pdf_obj,
) -> i32 {
    if key.is_empty() {
        warn!("Null string used for name tree key.");
        return -1;
    }
    let mut value = ht_lookup_table(names, key) as *mut obj_data;
    if value.is_null() {
        value = new((1_u64).wrapping_mul(::std::mem::size_of::<obj_data>() as u64) as u32)
            as *mut obj_data;
        (*value).object = object;
        (*value).closed = 0;
        ht_append_table(names, key, value as *mut libc::c_void);
    } else {
        assert!(!(*value).object.is_null());
        if !(*value).object.is_null() && (*(*value).object).is_undefined() {
            pdf_transfer_label(object, &mut *(*value).object);
            pdf_release_obj((*value).object);
            (*value).object = object
        } else {
            warn!("Object @{} already defined.", printable_key(key));
            pdf_release_obj(object);
            return -1;
        }
    }
    0
}
/*
 * The following routine returns copies, not the original object.
 */

pub(crate) unsafe fn pdf_names_lookup_reference(names: &mut ht_table, key: &[u8]) -> *mut pdf_obj {
    let object;
    let value = ht_lookup_table(names, key) as *mut obj_data;
    if !value.is_null() {
        object = (*value).object;
        assert!(!object.is_null());
    } else {
        /* A null object as dummy would create problems because as value
         * of a dictionary entry, a null object is be equivalent to no entry
         * at all. This matters for optimization of PDF destinations.
         */
        object = Object::Undefined.into_obj();
        pdf_names_add_object(names, key, &mut *object);
    }
    pdf_ref_obj(object)
}

pub(crate) unsafe fn pdf_names_lookup_object(names: *mut ht_table, key: &[u8]) -> *mut pdf_obj {
    assert!(!names.is_null());
    let value = ht_lookup_table(names, key) as *mut obj_data;
    if value.is_null() || !(*value).object.is_null() && (*(*value).object).is_undefined() {
        return ptr::null_mut();
    }
    assert!(!(*value).object.is_null());
    (*value).object
}

pub(crate) unsafe fn pdf_names_close_object(names: *mut ht_table, key: &[u8]) -> i32 {
    assert!(!names.is_null());
    let value = ht_lookup_table(names, key) as *mut obj_data;
    if value.is_null() || !(*value).object.is_null() && (*(*value).object).is_undefined() {
        warn!("Cannot close undefined object @{}.", printable_key(key));
        return -1;
    }
    assert!(!(*value).object.is_null());
    if (*value).closed != 0 {
        warn!("Object @{} already closed.", printable_key(key));
        return -1;
    }
    (*value).closed = 1;
    0
}
#[inline]
fn cmp_key(sd1: &named_object, sd2: &named_object) -> Ordering {
    if sd1.key.is_null() {
        Ordering::Less
    } else if sd2.key.is_null() {
        Ordering::Greater
    } else {
        unsafe {
            let key1 = slice::from_raw_parts(sd1.key, sd1.keylen as usize);
            let key2 = slice::from_raw_parts(sd2.key, sd2.keylen as usize);
            key1.cmp(key2)
        }
    }
}
unsafe fn build_name_tree(first: &mut [named_object], is_root: i32) -> pdf_dict {
    let mut result = pdf_dict::new();
    /*
     * According to PDF Refrence, Third Edition (p.101-102), a name tree
     * always has exactly one root node, which contains a SINGLE entry:
     * either Kids or Names but not both. If the root node has a Names
     * entry, it is the only node in the tree. If it has a Kids entry,
     * then each of the remaining nodes is either an intermediate node,
     * containing a Limits entry and a Kids entry, or a leaf node,
     * containing a Limits entry and a Names entry.
     */
    if is_root == 0 {
        let mut limits = vec![];
        let last = &first[first.len() - 1];
        limits.push_obj(pdf_string::new_from_ptr(
            first[0].key as *const libc::c_void,
            first[0].keylen as size_t,
        ));
        limits.push_obj(pdf_string::new_from_ptr(
            last.key as *const libc::c_void,
            last.keylen as size_t,
        ));
        result.set("Limits", limits);
    }
    if first.len() > 0 && first.len() <= 2 * 4 {
        /* Create leaf nodes. */
        let mut names = vec![];
        for cur in first.iter_mut() {
            names.push_obj(pdf_string::new_from_ptr(
                cur.key as *const libc::c_void,
                cur.keylen as size_t,
            ));
            match (&*cur.value).data {
                Object::Array(_) | Object::Dict(_) | Object::Stream(_) | Object::String(_) => {
                    names.push(pdf_ref_obj(cur.value));
                }
                Object::Invalid => {
                    panic!(
                        "Invalid object...: {}",
                        printable_key(std::slice::from_raw_parts(
                            cur.key as *const u8,
                            cur.keylen as _
                        ))
                    );
                }
                _ => {
                    names.push(pdf_link_obj(cur.value));
                }
            }
            pdf_release_obj(cur.value);
            cur.value = ptr::null_mut();
        }
        result.set("Names", names.into_obj());
    } else if first.len() > 0 {
        /* Intermediate node */
        let mut kids = vec![];
        for i in 0..4 {
            let start = i * first.len() / 4;
            let end = (i + 1) * first.len() / 4;
            let subtree = &mut *build_name_tree(&mut first[start..end], 0).into_obj();
            kids.push_obj(pdf_new_ref(subtree));
            pdf_release_obj(subtree);
        }
        result.set("Kids", kids);
    }
    result
}
unsafe fn flat_table(ht_tab: *mut ht_table, filter: *mut ht_table) -> Vec<named_object> {
    let mut iter: ht_iter = ht_iter {
        index: 0,
        curr: ptr::null_mut(),
        hash: ptr::null_mut(),
    };
    assert!(!ht_tab.is_null());
    let mut objects = Vec::with_capacity((*ht_tab).count as usize);
    if ht_set_iter(ht_tab, &mut iter) >= 0 {
        loop {
            let mut key = iter.get_key();

            if !filter.is_null() {
                let new_obj: *mut pdf_obj = ht_lookup_table(filter, key) as *mut pdf_obj;
                if new_obj.is_null() {
                    if !(ht_iter_next(&mut iter) >= 0) {
                        break;
                    }
                    continue;
                }
                key = (*new_obj).as_string().to_bytes_without_nul();
            }

            let value = ht_iter_getval(&iter) as *const obj_data;
            assert!(!(*value).object.is_null());
            objects.push(if let Object::Undefined = (*(*value).object).data {
                warn!(
                    "Object @{}\" not defined. Replaced by null.",
                    printable_key(key),
                );
                named_object {
                    key: key.as_ptr(),
                    keylen: key.len() as _,
                    value: Object::Null.into_obj(),
                }
            } else {
                named_object {
                    key: key.as_ptr(),
                    keylen: key.len() as _,
                    value: pdf_link_obj((*value).object as *mut _),
                }
            });

            if !(ht_iter_next(&mut iter) >= 0) {
                break;
            }
        }
        ht_clear_iter(&mut iter);
    }
    objects
}
/* Hash */
/* Not actually tree... */
/* Really create name tree... */

pub(crate) unsafe fn pdf_names_create_tree(
    names: *mut ht_table,
    filter: *mut ht_table,
) -> (Option<pdf_dict>, i32) {
    let mut flat = flat_table(names, filter);
    if flat.is_empty() {
        (None, flat.len() as i32)
    } else {
        flat.sort_unstable_by(cmp_key);
        let name_tree = build_name_tree(flat.as_mut_slice(), 1);
        (Some(name_tree), flat.len() as i32)
    }
}
