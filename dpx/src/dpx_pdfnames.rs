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

use crate::mfree;
use crate::warn;
use std::slice;
use std::cmp::Ordering;
use std::fmt::Write;

use super::dpx_dpxutil::{
    ht_append_table, ht_clear_iter, ht_clear_table, ht_init_table, ht_iter_getkey, ht_iter_getval,
    ht_iter_next, ht_lookup_table, ht_set_iter,
};
use super::dpx_mem::{new, renew};
use crate::dpx_pdfobj::{
    pdf_add_array, pdf_add_dict, pdf_link_obj, pdf_new_array, pdf_new_dict, pdf_new_null,
    pdf_new_string, pdf_new_undefined, pdf_obj, pdf_obj_typeof, pdf_ref_obj, pdf_release_obj,
    pdf_string_length, pdf_string_value, pdf_transfer_label, PdfObjType,
};
use libc::free;

pub type size_t = u64;
pub type __compar_fn_t =
    Option<unsafe extern "C" fn(_: *const libc::c_void, _: *const libc::c_void) -> i32>;

pub type hval_free_func = Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>;

use super::dpx_dpxutil::ht_iter;
use super::dpx_dpxutil::ht_table;
/* Hash */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct obj_data {
    pub object: *mut pdf_obj,
    pub closed: i32,
    /* 1 if object is closed */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct named_object {
    pub key: *mut i8,
    pub keylen: i32,
    pub value: *mut pdf_obj,
}
unsafe fn printable_key(key: *const i8, keylen: i32) -> String {
    let bytes = slice::from_raw_parts(key as *const u8, keylen as usize);
    let mut printable = String::with_capacity(bytes.len() * 2);
    for &b in bytes.iter() {
        if b.is_ascii_graphic() {
            write!(&mut printable, "{}", b as char);
        } else {
            write!(&mut printable, "#{:02X}", b);
        }
    }
    printable
}
#[inline]
unsafe extern "C" fn hval_free(mut hval: *mut libc::c_void) {
    let value = hval as *mut obj_data;
    if !(*value).object.is_null() {
        pdf_release_obj((*value).object);
        (*value).object = 0 as *mut pdf_obj
    }
    free(value as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn pdf_new_name_tree() -> *mut ht_table {
    let names =
        new((1_u64).wrapping_mul(::std::mem::size_of::<ht_table>() as u64) as u32) as *mut ht_table;
    ht_init_table(
        names,
        Some(hval_free as unsafe extern "C" fn(_: *mut libc::c_void) -> ()),
    );
    names
}
unsafe fn check_objects_defined(mut ht_tab: *mut ht_table) {
    let mut iter: ht_iter = ht_iter {
        index: 0,
        curr: 0 as *mut libc::c_void,
        hash: 0 as *mut ht_table,
    };
    if ht_set_iter(ht_tab, &mut iter) >= 0i32 {
        loop {
            let mut keylen: i32 = 0;
            let key = ht_iter_getkey(&mut iter, &mut keylen);
            let value = ht_iter_getval(&mut iter) as *mut obj_data;
            assert!(!(*value).object.is_null());
            if !(*value).object.is_null()
                && pdf_obj_typeof((*value).object) == PdfObjType::UNDEFINED
            {
                pdf_names_add_object(ht_tab, key as *const libc::c_void, keylen, pdf_new_null());
                warn!(
                    "Object @{} used, but not defined. Replaced by null.",
                    printable_key(key, keylen),
                );
            }
            if !(ht_iter_next(&mut iter) >= 0i32) {
                break;
            }
        }
        ht_clear_iter(&mut iter);
    };
}
#[no_mangle]
pub unsafe extern "C" fn pdf_delete_name_tree(mut names: *mut *mut ht_table) {
    assert!(!names.is_null() && !(*names).is_null());
    check_objects_defined(*names);
    ht_clear_table(*names);
    *names = mfree(*names as *mut libc::c_void) as *mut ht_table;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_names_add_object(
    mut names: *mut ht_table,
    mut key: *const libc::c_void,
    mut keylen: i32,
    mut object: *mut pdf_obj,
) -> i32 {
    assert!(!names.is_null() && !object.is_null());
    if key.is_null() || keylen < 1i32 {
        warn!("Null string used for name tree key.");
        return -1i32;
    }
    let mut value = ht_lookup_table(names, key, keylen) as *mut obj_data;
    if value.is_null() {
        value = new((1_u64).wrapping_mul(::std::mem::size_of::<obj_data>() as u64) as u32)
            as *mut obj_data;
        (*value).object = object;
        (*value).closed = 0i32;
        ht_append_table(names, key, keylen, value as *mut libc::c_void);
    } else {
        assert!(!(*value).object.is_null());
        if !(*value).object.is_null() && pdf_obj_typeof((*value).object) == PdfObjType::UNDEFINED {
            pdf_transfer_label(object, (*value).object);
            pdf_release_obj((*value).object);
            (*value).object = object
        } else {
            warn!(
                "Object @{} already defined.",
                printable_key(key as *const i8, keylen),
            );
            pdf_release_obj(object);
            return -1i32;
        }
    }
    0i32
}
/*
 * The following routine returns copies, not the original object.
 */
#[no_mangle]
pub unsafe extern "C" fn pdf_names_lookup_reference(
    mut names: *mut ht_table,
    mut key: *const libc::c_void,
    mut keylen: i32,
) -> *mut pdf_obj {
    let object;
    assert!(!names.is_null());
    let value = ht_lookup_table(names, key, keylen) as *mut obj_data;
    if !value.is_null() {
        object = (*value).object;
        assert!(!object.is_null());
    } else {
        /* A null object as dummy would create problems because as value
         * of a dictionary entry, a null object is be equivalent to no entry
         * at all. This matters for optimization of PDF destinations.
         */
        object = pdf_new_undefined();
        pdf_names_add_object(names, key, keylen, object);
    }
    pdf_ref_obj(object)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_names_lookup_object(
    mut names: *mut ht_table,
    mut key: *const libc::c_void,
    mut keylen: i32,
) -> *mut pdf_obj {
    assert!(!names.is_null());
    let value = ht_lookup_table(names, key, keylen) as *mut obj_data;
    if value.is_null()
        || !(*value).object.is_null() && pdf_obj_typeof((*value).object) == PdfObjType::UNDEFINED
    {
        return 0 as *mut pdf_obj;
    }
    assert!(!(*value).object.is_null());
    (*value).object
}
#[no_mangle]
pub unsafe extern "C" fn pdf_names_close_object(
    mut names: *mut ht_table,
    mut key: *const libc::c_void,
    mut keylen: i32,
) -> i32 {
    assert!(!names.is_null());
    let value = ht_lookup_table(names, key, keylen) as *mut obj_data;
    if value.is_null()
        || !(*value).object.is_null() && pdf_obj_typeof((*value).object) == PdfObjType::UNDEFINED
    {
        warn!(
            "Cannot close undefined object @{}.",
            printable_key(key as *const i8, keylen),
        );
        return -1i32;
    }
    assert!(!(*value).object.is_null());
    if (*value).closed != 0 {
        warn!(
            "Object @{} already closed.",
            printable_key(key as *const i8, keylen),
        );
        return -1i32;
    }
    (*value).closed = 1i32;
    0i32
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
unsafe fn build_name_tree(
    mut first: *mut named_object,
    mut num_leaves: i32,
    mut is_root: i32,
) -> *mut pdf_obj {
    let result = pdf_new_dict();
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
        let mut limits = pdf_new_array();
        let mut last = &mut *first.offset((num_leaves - 1i32) as isize) as *mut named_object;
        pdf_add_array(
            limits,
            pdf_new_string(
                (*first).key as *const libc::c_void,
                (*first).keylen as size_t,
            ),
        );
        pdf_add_array(
            limits,
            pdf_new_string((*last).key as *const libc::c_void, (*last).keylen as size_t),
        );
        pdf_add_dict(result, "Limits", limits);
    }
    if num_leaves > 0i32 && num_leaves <= 2i32 * 4i32 {
        /* Create leaf nodes. */
        let mut names = pdf_new_array();
        for i in 0..num_leaves {
            let cur = &mut *first.offset(i as isize) as *mut named_object;
            pdf_add_array(
                names,
                pdf_new_string((*cur).key as *const libc::c_void, (*cur).keylen as size_t),
            );
            match pdf_obj_typeof((*cur).value) {
                PdfObjType::ARRAY | PdfObjType::DICT | PdfObjType::STREAM | PdfObjType::STRING => {
                    pdf_add_array(names, pdf_ref_obj((*cur).value));
                }
                PdfObjType::OBJ_INVALID => {
                    panic!(
                        "Invalid object...: {}",
                        printable_key((*cur).key, (*cur).keylen),
                    );
                }
                _ => {
                    pdf_add_array(names, pdf_link_obj((*cur).value));
                }
            }
            pdf_release_obj((*cur).value);
            (*cur).value = 0 as *mut pdf_obj;
        }
        pdf_add_dict(result, "Names", names);
    } else if num_leaves > 0i32 {
        /* Intermediate node */
        let kids = pdf_new_array();
        for i in 0..4 {
            let start = i * num_leaves / 4i32;
            let end = (i + 1i32) * num_leaves / 4i32;
            let subtree = build_name_tree(&mut *first.offset(start as isize), end - start, 0i32);
            pdf_add_array(kids, pdf_ref_obj(subtree));
            pdf_release_obj(subtree);
        }
        pdf_add_dict(result, "Kids", kids);
    }
    result
}
unsafe fn flat_table(
    mut ht_tab: *mut ht_table,
    mut num_entries: *mut i32,
    mut filter: *mut ht_table,
) -> *mut named_object {
    let mut iter: ht_iter = ht_iter {
        index: 0,
        curr: 0 as *mut libc::c_void,
        hash: 0 as *mut ht_table,
    };
    assert!(!ht_tab.is_null());
    let mut objects = new(((*ht_tab).count as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<named_object>() as u64) as u32)
        as *mut named_object;
    let mut count = 0i32;
    if ht_set_iter(ht_tab, &mut iter) >= 0i32 {
        let mut current_block_19: u64;
        loop {
            let mut keylen: i32 = 0;
            let mut key = ht_iter_getkey(&mut iter, &mut keylen);
            if !filter.is_null() {
                let mut new_obj: *mut pdf_obj =
                    ht_lookup_table(filter, key as *const libc::c_void, keylen) as *mut pdf_obj;
                if new_obj.is_null() {
                    current_block_19 = 15240798224410183470;
                } else {
                    key = pdf_string_value(new_obj) as *mut i8;
                    keylen = pdf_string_length(new_obj) as i32;
                    current_block_19 = 12800627514080957624;
                }
            } else {
                current_block_19 = 12800627514080957624;
            }
            match current_block_19 {
                12800627514080957624 => {
                    let value = ht_iter_getval(&mut iter) as *mut obj_data;
                    assert!(!(*value).object.is_null());
                    if !(*value).object.is_null()
                        && pdf_obj_typeof((*value).object) == PdfObjType::UNDEFINED
                    {
                        warn!(
                            "Object @{}\" not defined. Replaced by null.",
                            printable_key(key, keylen),
                        );
                        let ref mut fresh4 = (*objects.offset(count as isize)).key;
                        *fresh4 = key;
                        (*objects.offset(count as isize)).keylen = keylen;
                        let ref mut fresh5 = (*objects.offset(count as isize)).value;
                        *fresh5 = pdf_new_null()
                    } else if !(*value).object.is_null() {
                        let ref mut fresh6 = (*objects.offset(count as isize)).key;
                        *fresh6 = key;
                        (*objects.offset(count as isize)).keylen = keylen;
                        let ref mut fresh7 = (*objects.offset(count as isize)).value;
                        *fresh7 = pdf_link_obj((*value).object)
                    }
                    count += 1
                }
                _ => {}
            }
            if !(ht_iter_next(&mut iter) >= 0i32) {
                break;
            }
        }
        ht_clear_iter(&mut iter);
    }
    *num_entries = count;
    objects = renew(
        objects as *mut libc::c_void,
        (count as u32 as u64).wrapping_mul(::std::mem::size_of::<named_object>() as u64) as u32,
    ) as *mut named_object;
    objects
}
/* Hash */
/* Not actually tree... */
/* Really create name tree... */
#[no_mangle]
pub unsafe extern "C" fn pdf_names_create_tree(
    mut names: *mut ht_table,
    mut count: *mut i32,
    mut filter: *mut ht_table,
) -> *mut pdf_obj {
    let name_tree;
    let flat = flat_table(names, count, filter);
    if flat.is_null() {
        name_tree = 0 as *mut pdf_obj
    } else {
        slice::from_raw_parts_mut(
            flat,
            *count as usize,
        ).sort_unstable_by(cmp_key);
        name_tree = build_name_tree(flat, *count, 1i32);
        free(flat as *mut libc::c_void);
    }
    name_tree
}
