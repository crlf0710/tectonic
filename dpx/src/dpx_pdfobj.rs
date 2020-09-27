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

use crate::bridge::DisplayExt;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::{Read, Seek, SeekFrom, Write};

use crate::dpx_pdfparse::{ParseNumber, ParsePdfObj, SkipWhite};
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_mem::{new, renew};
use super::dpx_mfileio::{tt_mfgets, work_buffer};
use super::dpx_pdfdev::pdf_sprint_number;
use super::dpx_pdfencrypt::{pdf_enc_set_generation, pdf_enc_set_label, pdf_encrypt_data};
use super::dpx_pdfparse::skip_white;
use crate::bridge::{
    ttstub_input_get_size, ttstub_input_getc, ttstub_output_close, ttstub_output_open,
    ttstub_output_open_stdout, ttstub_output_putc,
};
use libc::{free, memset, strlen, strtoul};

use libz_sys as libz;

use bridge::{size_t, InFile, OutputHandleWrapper};

pub(crate) const STREAM_COMPRESS: i32 = 1 << 0;
pub(crate) const STREAM_USE_PREDICTOR: i32 = 1 << 1;

/// Objects with this flag will not be put into an object stream.
/// For instance, all stream objects have this flag set.
const OBJ_NO_OBJSTM: i32 = 1 << 0;
/// Objects with this flag will not be encrypted.
/// This implies OBJ_NO_OBJSTM if encryption is turned on.
const OBJ_NO_ENCRYPT: i32 = 1 << 1;

/// (label, generation)
pub(crate) type ObjectId = (u32, u16);

pub struct pdf_obj {
    pub(crate) id: ObjectId,
    pub(crate) refcount: u32,
    pub(crate) flags: i32,
    pub(crate) data: PdfObjVariant,
}

impl pdf_obj {
    pub(crate) fn typ(&self) -> PdfObjType {
        match self.data {
            PdfObjVariant::BOOLEAN(_) => PdfObjType::BOOLEAN,
            PdfObjVariant::NUMBER(_) => PdfObjType::NUMBER,
            PdfObjVariant::STRING(_) => PdfObjType::STRING,
            PdfObjVariant::NAME(_) => PdfObjType::NAME,
            PdfObjVariant::ARRAY(_) => PdfObjType::ARRAY,
            PdfObjVariant::DICT(_) => PdfObjType::DICT,
            PdfObjVariant::STREAM(_) => PdfObjType::STREAM,
            PdfObjVariant::INDIRECT(_) => PdfObjType::INDIRECT,
            PdfObjVariant::NULL => PdfObjType::NULL,
            PdfObjVariant::UNDEFINED => PdfObjType::UNDEFINED,
            PdfObjVariant::OBJ_INVALID => PdfObjType::OBJ_INVALID,
        }
    }
    pub(crate) fn label(&self) -> u32 {
        self.id.0
    }
    pub(crate) fn generation(&self) -> u16 {
        self.id.1
    }
}

#[derive(Debug)]
pub(crate) enum PdfObjVariant {
    OBJ_INVALID,
    UNDEFINED,
    NULL,
    BOOLEAN(bool),
    NUMBER(*mut pdf_number),
    STRING(*mut pdf_string),
    NAME(*mut pdf_name),
    ARRAY(*mut pdf_array),
    DICT(*mut pdf_dict),
    STREAM(*mut pdf_stream),
    INDIRECT(*mut pdf_indirect),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum PdfObjType {
    BOOLEAN,
    NUMBER,
    STRING,
    NAME,
    ARRAY,
    DICT,
    STREAM,
    NULL,
    INDIRECT,
    UNDEFINED,
    OBJ_INVALID,
}

impl pdf_obj {
    pub(crate) fn is_bool(&self) -> bool {
        self.typ() == PdfObjType::BOOLEAN
    }
    pub(crate) fn is_number(&self) -> bool {
        self.typ() == PdfObjType::NUMBER
    }
    pub(crate) fn is_string(&self) -> bool {
        self.typ() == PdfObjType::STRING
    }
    pub(crate) fn is_name(&self) -> bool {
        self.typ() == PdfObjType::NAME
    }
    pub(crate) fn is_array(&self) -> bool {
        self.typ() == PdfObjType::ARRAY
    }
    pub(crate) fn is_dict(&self) -> bool {
        self.typ() == PdfObjType::DICT
    }
    pub(crate) fn is_stream(&self) -> bool {
        self.typ() == PdfObjType::STREAM
    }
    pub(crate) fn is_indirect(&self) -> bool {
        self.typ() == PdfObjType::INDIRECT
    }
    pub(crate) unsafe fn as_bool(&self) -> bool {
        if let PdfObjVariant::BOOLEAN(v) = self.data {
            v
        } else {
            panic!("invalid pdfobj::as_bool");
        }
    }
    pub(crate) unsafe fn as_f64(&self) -> f64 {
        if let PdfObjVariant::NUMBER(v) = self.data {
            (*v).value
        } else {
            panic!("invalid pdfobj::as_f64");
        }
    }
    pub(crate) unsafe fn as_dict(&self) -> &pdf_dict {
        if let PdfObjVariant::DICT(v) = self.data {
            &*v
        } else {
            panic!("invalid pdfobj::as_dict");
        }
    }
    pub(crate) unsafe fn as_dict_mut(&mut self) -> &mut pdf_dict {
        if let PdfObjVariant::DICT(v) = self.data {
            &mut *v
        } else {
            panic!("pdfobj::as_dict_mut on {:?}", self.typ());
        }
    }
    pub(crate) unsafe fn as_array(&self) -> &Vec<*mut Self> {
        if let PdfObjVariant::ARRAY(v) = self.data {
            &(*v).values
        } else {
            panic!("invalid pdfobj::as_array");
        }
    }
    pub(crate) unsafe fn as_array_mut(&mut self) -> &mut Vec<*mut Self> {
        if let PdfObjVariant::ARRAY(v) = self.data {
            &mut (*v).values
        } else {
            panic!("invalid pdfobj::as_array_mut");
        }
    }
    pub(crate) unsafe fn as_stream(&self) -> &pdf_stream {
        if let PdfObjVariant::STREAM(v) = self.data {
            &*v
        } else {
            panic!("invalid pdfobj::as_stream");
        }
    }
    pub(crate) unsafe fn as_stream_mut(&mut self) -> &mut pdf_stream {
        if let PdfObjVariant::STREAM(v) = self.data {
            &mut *v
        } else {
            panic!("invalid pdfobj::as_stream_mut");
        }
    }
    pub(crate) unsafe fn as_string(&self) -> &pdf_string {
        if let PdfObjVariant::STRING(v) = self.data {
            &*v
        } else {
            panic!("invalid pdfobj::as_string");
        }
    }
    pub(crate) unsafe fn as_name(&self) -> &CStr {
        if let PdfObjVariant::NAME(v) = self.data {
            (*v).name.as_c_str()
        } else {
            panic!("invalid pdfobj::as_name");
        }
    }
    pub(crate) unsafe fn as_indirect(&self) -> &pdf_indirect {
        if let PdfObjVariant::INDIRECT(v) = self.data {
            &*v
        } else {
            panic!("invalid pdfobj::as_indirect");
        }
    }
}

#[repr(C)]
pub struct pdf_file {
    pub(crate) handle: InFile,
    pub(crate) trailer: *mut pdf_obj,
    pub(crate) xref_table: *mut xref_entry,
    pub(crate) catalog: *mut pdf_obj,
    pub(crate) num_obj: i32,
    pub(crate) file_size: i32,
    pub(crate) version: u32,
    /* External interface to pdf routines */
    /* Name does not include the / */
    /* pdf_add_dict requires key but pdf_add_array does not.
     * pdf_add_array always append elements to array.
     * They should be pdf_put_array(array, idx, element) and
     * pdf_put_dict(dict, key, value)
     */
    /* pdf_add_dict() want pdf_obj as key, however, key must always be name
     * object and pdf_lookup_dict() and pdf_remove_dict() uses const char as
     * key. This strange difference seems come from pdfdoc that first allocate
     * name objects frequently used (maybe 1000 times) such as /Type and does
     * pdf_link_obj() it rather than allocate/free-ing them each time. But I
     * already removed that.
     */
    /* Apply proc(key, value, pdata) for each key-value pairs in dict, stop if proc()
     * returned non-zero value (and that value is returned). PDF object is passed for
     * key to allow modification (fix) of key.
     */
    /* Compare label of two indirect reference object.
     */
    /* The following routines are not appropriate for pdfobj.
     */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct xref_entry {
    pub(crate) typ: u8,
    pub(crate) id: ObjectId,
    pub(crate) direct: *mut pdf_obj,
    pub(crate) indirect: *mut pdf_obj,
}
impl Default for xref_entry {
    fn default() -> Self {
        xref_entry {
            typ: 0,
            id: (0, 0),
            direct: ptr::null_mut(),
            indirect: ptr::null_mut(),
        }
    }
}

use indexmap::IndexMap;

#[repr(C)]
pub(crate) struct pdf_dict {
    inner: IndexMap<pdf_name, *mut pdf_obj>,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct pdf_stream {
    pub(crate) dict: *mut pdf_obj,
    pub(crate) content: Vec<u8>,
    pub(crate) objstm_data: *mut i32,
    pub(crate) _flags: i32,
    pub(crate) decodeparms: decode_parms,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct decode_parms {
    pub(crate) predictor: i32,
    pub(crate) colors: i32,
    pub(crate) bits_per_component: i32,
    pub(crate) columns: i32,
}
#[derive(Clone, PartialEq, Eq)]
pub(crate) struct pdf_name {
    pub(crate) name: CString,
}

impl std::hash::Hash for pdf_name {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.to_bytes().hash(state)
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pdf_indirect {
    pub(crate) pf: *mut pdf_file,
    pub(crate) obj: *mut pdf_obj,
    pub(crate) id: ObjectId,
}

impl pdf_indirect {
    pub(crate) fn new(pf: *mut pdf_file, id: ObjectId) -> Self {
        Self {
            pf,
            obj: ptr::null_mut(),
            id,
        }
    }
}

#[derive(Clone)]
#[repr(C)]
pub(crate) struct pdf_array {
    pub(crate) values: Vec<*mut pdf_obj>,
}
#[derive(Clone, PartialEq, Eq)]
#[repr(C)]
pub(crate) struct pdf_string {
    pub(crate) string: Vec<u8>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pdf_number {
    pub(crate) value: f64,
}

// Must be replaced with std::convert::From
pub(crate) trait IntoObj {
    fn into_obj(self) -> *mut pdf_obj;
}
impl IntoObj for *mut pdf_obj {
    #[inline(always)]
    fn into_obj(self) -> Self {
        self
    }
}

impl IntoObj for f64 {
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::new(pdf_number { value: self });
        pdf_new_obj(PdfObjVariant::NUMBER(Box::into_raw(data)))
    }
}

impl IntoObj for bool {
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj {
        pdf_new_obj(PdfObjVariant::BOOLEAN(self))
    }
}

impl IntoObj for &str {
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj {
        pdf_name::new(self).into_obj()
    }
}

impl IntoObj for Vec<*mut pdf_obj> {
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::new(pdf_array { values: self });
        pdf_new_obj(PdfObjVariant::ARRAY(Box::into_raw(data)))
    }
}

impl IntoObj for pdf_name {
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::into_raw(Box::new(self));
        pdf_new_obj(PdfObjVariant::NAME(data))
    }
}

impl IntoObj for pdf_string {
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::into_raw(Box::new(self));
        pdf_new_obj(PdfObjVariant::STRING(data))
    }
}

impl IntoObj for pdf_stream {
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::into_raw(Box::new(self));
        let result = pdf_new_obj(PdfObjVariant::STREAM(data));
        /*
         * Although we are using an arbitrary pdf_object here, it must have
         * type=PDF_DICT and cannot be an indirect reference.  This will be
         * checked by the output routine.
         */
        unsafe { (*result).flags |= OBJ_NO_OBJSTM };
        result
    }
}

impl IntoObj for pdf_dict {
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::into_raw(Box::new(self));
        pdf_new_obj(PdfObjVariant::DICT(data))
    }
}

impl IntoObj for pdf_indirect {
    fn into_obj(self) -> *mut pdf_obj {
        let data = Box::new(self);
        pdf_new_obj(PdfObjVariant::INDIRECT(Box::into_raw(data)))
    }
}

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut pdf_output_handle: Option<OutputHandleWrapper> = None;
static mut pdf_output_file_position: usize = 0;
static mut pdf_output_line_position: usize = 0;
static mut compression_saved: i32 = 0i32;
static mut output_xref: Vec<xref_entry> = Vec::new();
static mut pdf_max_ind_objects: usize = 0;
static mut next_label: usize = 0;
static mut startxref: u32 = 0;
static mut output_stream: *mut pdf_obj = ptr::null_mut();
/* the limit is only 100 for linearized PDF */
static mut enc_mode: bool = false;
static mut doc_enc_mode: bool = false;
static mut trailer_dict: *mut pdf_obj = ptr::null_mut();
static mut xref_stream: *mut pdf_obj = ptr::null_mut();
static mut verbose: i32 = 0i32;
static mut compression_level: i8 = 9_i8;
static mut compression_use_predictor: i8 = 1_i8;

pub(crate) unsafe fn pdf_set_compression(level: i32) {
    if cfg!(not(feature = "libz-sys")) {
        panic!(
            "You don\'t have compression compiled in. Possibly libz wasn\'t found by configure."
        );
    }
    if cfg!(feature = "legacy-libz") && level != 0i32 {
        warn!("Unable to set compression level -- your zlib doesn\'t have compress2().");
    }
    if level >= 0i32 && level <= 9i32 {
        compression_level = level as i8
    } else {
        panic!("set_compression: invalid compression level: {}", level);
    };
}

pub(crate) unsafe fn pdf_set_use_predictor(bval: i32) {
    compression_use_predictor = (if bval != 0 { 1i32 } else { 0i32 }) as i8;
}
static mut pdf_version: u32 = 5_u32;

pub(crate) unsafe fn pdf_set_version(version: u32) {
    /* Don't forget to update CIDFont_stdcc_def[] in cid.c too! */
    if version >= 3_u32 && version <= 7_u32 {
        pdf_version = version
    };
}

pub(crate) unsafe fn pdf_get_version() -> u32 {
    pdf_version
}

pub(crate) unsafe fn pdf_obj_get_verbose() -> i32 {
    verbose
}

pub(crate) unsafe fn pdf_obj_set_verbose(level: i32) {
    verbose = level;
}
static mut current_objstm: *mut pdf_obj = ptr::null_mut(); // TODO: replace with Option<pdf_stream>
static mut do_objstm: i32 = 0;
unsafe fn add_xref_entry(label: usize, typ: u8, id: ObjectId) {
    if label >= pdf_max_ind_objects {
        pdf_max_ind_objects = (label / 512 + 1) * 512;
        output_xref.resize_with(pdf_max_ind_objects, Default::default);
    }
    output_xref[label] = xref_entry {
        typ,
        id,
        direct: ptr::null_mut(),
        indirect: ptr::null_mut(),
    }
}

pub(crate) unsafe fn pdf_out_init(
    filename: *const i8,
    do_encryption: bool,
    enable_object_stream: bool,
) {
    output_xref = vec![];
    pdf_max_ind_objects = 0;
    add_xref_entry(0, 0_u8, (0_u32, 0xffff_u16));
    next_label = 1;
    if pdf_version >= 5_u32 {
        if enable_object_stream {
            xref_stream = pdf_stream::new(STREAM_COMPRESS).into_obj();
            (*xref_stream).flags |= OBJ_NO_ENCRYPT;
            trailer_dict = (*xref_stream).as_stream_mut().get_dict_obj();
            (*trailer_dict).as_dict_mut().set("Type", "XRef");
            do_objstm = 1i32
        } else {
            trailer_dict = pdf_dict::new().into_obj();
            do_objstm = 0i32
        }
    } else {
        xref_stream = ptr::null_mut();
        trailer_dict = pdf_dict::new().into_obj();
        do_objstm = 0i32
    }
    output_stream = ptr::null_mut();
    if filename.is_null() {
        panic!("stdout PDF output not supported");
    }
    pdf_output_handle = ttstub_output_open(filename, 0i32);
    if pdf_output_handle.is_none() {
        if strlen(filename) < 128 {
            panic!("Unable to open \"{}\".", CStr::from_ptr(filename).display());
        } else {
            panic!("Unable to open file.");
        }
    }
    let handle = pdf_output_handle.as_mut().unwrap();
    pdf_out(handle, b"%PDF-1.");
    let v = [b'0' + (pdf_version as u8)];
    pdf_out(handle, &v[..]);
    pdf_out(handle, b"\n");
    pdf_out(handle, b"%\xe4\xf0\xed\xf8\n");
    enc_mode = false;
    doc_enc_mode = do_encryption;
}
unsafe fn dump_xref_table() {
    let handle = pdf_output_handle.as_mut().unwrap();
    pdf_out(handle, b"xref\n");
    let out = format!("{} {}\n", 0, next_label);
    pdf_out(handle, out.as_bytes());
    /*
     * Every space counts.  The space after the 'f' and 'n' is * *essential*.
     * The PDF spec says the lines must be 20 characters long including the
     * end of line character.
     */
    for i in 0..next_label {
        let typ: u8 = output_xref[i].typ;
        if typ > 1 {
            panic!("object type {} not allowed in xref table", char::from(typ));
        }
        let out = format!(
            "{:010} {:05} {} \n",
            output_xref[i].id.0,
            output_xref[i].id.1,
            if typ != 0 { 'n' } else { 'f' },
        );
        pdf_out(handle, out.as_bytes());
    }
}
unsafe fn dump_trailer_dict() {
    let handle = pdf_output_handle.as_mut().unwrap();
    pdf_out(handle, b"trailer\n");
    enc_mode = false;
    write_dict((*trailer_dict).as_dict(), handle);
    pdf_release_obj(trailer_dict);
    pdf_out_char(handle, b'\n');
}
/*
 * output a PDF 1.5 cross-reference stream;
 * contributed by Matthias Franz (March 21, 2007)
 */
unsafe fn dump_xref_stream() {
    let mut buf: [u8; 7] = [0; 7];
    /* determine the necessary size of the offset field */
    let mut pos = startxref; /* maximal offset value */
    let mut poslen = 1_u32; /* type                */
    loop {
        pos >>= 8i32; /* offset (big-endian) */
        if !(pos != 0) {
            break; /* generation          */
        }
        poslen = poslen.wrapping_add(1)
    }
    let mut w = vec![];
    w.push_obj(1_f64);
    w.push_obj(poslen as f64);
    w.push_obj(2_f64);
    (*trailer_dict).as_dict_mut().set("W", w);
    /* We need the xref entry for the xref stream right now */
    add_xref_entry(next_label - 1, 1_u8, (startxref, 0_u16));
    for i in 0..next_label {
        buf[0] = output_xref[i].typ;
        pos = output_xref[i].id.0;
        for j in (0..poslen).rev() {
            buf[(1 + j) as usize] = pos as u8;
            pos >>= 8;
        }
        let f3 = output_xref[i].id.1;
        buf[poslen.wrapping_add(1_u32) as usize] = (f3 as i32 >> 8i32) as u8;
        buf[poslen.wrapping_add(2_u32) as usize] = f3 as u8;
        (*xref_stream)
            .as_stream_mut()
            .add_slice(&buf[..(poslen.wrapping_add(3_u32) as usize)]);
    }
    pdf_release_obj(xref_stream);
}

pub(crate) unsafe fn pdf_out_flush() {
    if let Some(handle) = pdf_output_handle.as_mut() {
        /* Flush current object stream */
        if !current_objstm.is_null() {
            release_objstm(current_objstm);
            current_objstm = ptr::null_mut()
        }
        /*
         * Label xref stream - we need the number of correct objects
         * for the xref stream dictionary (= trailer).
         * Labelling it in pdf_out_init (with 1)  does not work (why?).
         */
        if !xref_stream.is_null() {
            pdf_label_obj(xref_stream);
        }
        /* Record where this xref is for trailer */
        startxref = pdf_output_file_position as u32;
        (*trailer_dict).as_dict_mut().set("Size", next_label as f64);
        if !xref_stream.is_null() {
            dump_xref_stream();
        } else {
            dump_xref_table();
            dump_trailer_dict();
        }
        /* Done with xref table */
        output_xref = vec![];
        pdf_out(handle, b"startxref\n");
        let out = format!("{}\n", startxref);
        pdf_out(handle, out.as_bytes());
        pdf_out(handle, b"%%EOF\n");
        if verbose != 0 {
            if compression_level as i32 > 0i32 {
                info!(
                    "Compression saved {} bytes{}\n",
                    compression_saved,
                    if pdf_version < 5_u32 {
                        ". Try \"-V 5\" for better compression"
                    } else {
                        ""
                    },
                );
            }
        }
        ttstub_output_close(pdf_output_handle.take().unwrap());
    };
}

pub(crate) unsafe fn pdf_error_cleanup() {
    /*
     * This routine is the cleanup required for an abnormal exit.
     * For now, simply close the file.
     */
    if pdf_output_handle.is_some() {
        ttstub_output_close(pdf_output_handle.take().unwrap());
    };
}

pub(crate) unsafe fn pdf_set_root(mut object: *mut pdf_obj) {
    if (*trailer_dict)
        .as_dict_mut()
        .set("Root", pdf_ref_obj(object))
        != 0
    {
        panic!("Root object already set!");
    }
    /* Adobe Readers don't like a document catalog inside an encrypted
     * object stream, although the PDF v1.5 spec seems to allow this.
     * Note that we don't set OBJ_NO_ENCRYPT since the name dictionary in
     * a document catalog may contain strings, which should be encrypted.
     */
    if doc_enc_mode {
        (*object).flags |= OBJ_NO_OBJSTM;
    };
}
pub(crate) unsafe fn pdf_set_info(object: *mut pdf_obj) {
    if (*trailer_dict)
        .as_dict_mut()
        .set("Info", pdf_ref_obj(object))
        != 0
    {
        panic!("Info object already set!");
    };
}
pub(crate) unsafe fn pdf_set_id(id: Vec<*mut pdf_obj>) {
    if (*trailer_dict).as_dict_mut().set("ID", id) != 0 {
        panic!("ID already set!");
    };
}
pub(crate) unsafe fn pdf_set_encrypt(mut encrypt: *mut pdf_obj) {
    if (*trailer_dict)
        .as_dict_mut()
        .set("Encrypt", pdf_ref_obj(encrypt))
        != 0
    {
        panic!("Encrypt object already set!");
    }
    (*encrypt).flags |= OBJ_NO_ENCRYPT;
}
unsafe fn pdf_out_char(handle: &mut OutputHandleWrapper, c: u8) {
    if !output_stream.is_null() && handle == pdf_output_handle.as_mut().unwrap() {
        (*output_stream).as_stream_mut().add_slice([c].as_ref());
    } else {
        ttstub_output_putc(handle, c as i32);
        /* Keep tallys for xref table *only* if writing a pdf file. */
        if pdf_output_handle.is_some() {
            pdf_output_file_position += 1;
            if c == b'\n' {
                pdf_output_line_position = 0
            } else {
                pdf_output_line_position += 1
            }
        }
    };
}
const xchar: &[u8; 17] = b"0123456789abcdef\x00";

unsafe fn pdf_out(handle: &mut OutputHandleWrapper, buffer: &[u8]) {
    let length = buffer.len();
    if !output_stream.is_null() && handle == pdf_output_handle.as_mut().unwrap() {
        (*output_stream).as_stream_mut().add_slice(buffer);
    } else {
        handle.write(buffer).unwrap();
        /* Keep tallys for xref table *only* if writing a pdf file */
        if pdf_output_handle.is_some() {
            pdf_output_file_position += length;
            pdf_output_line_position += length;
            /* "foo\nbar\n "... */
            if length > 0 && buffer[length - 1] == b'\n' {
                pdf_output_line_position = 0
            }
        }
    };
}
/*  returns 1 if a white-space character is necessary to separate
an object of type1 followed by an object of type2              */
unsafe fn pdf_need_white(type1: PdfObjType, type2: PdfObjType) -> bool {
    use PdfObjType::*;
    return !(type1 == STRING
        || type1 == ARRAY
        || type1 == DICT
        || type2 == STRING
        || type2 == NAME
        || type2 == ARRAY
        || type2 == DICT);
}
unsafe fn pdf_out_white(handle: &mut OutputHandleWrapper) {
    if handle == pdf_output_handle.as_mut().unwrap() && pdf_output_line_position >= 80 {
        pdf_out_char(handle, b'\n');
    } else {
        pdf_out_char(handle, b' ');
    };
}

fn pdf_new_obj(data: PdfObjVariant) -> *mut pdf_obj {
    Box::into_raw(Box::new(pdf_obj {
        data: data,
        id: (0, 0),
        refcount: 1_u32,
        flags: 0i32,
    }))
}

unsafe fn pdf_label_obj(mut object: *mut pdf_obj) {
    if object.is_null() || (*object).typ() == PdfObjType::OBJ_INVALID {
        panic!("pdf_label_obj(): passed invalid object.");
    }
    /*
     * Don't change label on an already labeled object. Ignore such calls.
     */
    if (*object).label() == 0 {
        (*object).id = (next_label as u32, 0);
        next_label += 1;
    };
}
/*
 * Transfer the label assigned to the object src to the object dst.
 * The object dst must not yet have been labeled.
 */

pub(crate) unsafe fn pdf_transfer_label(mut dst: *mut pdf_obj, mut src: *mut pdf_obj) {
    assert!(!dst.is_null() && (*dst).label() == 0 && !src.is_null());
    (*dst).id = (*src).id;
    (*src).id = (0, 0);
}
/*
 * This doesn't really copy the object, but allows it to be used without
 * fear that somebody else will free it.
 */

pub(crate) unsafe fn pdf_link_obj(mut object: *mut pdf_obj) -> *mut pdf_obj {
    if object.is_null() || (*object).typ() == PdfObjType::OBJ_INVALID {
        panic!("pdf_link_obj(): passed invalid object.");
    }
    (*object).refcount += 1;
    object
}

pub(crate) unsafe fn pdf_ref_obj(object: *mut pdf_obj) -> *mut pdf_obj {
    if object.is_null() || (*object).typ() == PdfObjType::OBJ_INVALID {
        panic!("pdf_ref_obj(): passed invalid object.");
    }
    if (*object).refcount == 0_u32 {
        info!("\nTrying to refer already released object!!!\n");
        pdf_write_obj(object, ttstub_output_open_stdout().as_mut().unwrap());
        panic!("Cannot continue...");
    }
    if !object.is_null() && (*object).is_indirect() {
        return pdf_link_obj(object);
    } else {
        return pdf_new_ref(object);
    };
}
unsafe fn write_indirect(indirect: *mut pdf_indirect, handle: &mut OutputHandleWrapper) {
    assert!((*indirect).pf.is_null());
    let (label, generation) = (*indirect).id;
    pdf_out(handle, format!("{} {} R", label, generation).as_bytes());
}
/* The undefined object is used as a placeholder in pdfnames.c
 * for objects which are referenced before they are defined.
 */

pub(crate) fn pdf_new_undefined() -> *mut pdf_obj {
    pdf_new_obj(PdfObjVariant::UNDEFINED)
}

pub(crate) fn pdf_new_null() -> *mut pdf_obj {
    pdf_new_obj(PdfObjVariant::NULL)
}

unsafe fn write_null(handle: &mut OutputHandleWrapper) {
    pdf_out(handle, b"null");
}

unsafe fn write_boolean(data: bool, handle: &mut OutputHandleWrapper) {
    pdf_out(handle, if data { b"true" } else { b"false" });
}

unsafe fn write_number(number: *mut pdf_number, handle: &mut OutputHandleWrapper) {
    let mut buf = Vec::new();
    pdf_sprint_number(&mut buf, (*number).value);
    pdf_out(handle, &buf);
}

pub(crate) unsafe fn pdf_set_number(object: &mut pdf_obj, value: f64) {
    if let PdfObjVariant::NUMBER(v) = object.data {
        (*v).value = value;
    } else {
        panic!("pdf_set_number on type {:?}", object.typ());
    }
}

impl pdf_string {
    pub(crate) fn new<K>(from: K) -> Self
    where
        K: AsRef<[u8]>,
    {
        let mut string = Vec::from(from.as_ref());
        string.push(0);
        Self { string }
    }
    pub(crate) unsafe fn new_from_ptr(ptr: *const libc::c_void, length: size_t) -> Self {
        if ptr.is_null() {
            Self::new(&[])
        } else {
            Self::new(std::slice::from_raw_parts(
                ptr as *const u8,
                length as usize,
            ))
        }
    }
    pub(crate) fn set<K>(&mut self, from: K)
    where
        K: AsRef<[u8]>,
    {
        self.string = Vec::from(from.as_ref());
        self.string.push(0);
    }
    pub(crate) fn len(&self) -> usize {
        self.string.len() - 1
    }
    pub(crate) fn to_bytes(&self) -> &[u8] {
        &self.string[..self.len()]
    }
    pub(crate) fn to_bytes_without_nul(&self) -> &[u8] {
        let pos = self
            .string
            .iter()
            .position(|&x| x == 0)
            .unwrap_or(self.len());
        &self.string[..pos]
    }
    pub(crate) fn as_mut_slice(&mut self) -> &mut [u8] {
        let len = self.len();
        &mut self.string[..len]
    }
}

pub(crate) unsafe fn pdf_string_value(object: &pdf_obj) -> *mut libc::c_void {
    let data = object.as_string();
    if data.len() == 0 {
        ptr::null_mut()
    } else {
        data.string.as_ptr() as *mut u8 as *mut libc::c_void
    }
}

/*
 * This routine escapes non printable characters and control
 * characters in an output string.
 */

pub(crate) unsafe fn pdfobj_escape_str(buffer: &mut Vec<u8>, s: *const u8, len: size_t) {
    for i in 0..len {
        let ch = *s.offset(i as isize);
        /*
         * We always write three octal digits. Optimization only gives few Kb
         * smaller size for most documents when zlib compressed.
         */
        if ch < 32 || ch > 126 {
            buffer.push(b'\\');
            write!(buffer, "{:03o}", ch).unwrap();
        } else {
            match ch {
                40 => {
                    buffer.push(b'\\');
                    buffer.push(b'(');
                }
                41 => {
                    buffer.push(b'\\');
                    buffer.push(b')');
                }
                92 => {
                    buffer.push(b'\\');
                    buffer.push(b'\\');
                }
                _ => {
                    buffer.push(ch);
                }
            }
        }
    }
}
unsafe fn write_string(strn: &pdf_string, handle: &mut OutputHandleWrapper) {
    let mut s: *mut u8 = ptr::null_mut();
    let mut nescc: i32 = 0i32;
    let mut len: size_t = 0i32 as size_t;
    if enc_mode {
        pdf_encrypt_data(
            strn.string.as_ptr() as *const u8,
            strn.len() as size_t,
            &mut s,
            &mut len,
        );
    } else {
        s = strn.string.as_ptr() as *const u8 as *mut u8;
        len = strn.len() as size_t;
    }
    /*
     * Count all ASCII non-printable characters.
     */
    for i in 0..len {
        if libc::isprint(*s.offset(i as isize) as _) == 0 {
            nescc += 1
        }
    }
    /*
     * If the string contains much escaped chars, then we write it as
     * ASCII hex string.
     */
    if nescc as u64 > len.wrapping_div(3) as _ {
        pdf_out_char(handle, b'<');
        for i in 0..len {
            pdf_out_char(
                handle,
                xchar[(*s.offset(i as isize) as i32 >> 4i32 & 0xfi32) as usize],
            );
            pdf_out_char(
                handle,
                xchar[(*s.offset(i as isize) as i32 & 0xfi32) as usize],
            );
        }
        pdf_out_char(handle, b'>');
    } else {
        pdf_out_char(handle, b'(');
        /*
         * This section of code probably isn't speed critical.  Escaping the
         * characters in the string one at a time may seem slow, but it's
         * safe if the formatted string length exceeds FORMAT_BUF_SIZE.
         * Occasionally you see some long strings in PDF.  pdfobj_escape_str
         * is also used for strings of text with no kerning.  These must be
         * handled as quickly as possible since there are so many of them.
         */
        let mut wbuf = Vec::new();
        for i in 0..len {
            pdfobj_escape_str(&mut wbuf, &mut *s.offset(i as isize), 1i32 as size_t);
            pdf_out(handle, wbuf.as_slice());
            wbuf.clear();
        }
        pdf_out_char(handle, b')');
    }
    if enc_mode as i32 != 0 && !s.is_null() {
        free(s as *mut libc::c_void);
    };
}

/* Name does *not* include the /. */

unsafe fn write_name(name: &pdf_name, handle: &mut OutputHandleWrapper) {
    let cstr = name.name.as_c_str();
    /*
     * From PDF Reference, 3rd ed., p.33:
     *
     *  Beginning with PDF 1.2, any character except null (character code 0)
     *  may be included in a name by writing its 2-digit hexadecimal code,
     *  preceded bythe number sign character (#); see implementation notes 3
     *  and 4 in Appendix H. This syntax is required in order to represent
     *  any of the delimiter or white-space characters or the number sign
     *  character itself; it is recommended but not required for characters
     *  whose codes are outside the range 33 (!) to 126 (~).
     */
    pdf_out_char(handle, b'/');
    for &byte in cstr.to_bytes() {
        if byte < b'!' || byte > b'~' || b"#()/<>[]{}%".contains(&byte) {
            pdf_out_char(handle, b'#');
            pdf_out_char(handle, xchar[((byte as i32) >> 4 & 0xf) as usize] as u8);
            pdf_out_char(handle, xchar[(byte as i32 & 0xf) as usize] as u8);
        } else {
            pdf_out_char(handle, byte);
        }
    }
}

/*
 * We do not have pdf_name_length() since '\0' is not allowed
 * in PDF name object.
 */

unsafe fn write_array(array: *mut pdf_array, handle: &mut OutputHandleWrapper) {
    pdf_out_char(handle, b'[');
    if !(*array).values.is_empty() {
        let mut type1 = PdfObjType::UNDEFINED;
        for i in 0..(*array).values.len() {
            if !(*array).values[i as usize].is_null() {
                let type2 = (*(*array).values[i as usize]).typ();
                if type1 != PdfObjType::UNDEFINED && pdf_need_white(type1, type2) {
                    pdf_out_white(handle);
                }
                type1 = type2;
                pdf_write_obj((*array).values[i as usize], handle);
            } else {
                warn!("PDF array element {} undefined.", i);
            }
        }
    }
    pdf_out_char(handle, b']');
}
impl pdf_array {
    pub(crate) unsafe fn get(&self, idx: i32) -> Option<&pdf_obj> {
        let len = self.values.len();
        if idx < 0 {
            Some(&*self.values[len - (idx.abs() as usize)])
        } else if (idx as usize) < len {
            Some(&*self.values[idx as usize])
        } else {
            None
        }
    }
    pub(crate) unsafe fn get_mut(&mut self, idx: i32) -> Option<&mut pdf_obj> {
        let len = self.values.len();
        if idx < 0 {
            Some(&mut *self.values[len - (idx.abs() as usize)])
        } else if (idx as usize) < len {
            Some(&mut *self.values[idx as usize])
        } else {
            None
        }
    }

    pub(crate) fn len(&self) -> u32 {
        self.values.len() as u32
    }

    pub(crate) fn push<O>(&mut self, object: O)
    where
        O: IntoObj,
    {
        self.values.push(object.into_obj());
    }
}

impl Drop for pdf_array {
    fn drop(&mut self) {
        let values = &mut self.values;
        for val in values.drain(..) {
            unsafe {
                pdf_release_obj(val);
            }
        }
    }
}

pub(crate) trait PushObj {
    fn push_obj<O>(&mut self, object: O)
    where
        O: IntoObj;
}

impl PushObj for Vec<*mut pdf_obj> {
    fn push_obj<O>(&mut self, object: O)
    where
        O: IntoObj,
    {
        self.push(object.into_obj());
    }
}

/* Prepend an object to an array */
unsafe fn pdf_unshift_array(array: &mut pdf_obj, object: *mut pdf_obj) {
    array.as_array_mut().insert(0, object);
}
unsafe fn write_dict(dict: &pdf_dict, handle: &mut OutputHandleWrapper) {
    pdf_out(handle, b"<<");
    for (k, &v) in dict.inner.iter() {
        write_name(k, handle);
        if pdf_need_white(PdfObjType::NAME, (*v).typ()) {
            pdf_out_white(handle);
        }
        pdf_write_obj(v, handle);
    }
    pdf_out(handle, b">>");
}

impl pdf_dict {
    pub(crate) fn new() -> Self {
        Self {
            inner: IndexMap::new(),
        }
    }
}

impl Drop for pdf_dict {
    fn drop(&mut self) {
        for (_k, v) in self.inner.drain(..) {
            unsafe {
                pdf_release_obj(v);
            }
        }
    }
}

impl pdf_dict {
    /* Array is ended by a node with NULL this pointer */
    /* pdf_add_dict returns 0 if the key is new and non-zero otherwise */
    pub(crate) unsafe fn set<K, V>(&mut self, key: K, value: V) -> i32
    where
        K: AsRef<[u8]>,
        V: IntoObj,
    {
        let value = value.into_obj();
        /* It seems that NULL is sometimes used for null object... */
        if !value.is_null() && (*value).typ() == PdfObjType::OBJ_INVALID {
            panic!("pdf_add_dict(): Passed invalid value");
        }
        /* If this key already exists, simply replace the value */
        if let Some(existing) = self.inner.insert(pdf_name::new(key.as_ref()), value) {
            pdf_release_obj(existing);
            1
        } else {
            0
        }
    }
    /* pdf_merge_dict makes a link for each item in dict2 before stealing it */
    pub(crate) unsafe fn merge(&mut self, dict2: &Self) {
        for (k, &v) in dict2.inner.iter() {
            self.set(k.to_bytes(), pdf_link_obj(v));
        }
    }
}

impl pdf_name {
    pub(crate) fn new<K: AsRef<[u8]>>(from: K) -> Self {
        let name = CString::new(from.as_ref()).unwrap();
        pdf_name { name }
    }
    pub(crate) fn to_bytes(&self) -> &[u8] {
        self.name.to_bytes()
    }
}

impl std::borrow::Borrow<[u8]> for pdf_name {
    fn borrow(&self) -> &[u8] {
        self.to_bytes()
    }
}

impl pdf_dict {
    pub(crate) unsafe fn foreach<T>(
        &mut self,
        f: unsafe fn(_: &pdf_name, _: &mut pdf_obj, _: &mut T) -> i32,
        pdata: &mut T,
    ) -> i32 {
        self.foreach_dict(
            |k, v, pdata| {
                let e = f(k, &mut *v, pdata);
                e
            },
            pdata,
        )
    }
    fn foreach_dict<F, T>(&mut self, f: F, pdata: &mut T) -> i32
    where
        F: Fn(&pdf_name, *mut pdf_obj, &mut T) -> i32,
    {
        let mut error: i32 = 0i32;
        for (k, &v) in self.inner.iter() {
            if error != 0 {
                break;
            }
            error = f(k, v, pdata);
        }
        error
    }

    pub(crate) unsafe fn has<K>(&self, name: K) -> bool
    where
        K: AsRef<[u8]>,
    {
        self.inner.contains_key(name.as_ref())
    }
    pub(crate) unsafe fn get<K>(&self, name: K) -> Option<&pdf_obj>
    where
        K: AsRef<[u8]>,
    {
        match self.inner.get(name.as_ref()) {
            Some(&x) => Some(&*x),
            None => None,
        }
    }
    pub(crate) unsafe fn get_mut<K>(&mut self, name: K) -> Option<&mut pdf_obj>
    where
        K: AsRef<[u8]>,
    {
        match self.inner.get_mut(name.as_ref()) {
            Some(&mut x) => Some(&mut *x),
            None => None,
        }
    }
}

pub(crate) unsafe fn pdf_remove_dict<K>(dict: &mut pdf_obj, name: K)
where
    K: AsRef<[u8]>,
{
    let dict = dict.as_dict_mut();
    if let Some(existing_value) = dict.inner.shift_remove(name.as_ref()) {
        pdf_release_obj(existing_value);
    }
}

impl pdf_stream {
    pub(crate) fn new(flags: i32) -> Self {
        Self {
            dict: unsafe { pdf_dict::new().into_obj() },
            _flags: flags,
            decodeparms: decode_parms {
                predictor: 2i32,
                columns: 0i32,
                bits_per_component: 0i32,
                colors: 0i32,
            },
            objstm_data: ptr::null_mut(),
            content: Vec::new(),
        }
    }
}

pub(crate) unsafe fn pdf_stream_set_predictor(
    stream: *mut pdf_obj,
    predictor: i32,
    columns: i32,
    bpc: i32,
    colors: i32,
) {
    if !(*stream).is_stream() {
        return;
    } else {
        if columns < 0i32 || bpc < 0i32 || colors < 0i32 {
            return;
        }
    }
    let stream = (*stream).as_stream_mut();
    stream.decodeparms.predictor = predictor;
    stream.decodeparms.columns = columns;
    stream.decodeparms.bits_per_component = bpc;
    stream.decodeparms.colors = colors;
    stream._flags |= STREAM_USE_PREDICTOR;
}
/* Adaptive PNG filter
 * We use the "minimum sum of absolute differences" heuristic approach
 * for finding the most optimal filter to be used.
 *
 * From http://www.libpng.org/pub/png/book/chapter09.html
 *
 *   For grayscale and truecolor images of 8 or more bits per sample, with or
 *   without alpha channels, dynamic filtering is almost always beneficial. The
 *   approach that has by now become standard is known as the minimum sum of
 *   absolute differences heuristic and was first proposed by Lee Daniel
 *   Crocker in February 1995.
 */
#[cfg(feature = "libz-sys")]
unsafe fn filter_PNG15_apply_filter(
    raster: *mut libc::c_uchar,
    columns: i32,
    rows: i32,
    bpc: i8,
    colors: i8,
    length: *mut i32,
) -> *mut libc::c_uchar {
    let bits_per_pixel: libc::c_int = colors as libc::c_int * bpc as libc::c_int;
    let bytes_per_pixel: libc::c_int = (bits_per_pixel + 7i32) / 8i32;
    let rowbytes: i32 = columns * bytes_per_pixel;
    assert!(!raster.is_null() && !length.is_null());
    /* Result */
    let dst = new((((rowbytes + 1i32) * rows) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<libc::c_uchar>() as u64) as u32)
        as *mut libc::c_uchar;
    *length = (rowbytes + 1i32) * rows;
    for j in 0..rows {
        let pp: *mut libc::c_uchar = dst.offset((j * (rowbytes + 1i32)) as isize);
        let p: *mut libc::c_uchar = raster.offset((j * rowbytes) as isize);
        let mut sum: [u32; 5] = [
            0i32 as u32,
            0i32 as u32,
            0i32 as u32,
            0i32 as u32,
            0i32 as u32,
        ];
        /* First calculated sum of values to make a heuristic guess
         * of optimal predictor function.
         */
        for i in 0..rowbytes {
            let left: libc::c_int = if i - bytes_per_pixel >= 0i32 {
                *p.offset((i - bytes_per_pixel) as isize) as libc::c_int
            } else {
                0i32
            };
            let up: libc::c_int = if j > 0i32 {
                *p.offset(i as isize).offset(-(rowbytes as isize)) as libc::c_int
            } else {
                0i32
            };
            let uplft: libc::c_int = if j > 0i32 {
                if i - bytes_per_pixel >= 0i32 {
                    *p.offset(i as isize)
                        .offset(-(rowbytes as isize))
                        .offset(-(bytes_per_pixel as isize)) as libc::c_int
                } else {
                    0i32
                }
            } else {
                0i32
            };
            /* Type 0 -- None */
            sum[0] = (sum[0] as libc::c_uint).wrapping_add(*p.offset(i as isize) as libc::c_uint)
                as u32 as u32;
            /* Type 1 -- Sub */
            sum[1] = (sum[1] as libc::c_uint)
                .wrapping_add((*p.offset(i as isize) as libc::c_int - left).abs() as libc::c_uint)
                as u32 as u32;
            /* Type 2 -- Up */
            sum[2] = (sum[2] as libc::c_uint)
                .wrapping_add((*p.offset(i as isize) as libc::c_int - up).abs() as libc::c_uint)
                as u32 as u32;
            /* Type 3 -- Average */
            let tmp: libc::c_int = (((up + left) / 2i32) as f64).floor() as libc::c_int;
            sum[3] = (sum[3] as libc::c_uint)
                .wrapping_add((*p.offset(i as isize) as libc::c_int - tmp).abs() as libc::c_uint)
                as u32 as u32;
            /* Type 4 -- Peath */
            let q: libc::c_int = left + up - uplft;
            let qa: libc::c_int = (q - left).abs();
            let qb: libc::c_int = (q - up).abs();
            let qc: libc::c_int = (q - uplft).abs();
            if qa <= qb && qa <= qc {
                sum[4] = (sum[4] as libc::c_uint).wrapping_add(
                    (*p.offset(i as isize) as libc::c_int - left).abs() as libc::c_uint,
                ) as u32 as u32
            } else if qb <= qc {
                sum[4] = (sum[4] as libc::c_uint)
                    .wrapping_add((*p.offset(i as isize) as libc::c_int - up).abs() as libc::c_uint)
                    as u32 as u32
            } else {
                sum[4] = (sum[4] as libc::c_uint).wrapping_add(
                    (*p.offset(i as isize) as libc::c_int - uplft).abs() as libc::c_uint,
                ) as u32 as u32
            }
        }
        let mut min: libc::c_int = sum[0] as libc::c_int;
        let mut min_idx: libc::c_int = 0i32;
        for i in 0..5 {
            if sum[i as usize] < min as libc::c_uint {
                min = sum[i as usize] as libc::c_int;
                min_idx = i
            }
        }
        let typ = min_idx;
        /* Now we actually apply filter. */
        *pp.offset(0) = typ as libc::c_uchar;
        match typ {
            0 => {
                libc::memcpy(
                    pp.offset(1) as *mut libc::c_void,
                    p as *const libc::c_void,
                    rowbytes as usize,
                );
            }
            1 => {
                for i in 0..rowbytes {
                    let left_0: libc::c_int = if i - bytes_per_pixel >= 0i32 {
                        *p.offset((i - bytes_per_pixel) as isize) as libc::c_int
                    } else {
                        0i32
                    };
                    *pp.offset((i + 1i32) as isize) =
                        (*p.offset(i as isize) as libc::c_int - left_0) as libc::c_uchar;
                }
            }
            2 => {
                for i in 0..rowbytes {
                    let up_0: libc::c_int = if j > 0i32 {
                        *p.offset(i as isize).offset(-(rowbytes as isize)) as libc::c_int
                    } else {
                        0i32
                    };
                    *pp.offset((i + 1i32) as isize) =
                        (*p.offset(i as isize) as libc::c_int - up_0) as libc::c_uchar;
                }
            }
            3 => {
                for i in 0..rowbytes {
                    let up_1: libc::c_int = if j > 0i32 {
                        *p.offset(i as isize).offset(-(rowbytes as isize)) as libc::c_int
                    } else {
                        0i32
                    };
                    let left_1: libc::c_int = if i - bytes_per_pixel >= 0i32 {
                        *p.offset((i - bytes_per_pixel) as isize) as libc::c_int
                    } else {
                        0i32
                    };
                    let tmp_0: libc::c_int =
                        (((up_1 + left_1) / 2i32) as f64).floor() as libc::c_int;
                    *pp.offset((i + 1i32) as isize) =
                        (*p.offset(i as isize) as libc::c_int - tmp_0) as libc::c_uchar;
                }
            }
            4 => {
                /* Peath */
                for i in 0..rowbytes {
                    let up_2: libc::c_int = if j > 0i32 {
                        *p.offset(i as isize).offset(-(rowbytes as isize)) as libc::c_int
                    } else {
                        0i32
                    };
                    let left_2: libc::c_int = if i - bytes_per_pixel >= 0i32 {
                        *p.offset((i - bytes_per_pixel) as isize) as libc::c_int
                    } else {
                        0i32
                    };
                    let uplft_0: libc::c_int = if j > 0i32 {
                        if i - bytes_per_pixel >= 0i32 {
                            *p.offset(i as isize)
                                .offset(-(rowbytes as isize))
                                .offset(-(bytes_per_pixel as isize))
                                as libc::c_int
                        } else {
                            0i32
                        }
                    } else {
                        0i32
                    };
                    let q_0: libc::c_int = left_2 + up_2 - uplft_0;
                    let qa_0: libc::c_int = (q_0 - left_2).abs();
                    let qb_0: libc::c_int = (q_0 - up_2).abs();
                    let qc_0: libc::c_int = (q_0 - uplft_0).abs();
                    if qa_0 <= qb_0 && qa_0 <= qc_0 {
                        *pp.offset((i + 1i32) as isize) =
                            (*p.offset(i as isize) as libc::c_int - left_2) as libc::c_uchar
                    } else if qb_0 <= qc_0 {
                        *pp.offset((i + 1i32) as isize) =
                            (*p.offset(i as isize) as libc::c_int - up_2) as libc::c_uchar
                    } else {
                        *pp.offset((i + 1i32) as isize) =
                            (*p.offset(i as isize) as libc::c_int - uplft_0) as libc::c_uchar
                    }
                }
            }
            _ => {}
        }
    }
    return dst;
}
/* TIFF predictor filter support
 *
 * Many PDF viewers seems to have broken TIFF 2 predictor support?
 * Ony GhostScript and MuPDF render 4bpc grayscale image with TIFF 2 predictor
 * filter applied correctly.
 *
 *  Acrobat Reader DC  2015.007.20033  NG
 *  Adobe Acrobat X    10.1.13         NG
 *  Foxit Reader       4.1.5.425       NG
 *  GhostScript        9.16            OK
 *  SumatraPDF(MuPDF)  v3.0            OK
 *  Evince(poppler)    2.32.0.145      NG (1bit and 4bit broken)
 */
/* This modifies "raster" itself! */
#[cfg(feature = "libz-sys")]
unsafe fn apply_filter_TIFF2_1_2_4(
    raster: *mut libc::c_uchar,
    width: i32,
    height: i32,
    bpc: i8,
    num_comp: i8,
) {
    let rowbytes: i32 = (bpc as libc::c_int * num_comp as libc::c_int * width + 7i32) / 8i32;
    let mask: u8 = ((1i32 << bpc as libc::c_int) - 1i32) as u8;
    assert!(!raster.is_null());
    assert!(bpc as libc::c_int > 0i32 && bpc as libc::c_int <= 8i32);
    let prev =
        new((num_comp as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32)
            as *mut u16;
    /* Generic routine for 1 to 16 bit.
     * It supports, e.g., 7 bpc images too.
     * Actually, it is not necessary to have 16 bit inbuf and outbuf
     * since we only need 1, 2, and 4 bit support here. 8 bit is enough.
     */
    for j in 0..height {
        memset(
            prev as *mut libc::c_void,
            0i32,
            (::std::mem::size_of::<u16>() as u64).wrapping_mul(num_comp as u64) as _,
        );
        let mut outbuf = 0i32 as u16;
        let mut inbuf = outbuf;
        let mut outbits = 0i32;
        let mut inbits = outbits;
        let mut k = j * rowbytes;
        let mut l = k;
        for _ in 0..width {
            for c in 0..num_comp as libc::c_int {
                if inbits < bpc as libc::c_int {
                    /* need more byte */
                    inbuf = ((inbuf as libc::c_int) << 8i32
                        | *raster.offset(l as isize) as libc::c_int)
                        as u16; /* consumed bpc bits */
                    l += 1;
                    inbits += 8i32
                }
                let cur = (inbuf as libc::c_int >> inbits - bpc as libc::c_int
                    & mask as libc::c_int) as u8;
                inbits -= bpc as libc::c_int;
                let mut sub = (cur as libc::c_int - *prev.offset(c as isize) as libc::c_int) as i8;
                *prev.offset(c as isize) = cur as u16;
                if (sub as libc::c_int) < 0i32 {
                    sub = (sub as libc::c_int + (1i32 << bpc as libc::c_int)) as i8
                }
                /* Append newly filtered component value */
                outbuf =
                    ((outbuf as libc::c_int) << bpc as libc::c_int | sub as libc::c_int) as u16;
                outbits += bpc as libc::c_int;
                /* flush */
                if outbits >= 8i32 {
                    *raster.offset(k as isize) =
                        (outbuf as libc::c_int >> outbits - 8i32) as libc::c_uchar;
                    k += 1;
                    outbits -= 8i32
                }
            }
        }
        if outbits > 0i32 {
            *raster.offset(k as isize) =
                ((outbuf as libc::c_int) << 8i32 - outbits) as libc::c_uchar
        }
    }
    free(prev as *mut libc::c_void);
}
#[cfg(feature = "libz-sys")]
unsafe fn filter_TIFF2_apply_filter(
    raster: *mut libc::c_uchar,
    columns: i32,
    rows: i32,
    bpc: i8,
    colors: i8,
    length: *mut i32,
) -> *mut libc::c_uchar {
    let rowbytes: i32 = (bpc as libc::c_int * colors as libc::c_int * columns + 7i32) / 8i32;
    assert!(!raster.is_null() && !length.is_null());
    let dst = new(((rowbytes * rows) as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<libc::c_uchar>() as u64) as u32)
        as *mut libc::c_uchar;
    libc::memcpy(
        dst as *mut libc::c_void,
        raster as *const libc::c_void,
        (rowbytes * rows) as usize,
    );
    *length = rowbytes * rows;
    match bpc as libc::c_int {
        1 | 2 | 4 => {
            apply_filter_TIFF2_1_2_4(dst, columns, rows, bpc, colors);
        }
        8 => {
            let prev = new(
                (colors as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32,
            ) as *mut u16;
            for j in 0..rows {
                memset(
                    prev as *mut libc::c_void,
                    0i32,
                    (::std::mem::size_of::<u16>() as u64).wrapping_mul(colors as u64) as _,
                );
                for i in 0..columns {
                    let pos: i32 = colors as libc::c_int * (columns * j + i);
                    for c in 0..colors as libc::c_int {
                        let cur: u8 = *raster.offset((pos + c) as isize);
                        let sub: i32 = cur as libc::c_int - *prev.offset(c as isize) as libc::c_int;
                        *prev.offset(c as isize) = cur as u16;
                        *dst.offset((pos + c) as isize) = sub as libc::c_uchar;
                    }
                }
            }
            free(prev as *mut libc::c_void);
        }
        16 => {
            let prev = new(
                (colors as u32 as u64).wrapping_mul(::std::mem::size_of::<u16>() as u64) as u32,
            ) as *mut u16;
            for j in 0..rows {
                memset(
                    prev as *mut libc::c_void,
                    0i32,
                    (::std::mem::size_of::<u16>() as u64).wrapping_mul(colors as u64) as _,
                );
                for i in 0..columns {
                    let pos_0: i32 = 2i32 * colors as libc::c_int * (columns * j + i);
                    for c_0 in 0..colors as libc::c_int {
                        let cur_0: u16 =
                            (*raster.offset((pos_0 + 2i32 * c_0) as isize) as libc::c_int * 256i32
                                + *raster.offset((pos_0 + 2i32 * c_0 + 1i32) as isize)
                                    as libc::c_int) as u16;
                        let sub_0: u16 = (cur_0 as libc::c_int
                            - *prev.offset(c_0 as isize) as libc::c_int)
                            as u16;
                        *prev.offset(c_0 as isize) = cur_0;
                        *dst.offset((pos_0 + 2i32 * c_0) as isize) =
                            (sub_0 as libc::c_int >> 8i32 & 0xffi32) as libc::c_uchar;
                        *dst.offset((pos_0 + 2i32 * c_0 + 1i32) as isize) =
                            (sub_0 as libc::c_int & 0xffi32) as libc::c_uchar;
                    }
                }
            }
            free(prev as *mut libc::c_void);
        }
        _ => {}
    }
    return dst;
}
#[cfg(feature = "libz-sys")]
unsafe fn filter_create_predictor_dict(
    predictor: libc::c_int,
    columns: i32,
    bpc: libc::c_int,
    colors: libc::c_int,
) -> pdf_dict {
    let mut parms = pdf_dict::new();
    parms.set("BitsPerComponent", bpc as f64);
    parms.set("Colors", colors as f64);
    parms.set("Columns", columns as f64);
    parms.set("Predictor", predictor as f64);
    parms
}
unsafe fn write_stream(stream: &mut pdf_stream, handle: &mut OutputHandleWrapper) {
    /*
     * Always work from a copy of the stream. All filters read from
     * "filtered" and leave their result in "filtered".
     */
    let mut filtered = new(stream.content.len() as u32) as *mut u8;
    libc::memcpy(
        filtered as *mut libc::c_void,
        stream.content.as_ptr() as *const libc::c_void,
        stream.content.len(),
    );
    let mut filtered_length = stream.content.len() as u32;
    /* PDF/A requires Metadata to be not filtered. */
    if stream
        .get_dict()
        .get("Type")
        .filter(|&typ| b"Metadata" == (*typ).as_name().to_bytes())
        .is_some()
    {
        stream._flags &= !STREAM_COMPRESS;
    }
    /* Apply compression filter if requested */
    #[cfg(feature = "libz-sys")]
    {
        if stream.content.len() > 0
            && stream._flags & STREAM_COMPRESS != 0
            && compression_level as libc::c_int > 0i32
        {
            /* First apply predictor filter if requested. */
            if compression_use_predictor as libc::c_int != 0
                && stream._flags & STREAM_USE_PREDICTOR != 0
                && !(*stream.dict).as_dict().has("DecodeParms")
            {
                let bits_per_pixel: libc::c_int =
                    stream.decodeparms.colors * stream.decodeparms.bits_per_component;
                let len: i32 = (stream.decodeparms.columns * bits_per_pixel + 7i32) / 8i32;
                let rows: i32 = (stream.content.len() as i32) / len;
                let mut filtered2: *mut libc::c_uchar = ptr::null_mut();
                let mut length2: i32 = stream.content.len() as i32;
                let parms = filter_create_predictor_dict(
                    stream.decodeparms.predictor,
                    stream.decodeparms.columns,
                    stream.decodeparms.bits_per_component,
                    stream.decodeparms.colors,
                );
                match stream.decodeparms.predictor {
                    2 => {
                        /* TIFF2 */
                        filtered2 = filter_TIFF2_apply_filter(
                            filtered,
                            stream.decodeparms.columns,
                            rows,
                            stream.decodeparms.bits_per_component as i8,
                            stream.decodeparms.colors as i8,
                            &mut length2,
                        )
                    }
                    15 => {
                        /* PNG optimun */
                        filtered2 = filter_PNG15_apply_filter(
                            filtered,
                            stream.decodeparms.columns,
                            rows,
                            stream.decodeparms.bits_per_component as i8,
                            stream.decodeparms.colors as i8,
                            &mut length2,
                        )
                    }
                    _ => {
                        warn!(
                            "Unknown/unsupported Predictor function {}.",
                            stream.decodeparms.predictor
                        );
                    }
                }
                if !filtered2.is_null() {
                    free(filtered as *mut libc::c_void);
                    filtered = filtered2;
                    filtered_length = length2 as libc::c_uint;
                    (*stream.dict).as_dict_mut().set("DecodeParms", parms);
                }
            }
            let filters = (*stream.dict).as_dict_mut().get_mut("Filter");
            let mut buffer_length: libz::uLong;
            buffer_length = filtered_length
                .wrapping_add(filtered_length.wrapping_div(1000i32 as libc::c_uint))
                .wrapping_add(14i32 as libc::c_uint) as libz::uLong;
            let buffer = new((buffer_length as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<libc::c_uchar>() as u64)
                as u32) as *mut libc::c_uchar;
            let filter_name = "FlateDecode".into_obj();
            let has_filters = filters.is_some();
            if let Some(filters) = filters {
                /*
                 * FlateDecode is the first filter to be applied to the stream.
                 */
                pdf_unshift_array(filters, filter_name);
            } else {
                /*
                 * Adding the filter as a name instead of a one-element array
                 * is crucial because otherwise Adobe Reader cannot read the
                 * cross-reference stream any more, cf. the PDF v1.5 Errata.
                 */
                (*stream.dict).as_dict_mut().set("Filter", filter_name);
            }

            #[cfg(not(feature = "legacy-libz"))]
            {
                if libz::compress2(
                    buffer,
                    &mut buffer_length,
                    filtered,
                    filtered_length as libz::uLong,
                    compression_level as libc::c_int,
                ) != 0
                {
                    panic!("Zlib error");
                }
            }
            #[cfg(feature = "legacy-libz")]
            {
                if libz::compress(
                    buffer,
                    &mut buffer_length,
                    filtered,
                    filtered_length as libz::uLong,
                ) != 0
                {
                    panic!("Zlib error");
                }
            }
            free(filtered as *mut libc::c_void);
            compression_saved = (compression_saved as u64).wrapping_add(
                (filtered_length as u64)
                    .wrapping_sub(buffer_length as u64)
                    .wrapping_sub(if has_filters {
                        strlen(b"/FlateDecode \x00" as *const u8 as *const i8)
                    } else {
                        strlen(b"/Filter/FlateDecode\n\x00" as *const u8 as *const i8)
                    } as u64),
            ) as libc::c_int as libc::c_int;
            filtered = buffer;
            filtered_length = buffer_length as libc::c_uint
        }
    }
    /* HAVE_ZLIB */
    /* AES will change the size of data! */
    if enc_mode {
        let mut cipher: *mut u8 = ptr::null_mut();
        let mut cipher_len: size_t = 0i32 as size_t;
        pdf_encrypt_data(
            filtered,
            filtered_length as size_t,
            &mut cipher,
            &mut cipher_len,
        );
        free(filtered as *mut libc::c_void);
        filtered = cipher;
        filtered_length = cipher_len as u32
    }
    (*stream.dict)
        .as_dict_mut()
        .set("Length", filtered_length as f64);
    pdf_write_obj(stream.dict, handle);
    pdf_out(handle, b"\nstream\n");
    let mut v = Vec::<u8>::new();
    for i in 0..filtered_length {
        v.push(*filtered.offset(i as isize));
    }
    if filtered_length > 0_u32 {
        pdf_out(
            handle, &v, //TODO: check
        );
    }
    free(filtered as *mut libc::c_void);
    /*
     * This stream length "object" gets reset every time write_stream is
     * called for the stream object.
     * If this stream gets written more than once with different
     * filters, this could be a problem.
     */
    pdf_out(handle, b"\n");
    pdf_out(handle, b"endstream");
}

impl Drop for pdf_stream {
    fn drop(&mut self) {
        let pdf_stream {
            dict, objstm_data, ..
        } = *self;
        unsafe {
            pdf_release_obj(dict);
            if !objstm_data.is_null() {
                free(objstm_data as *mut libc::c_void);
            }
        }
    }
}

impl pdf_stream {
    pub(crate) fn get_dict(&self) -> &pdf_dict {
        unsafe { (*self.dict).as_dict() }
    }
    pub(crate) fn get_dict_mut(&mut self) -> &mut pdf_dict {
        unsafe { (*self.dict).as_dict_mut() }
    }
    pub(crate) unsafe fn get_dict_obj(&mut self) -> &mut pdf_obj {
        &mut (*self.dict)
    }
    pub(crate) fn len(&self) -> usize {
        self.content.len()
    }
}

pub(crate) unsafe fn pdf_stream_dataptr(stream: &pdf_obj) -> *const libc::c_void {
    stream.as_stream().content.as_ptr() as *const libc::c_void
}

unsafe fn set_objstm_data(objstm: &mut pdf_obj, data: *mut i32) {
    objstm.as_stream_mut().objstm_data = data;
}
unsafe fn get_objstm_data(objstm: &pdf_obj) -> *mut i32 {
    objstm.as_stream().objstm_data
}

impl pdf_stream {
    pub(crate) unsafe fn add(&mut self, stream_data: *const libc::c_void, length: i32) {
        if length < 1i32 {
            return;
        }
        let payload = std::slice::from_raw_parts(stream_data as *const u8, length as usize);
        self.add_slice(payload);
    }
    pub(crate) fn add_slice(&mut self, slice: &[u8]) {
        self.content.extend_from_slice(slice);
    }
}

impl pdf_stream {
    pub(crate) fn add_str(&mut self, stream_data: &str) {
        if !stream_data.is_empty() {
            self.content.extend_from_slice(stream_data.as_bytes());
        }
    }
}

#[cfg(feature = "libz-sys")]
mod flate2_libz_helpers {
    // Workaround for https://github.com/rust-lang/libz-sys/issues/55
    // (This code is stolen from flate2: https://github.com/rust-lang/flate2-rs/blob/31fb07820345691352aaa64f367c1e482ad9cfdc/src/ffi/c.rs#L60)
    use libc::c_void;
    use std::{
        alloc::{self, Layout},
        ptr,
    };

    const ALIGN: usize = std::mem::align_of::<usize>();

    fn align_up(size: usize, align: usize) -> usize {
        (size + align - 1) & !(align - 1)
    }

    pub extern "C" fn zalloc(_ptr: *mut c_void, items: u32, item_size: u32) -> *mut c_void {
        // We need to multiply `items` and `item_size` to get the actual desired
        // allocation size. Since `zfree` doesn't receive a size argument we
        // also need to allocate space for a `usize` as a header so we can store
        // how large the allocation is to deallocate later.
        let size = match (items as usize)
            .checked_mul(item_size as usize)
            .map(|size| align_up(size, ALIGN))
            .and_then(|i| i.checked_add(std::mem::size_of::<usize>()))
        {
            Some(i) => i,
            None => return ptr::null_mut(),
        };

        // Make sure the `size` isn't too big to fail `Layout`'s restrictions
        let layout = match Layout::from_size_align(size, ALIGN) {
            Ok(layout) => layout,
            Err(_) => return ptr::null_mut(),
        };

        unsafe {
            // Allocate the data, and if successful store the size we allocated
            // at the beginning and then return an offset pointer.
            let ptr = alloc::alloc(layout) as *mut usize;
            if ptr.is_null() {
                return ptr as *mut c_void;
            }
            *ptr = size;
            ptr.add(1) as *mut c_void
        }
    }

    pub extern "C" fn zfree(_ptr: *mut c_void, address: *mut c_void) {
        unsafe {
            // Move our address being free'd back one pointer, read the size we
            // stored in `zalloc`, and then free it using the standard Rust
            // allocator.
            let ptr = (address as *mut usize).offset(-1);
            let size = *ptr;
            let layout = Layout::from_size_align_unchecked(size, ALIGN);
            alloc::dealloc(ptr as *mut u8, layout)
        }
    }
}

#[cfg(feature = "libz-sys")]
pub(crate) unsafe fn pdf_add_stream_flate(dst: &mut pdf_stream, data: &[u8]) -> libc::c_int {
    const WBUF_SIZE: usize = 4096;
    let mut wbuf = [0u8; WBUF_SIZE];
    let mut z: libz::z_stream = libz::z_stream {
        next_in: data.as_ptr() as *mut u8,
        avail_in: data.len() as libz::uInt,
        total_in: 0,
        next_out: wbuf.as_mut_ptr(),
        avail_out: WBUF_SIZE as libz::uInt,
        total_out: 0,
        msg: ptr::null_mut(),
        state: ptr::null_mut(),
        zalloc: flate2_libz_helpers::zalloc,
        zfree: flate2_libz_helpers::zfree,
        opaque: 0 as libz::voidpf,
        data_type: 0,
        adler: 0,
        reserved: 0,
    };
    if libz::inflateInit_(
        &mut z,
        b"1.2.11\x00" as *const u8 as *const i8,
        ::std::mem::size_of::<libz::z_stream>() as u64 as libc::c_int,
    ) != 0i32
    {
        warn!("inflateInit() failed.");
        return -1i32;
    }
    loop {
        let status = libz::inflate(&mut z, 0i32);
        assert!(z.avail_out <= WBUF_SIZE as u32);
        if status == 1i32
        /* Z_STREAM_END */
        {
            break;
        }
        if status != 0i32 {
            warn!("inflate() failed. Broken PDF file?");
            libz::inflateEnd(&mut z);
            return -1i32;
        }
        if z.avail_out == 0 {
            dst.add_slice(wbuf.as_ref());
            z.next_out = wbuf.as_mut_ptr();
            z.avail_out = WBUF_SIZE as libz::uInt
        }
    }
    if (WBUF_SIZE as u32) - z.avail_out > 0 {
        dst.add_slice(&wbuf[..((WBUF_SIZE - z.avail_out as usize) as usize)]);
    }

    return if libz::inflateEnd(&mut z) == 0i32 {
        0i32
    } else {
        -1i32
    };
}

#[cfg(feature = "libz-sys")]
unsafe fn get_decode_parms(parms: &mut decode_parms, dict: &mut pdf_obj) -> libc::c_int {
    assert!(dict.is_dict());
    /* Fill with default values */
    parms.predictor = 1i32;
    parms.colors = 1i32;
    parms.bits_per_component = 8i32;
    parms.columns = 1i32;
    let tmp = pdf_deref_obj(dict.as_dict_mut().get_mut("Predictor"));
    if !tmp.is_null() {
        parms.predictor = (*tmp).as_f64() as libc::c_int
    }
    let tmp = pdf_deref_obj(dict.as_dict_mut().get_mut("Colors"));
    if !tmp.is_null() {
        parms.colors = (*tmp).as_f64() as libc::c_int
    }
    let tmp = pdf_deref_obj(dict.as_dict_mut().get_mut("BitsPerComponent"));
    if !tmp.is_null() {
        parms.bits_per_component = (*tmp).as_f64() as libc::c_int
    }
    let tmp = pdf_deref_obj(dict.as_dict_mut().get_mut("Columns"));
    if !tmp.is_null() {
        parms.columns = (*tmp).as_f64() as i32
    }
    if parms.bits_per_component != 1i32
        && parms.bits_per_component != 2i32
        && parms.bits_per_component != 4i32
        && parms.bits_per_component != 8i32
        && parms.bits_per_component != 16i32
    {
        warn!(
            "Invalid BPC value in DecodeParms: {}",
            parms.bits_per_component,
        );
        return -1i32;
    } else {
        if parms.predictor <= 0i32 || parms.colors <= 0i32 || parms.columns <= 0i32 {
            return -1i32;
        }
    }
    return 0i32;
}
/* From Xpdf version 3.04
 * I'm not sure if I properly ported... Untested.
 */
#[cfg(feature = "libz-sys")]
unsafe fn filter_row_TIFF2(
    dst: &mut [u8],
    src: *const libc::c_uchar,
    parms: &mut decode_parms,
) -> libc::c_int {
    let p: *const libc::c_uchar = src;
    /* bits_per_component < 8 here */
    let mask: libc::c_int = (1i32 << parms.bits_per_component) - 1i32; /* 2 bytes buffer */
    let col = new((parms.colors as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<libc::c_uchar>() as u64) as u32)
        as *mut libc::c_uchar;
    memset(col as *mut libc::c_void, 0i32, parms.colors as _);
    let mut outbuf = 0i32;
    let mut inbuf = outbuf;
    let mut outbits = 0i32;
    let mut inbits = outbits;
    let mut k = 0;
    let mut j = k;
    for _ in 0..parms.columns {
        /* expanding each color component into an 8-bits bytes array */
        for ci in 0..parms.colors {
            if inbits < parms.bits_per_component {
                /* need more byte */
                inbuf = inbuf << 8i32 | *p.offset(j as isize) as libc::c_int;
                j += 1;
                inbits += 8;
            }
            /* predict current color component */
            *col.offset(ci as isize) = (*col.offset(ci as isize) as libc::c_int
                + (inbuf >> inbits - parms.bits_per_component)
                & mask) as libc::c_uchar; /* consumed bpc bits */
            inbits -= parms.bits_per_component;
            /* append newly predicted color component value */
            outbuf = outbuf << parms.bits_per_component | *col.offset(ci as isize) as i32;
            outbits += parms.bits_per_component;
            if outbits >= 8 {
                /* flush */
                dst[k as usize] = (outbuf >> outbits - 8) as u8;
                k = k + 1;
                outbits -= 8;
            }
        }
    }
    if outbits > 0 {
        dst[k as usize] = (outbuf << 8 - outbits) as u8
    }
    free(col as *mut libc::c_void);
    return 0i32;
}
/* This routine is inefficient. Length is typically 4 for Xref streams.
 * Especially, calling pdf_add_stream() for each 4 bytes append is highly
 * inefficient.
 */
#[cfg(feature = "libz-sys")]
unsafe fn filter_decoded(
    dst_stream: &mut pdf_stream,
    data: &[u8],
    parms: &mut decode_parms,
) -> libc::c_int {
    let bits_per_pixel: i32 = parms.colors * parms.bits_per_component;
    let bytes_per_pixel: i32 = (bits_per_pixel + 7i32) / 8i32;
    let length: i32 = (parms.columns * bits_per_pixel + 7i32) / 8i32;
    let len_usize = length as usize;
    let mut error = 0i32;
    if parms.predictor < 10 {
        let mut buf = vec![0u8; length as usize];
        match parms.predictor {
            1 => {
                /* No prediction */
                dst_stream.add_slice(&data);
                return error;
            }
            2 => {
                let bytes_per_pixel = bytes_per_pixel as usize;
                /* TIFF Predictor 2 */
                if parms.bits_per_component == 8i32 {
                    let mut chunks = data.chunks_exact(len_usize);
                    while let Some(p) = chunks.next() {
                        for i in 0..len_usize {
                            let pixel_value: i32 = if i >= bytes_per_pixel {
                                buf[(i - bytes_per_pixel) as usize] as i32
                            } else {
                                0i32
                            };
                            buf[i] = ((p[i] as i32) + pixel_value & 0xff) as u8;
                        }
                        dst_stream.add_slice(&buf[..len_usize]);
                    }
                    assert!(chunks.remainder().is_empty());
                } else if parms.bits_per_component == 16i32 {
                    let mut chunks = data.chunks_exact(len_usize);
                    while let Some(p) = chunks.next() {
                        for i in (0..length).step_by(2) {
                            let b = (i - (bytes_per_pixel as i32)) as i32;
                            let hi = if b >= 0 { buf[b as usize] as i32 } else { 0i32 };
                            let lo = if b >= 0 {
                                buf[(b as usize) + 1] as i32
                            } else {
                                0i32
                            };
                            let pv_0 = hi << 8 | lo;
                            let i = i as usize;
                            let cv = (p[i] as i32) << 8 | p[i + 1] as i32;
                            let c = pv_0 + cv;
                            buf[i] = (c >> 8) as u8;
                            buf[i + 1] = (c & 0xff) as u8;
                        }
                        dst_stream.add_slice(&buf[..len_usize]);
                    }
                    assert!(chunks.remainder().is_empty());
                } else {
                    let mut chunks = data.chunks_exact(len_usize);
                    while let Some(p) = chunks.next() {
                        if error != 0 {
                            break;
                        }
                        error = filter_row_TIFF2(buf.as_mut_slice(), p.as_ptr(), parms);
                        if error == 0 {
                            dst_stream.add_slice(&buf[..(length as usize)]);
                        }
                    }
                    assert!(chunks.remainder().is_empty());
                }
                return error;
            }
            _ => {
                warn!("Unknown Predictor type value :{}", parms.predictor);
                error = -1i32;
                return error;
            }
        }
    } else {
        let rowlen = len_usize + 1;
        let mut prev = vec![0u8; rowlen];
        let mut current = vec![0u8; rowlen];
        match parms.predictor {
            // PNG can improve its compression ratios by applying filters to each scanline of the image.
            // FlateDecode incorporates these filtering methods.
            10 | // PNG None
            11 | // PNG Sub on all rows
            12 | // PNG UP on all rows
            13 | // PNG Average on all rows
            14 | // PNG Paeth on all rows
            15   // PNG Optimun: each scanline encodes the filter type in its first byte.
                 // The prediction algorithm can change from line to line
            => {
                let typ = (parms.predictor - 10) as u8;
                let mut chunks = data.chunks_exact(rowlen);
                let bytes_per_pixel = bytes_per_pixel as usize;
                while let Some(p) = chunks.next() {
                    if error != 0 {
                        break;
                    }
                    if parms.predictor != 15 && p[0] != typ {
                        warn!(
                            "Mismatched Predictor type in data stream: predictor said {:?}, but line had {:?}",
                            PngFilterType::from_u8(typ),
                            PngFilterType::from_u8(p[0])
                        );
                        error = -1i32;
                    }
                    if let Some(filter) = PngFilterType::from_u8(p[0]) {
                        current[..rowlen].copy_from_slice(p);
                        if let Err(unf_err) = png_unfilter_scanline(filter, bytes_per_pixel, &prev[1..rowlen], &mut current[1..rowlen]) {
                            warn!("unfiltering PNG scanline failed: {}", unf_err);
                            error = -1;
                        }
                        prev[..rowlen].copy_from_slice(&current[..rowlen]);
                    } else {
                        warn!("Unknown PNG predictor type: {}", p[0]);
                        error = -1;
                    }
                    if error == 0 {
                        dst_stream.add_slice(&current[1..rowlen]);
                    }
                }
                if error == 0 {
                    if !chunks.remainder().is_empty() {
                        warn!("remaining scanlines in PNG stream decoding");
                        error = -1;
                    }
                }
            }
            _ => {
                warn!("Unknown Predictor type value: {}", parms.predictor);
                error = -1i32;
                return error;
            }
        }
    }
    error
}

#[cfg(feature = "libz-sys")]
unsafe fn pdf_add_stream_flate_filtered(
    dst_stream: &mut pdf_stream,
    data: &[u8],
    parms: &mut decode_parms,
) -> libc::c_int {
    const WBUF_SIZE: usize = 4096;
    let mut wbuf: [libz::Bytef; WBUF_SIZE] = [0; WBUF_SIZE];
    let mut z: libz::z_stream = libz::z_stream {
        next_in: data.as_ptr() as *mut u8,
        avail_in: data.len() as libz::uInt,
        total_in: 0,
        next_out: wbuf.as_mut_ptr(),
        avail_out: WBUF_SIZE as libz::uInt,
        total_out: 0,
        msg: ptr::null_mut(),
        state: ptr::null_mut(),
        zalloc: flate2_libz_helpers::zalloc,
        zfree: flate2_libz_helpers::zfree,
        opaque: 0 as libz::voidpf,
        data_type: 0,
        adler: 0,
        reserved: 0,
    };
    if libz::inflateInit_(
        &mut z,
        b"1.2.11\x00" as *const u8 as *const i8,
        ::std::mem::size_of::<libz::z_stream>() as u64 as libc::c_int,
    ) != 0i32
    {
        warn!("inflateInit() failed.");
        return -1i32;
    }
    let mut tmp_stream = pdf_stream::new(0);
    loop {
        let status = libz::inflate(&mut z, 0i32);
        if status == 1i32 {
            break;
        }
        if status != 0i32 {
            warn!("inflate() failed. Broken PDF file?");
            libz::inflateEnd(&mut z);
            return -1i32;
        }
        if z.avail_out == 0i32 as libc::c_uint {
            tmp_stream.add_slice(&wbuf[..]);
            z.next_out = wbuf.as_mut_ptr();
            z.avail_out = WBUF_SIZE as libz::uInt
        }
    }
    if (WBUF_SIZE as u32) > z.avail_out {
        let remain = &wbuf[..(WBUF_SIZE - (z.avail_out as usize))];
        tmp_stream.add_slice(remain);
    }
    let error = filter_decoded(dst_stream, &tmp_stream.content, parms);
    if error == 0 && libz::inflateEnd(&mut z) == 0i32 {
        0i32
    } else {
        -1i32
    }
}

pub(crate) unsafe fn pdf_concat_stream(dst: &mut pdf_stream, src: &mut pdf_stream) -> i32 {
    let mut error: i32 = 0i32;
    let stream_dict = (*(src as *mut pdf_stream)).get_dict_mut(); // TODO: fix hack
    let stream_data = &src.content;
    if stream_dict.get("Filter").is_some() {
        #[cfg(feature = "libz-sys")]
        {
            let mut parms = decode_parms {
                predictor: 0,
                colors: 0,
                bits_per_component: 0,
                columns: 0,
            };
            let mut have_parms: libc::c_int = 0i32;
            if stream_dict.has("DecodeParms") {
                /* Dictionary or array */
                let mut tmp = pdf_deref_obj(stream_dict.get_mut("DecodeParms"));
                if !tmp.is_null() && (*tmp).is_array() {
                    if (*tmp).as_array().len() > 1 {
                        warn!("Unexpected size for DecodeParms array.");
                        return -1i32;
                    }

                    let array = (*tmp).as_array_mut();
                    tmp = if !array.is_empty() {
                        pdf_deref_obj(Some(&mut *array[0]))
                    } else {
                        0 as *mut pdf_obj
                    };
                }
                if !(!tmp.is_null() && (*tmp).is_dict()) {
                    warn!("PDF dict expected for DecodeParms...");
                    return -1i32;
                }
                error = get_decode_parms(&mut parms, &mut *tmp);
                if error != 0 {
                    panic!("Invalid value(s) in DecodeParms dictionary.");
                }
                have_parms = 1i32
            }
            let mut filter = stream_dict.get("Filter").unwrap();
            if (*filter).is_array() {
                if (*filter).as_array().len() > 1 {
                    warn!("Multiple DecodeFilter not supported.");
                    return -1i32;
                }
                filter = &**(*filter).as_array().get(0).expect("Broken PDF file?");
            }
            if (*filter).is_name() {
                let filter_name = (*filter).as_name().to_bytes();
                if filter_name == b"FlateDecode" {
                    if have_parms != 0 {
                        error = pdf_add_stream_flate_filtered(dst, stream_data, &mut parms)
                    } else {
                        error = pdf_add_stream_flate(dst, stream_data)
                    }
                } else {
                    warn!("DecodeFilter \"{}\" not supported.", filter_name.display());
                    error = -1i32
                }
            } else {
                panic!("Broken PDF file?");
            }
        }
    } else {
        (*dst).add_slice(stream_data);
    }
    /* HAVE_ZLIB */
    error
}
unsafe fn pdf_stream_uncompress(src: &mut pdf_obj) -> *mut pdf_obj {
    let mut dst = pdf_stream::new(0i32);
    assert!(src.is_stream());
    dst.get_dict_mut().merge(src.as_stream().get_dict());
    pdf_remove_dict(dst.get_dict_obj(), "Length");
    pdf_concat_stream(&mut dst, src.as_stream_mut());
    dst.into_obj()
}
unsafe fn pdf_write_obj(object: *mut pdf_obj, handle: &mut OutputHandleWrapper) {
    if object.is_null() {
        write_null(handle);
        return;
    }
    if object.is_null()
        || (*object).typ() == PdfObjType::OBJ_INVALID
        || (*object).typ() == PdfObjType::UNDEFINED
    {
        panic!(
            "pdf_write_obj: Invalid object, type = {:?}\n",
            (*object).typ()
        );
    }
    match (*object).data {
        PdfObjVariant::BOOLEAN(v) => {
            write_boolean(v, handle);
        }
        PdfObjVariant::NUMBER(v) => {
            write_number(v, handle);
        }
        PdfObjVariant::STRING(v) => {
            write_string(&*v, handle);
        }
        PdfObjVariant::NAME(v) => {
            write_name(&*v, handle);
        }
        PdfObjVariant::ARRAY(v) => {
            write_array(v, handle);
        }
        PdfObjVariant::DICT(v) => {
            write_dict(&*v, handle);
        }
        PdfObjVariant::STREAM(v) => {
            write_stream(&mut *v, handle);
        }
        PdfObjVariant::NULL => {
            write_null(handle);
        }
        PdfObjVariant::INDIRECT(v) => {
            write_indirect(v, handle);
        }
        _ => {}
    };
}
/* Write the object to the file */
unsafe fn pdf_flush_obj(object: *mut pdf_obj, handle: &mut OutputHandleWrapper) {
    /*
     * Record file position
     */
    let (label, generation) = (*object).id;
    add_xref_entry(
        label as usize,
        1_u8,
        (pdf_output_file_position as u32, generation),
    );
    let out = format!("{} {} obj\n", label, generation);
    enc_mode = doc_enc_mode as i32 != 0 && (*object).flags & OBJ_NO_ENCRYPT == 0;
    pdf_enc_set_label(label);
    pdf_enc_set_generation(generation as u32);
    pdf_out(handle, out.as_bytes());
    pdf_write_obj(object, handle);
    pdf_out(handle, b"\nendobj\n");
}
unsafe fn pdf_add_objstm(objstm: &mut pdf_obj, object: &mut pdf_obj) -> i32 {
    assert!(objstm.is_stream());
    let data = get_objstm_data(objstm);
    let ref mut fresh15 = *data.offset(0);
    *fresh15 += 1;
    let pos = *fresh15;
    *data.offset((2i32 * pos) as isize) = object.label() as i32;
    *data.offset((2i32 * pos + 1i32) as isize) = objstm.as_stream().len() as i32;
    add_xref_entry(
        object.label() as usize,
        2_u8,
        (objstm.label(), (pos - 1i32) as u16),
    );
    /* redirect output into objstm */
    output_stream = objstm as *mut pdf_obj;
    enc_mode = false;
    let handle = pdf_output_handle.as_mut().unwrap();
    pdf_write_obj(object, handle);
    pdf_out_char(handle, b'\n');
    output_stream = ptr::null_mut();
    pos
}
unsafe fn release_objstm(objstm: *mut pdf_obj) {
    let data: *mut i32 = get_objstm_data(&*objstm);
    let pos: i32 = *data.offset(0);
    let stream = (*objstm).as_stream_mut();
    /* Precede stream data by offset table */
    /* Reserve 22 bytes for each entry (two 10 digit numbers plus two spaces) */
    let old_buf = std::mem::replace(
        &mut (*stream).content,
        Vec::with_capacity(22 * pos as usize),
    );
    let mut val: *mut i32 = data.offset(2);
    for _ in 0..(2 * pos) {
        let slice = format!("{} ", *val);
        val = val.offset(1);
        (*objstm).as_stream_mut().add_slice(slice.as_bytes());
    }
    let dict = (*objstm).as_stream_mut().get_dict_mut();
    dict.set("Type", "ObjStm");
    dict.set("N", pos as f64);
    dict.set("First", (*stream).content.len() as f64);
    (*objstm).as_stream_mut().add_slice(old_buf.as_ref());
    pdf_release_obj(objstm);
}

pub unsafe fn pdf_release_obj(mut object: *mut pdf_obj) {
    if object.is_null() {
        return;
    }
    if object.is_null() || (*object).typ() == PdfObjType::OBJ_INVALID || (*object).refcount <= 0 {
        info!(
            "\npdf_release_obj: object={:p}, type={:?}, refcount={}\n",
            object,
            (*object).typ(),
            (*object).refcount,
        );
        pdf_write_obj(object, ttstub_output_open_stdout().as_mut().unwrap());
        panic!("pdf_release_obj:  Called with invalid object.");
    }
    (*object).refcount -= 1;
    if (*object).refcount == 0_u32 {
        /*
         * Nothing is using this object so it's okay to remove it.
         * Nonzero "label" means object needs to be written before it's destroyed.
         */
        if (*object).label() != 0 && pdf_output_handle.is_some() {
            if do_objstm == 0
                || (*object).flags & OBJ_NO_OBJSTM != 0
                || doc_enc_mode as i32 != 0 && (*object).flags & OBJ_NO_ENCRYPT != 0
                || (*object).generation() as i32 != 0
            {
                let handle = pdf_output_handle.as_mut().unwrap();
                pdf_flush_obj(object, handle);
            } else {
                if current_objstm.is_null() {
                    let data: *mut i32 = new(((2i32 * 200i32 + 2i32) as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<i32>() as u64)
                        as u32) as *mut i32;
                    *data.offset(1) = 0;
                    *data.offset(0) = 0;
                    current_objstm = pdf_stream::new(STREAM_COMPRESS).into_obj();
                    set_objstm_data(&mut *current_objstm, data);
                    pdf_label_obj(current_objstm);
                }
                if pdf_add_objstm(&mut *current_objstm, &mut *object) == 200i32 {
                    release_objstm(current_objstm);
                    current_objstm = ptr::null_mut()
                }
            }
        }
        match (*object).data {
            PdfObjVariant::BOOLEAN(..) => {}
            PdfObjVariant::NUMBER(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::STRING(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::NAME(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::ARRAY(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::DICT(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::STREAM(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::INDIRECT(v) => {
                let _ = Box::from_raw(v);
            }
            PdfObjVariant::NULL | _ => {}
        }
        /* This might help detect freeing already freed objects */
        (*object).data = PdfObjVariant::OBJ_INVALID;
        free(object as *mut libc::c_void);
    };
}
/* PDF reading starts around here */
/* As each lines may contain null-characters, so outptr here is NOT
 * null-terminated string. Returns -1 for when EOF is already reached, and -2
 * if buffer has no enough space.
 */
#[derive(Copy, Clone, Debug)]
enum MfReadErr {
    Eof,
    NotEnoughSpace,
}
unsafe fn tt_mfreadln<R: Read + Seek>(size: usize, handle: &mut R) -> Result<Vec<u8>, MfReadErr> {
    let mut c;
    let mut buf = Vec::with_capacity(size + 1);
    loop {
        c = ttstub_input_getc(handle);
        if !(c != -1 && c != '\n' as i32 && c != '\r' as i32) {
            break;
        }
        if buf.len() >= size {
            return Err(MfReadErr::NotEnoughSpace);
        }
        buf.push(c as u8);
    }
    if c == -1 && buf.is_empty() {
        return Err(MfReadErr::Eof);
    }
    if c == '\r' as i32
        && {
            c = ttstub_input_getc(handle);
            c >= 0
        }
        && c != '\n' as i32
    {
        handle.seek(SeekFrom::Current(-1)).unwrap();
    }
    Ok(buf)
}
unsafe fn backup_line<R: Read + Seek>(handle: &mut R) -> i32 {
    let mut ch: i32 = -1i32;
    /* Note: this code should work even if \r\n is eol. It could fail on a
     * machine where \n is eol and there is a \r in the stream --- Highly
     * unlikely in the last few bytes where this is likely to be used.
     */
    match handle.seek(SeekFrom::Current(0)) {
        Ok(pos) if pos > 1 => loop {
            let pos = handle.seek(SeekFrom::Current(-2));
            match pos {
                Ok(pos)
                    if (pos > 0
                        && {
                            ch = ttstub_input_getc(handle);
                            ch >= 0i32
                        }
                        && (ch != '\n' as i32 && ch != '\r' as i32)) => {}
                _ => break,
            }
        },
        _ => {}
    }
    if ch < 0i32 {
        return 0i32;
    }
    1i32
}
unsafe fn find_xref<R: Read + Seek>(handle: &mut R, file_size: i32) -> i32 {
    let mut tries: i32 = 10i32;
    loop {
        if backup_line(handle) == 0 {
            tries = 0i32;
            break;
        } else {
            let currentpos = handle.seek(SeekFrom::Current(0)).unwrap() as i32;
            let n = core::cmp::min(b"startxref".len() as i32, file_size - currentpos) as usize;
            let mut buf = vec![0; n];
            handle.read_exact(buf.as_mut_slice());
            handle.seek(SeekFrom::Start(currentpos as u64)).unwrap();
            tries -= 1;
            if !(tries > 0 && !buf.starts_with(b"startxref")) {
                break;
            }
        }
    }
    if tries <= 0i32 {
        return 0i32;
    }
    /* Skip rest of this line */
    tt_mfgets(work_buffer.as_mut_ptr(), 1024i32, handle);
    /* Next line of input file should contain actual xref location */
    match tt_mfreadln(1024, handle) {
        Err(_) => {
            warn!("Reading xref location data failed... Not a PDF file?");
            0
        }
        Ok(buf) if buf.len() == 0 => {
            warn!("Reading xref location data failed... Not a PDF file?");
            0
        }
        Ok(buf) => {
            let mut p = buf.as_slice();
            p.skip_white();
            let number = p.parse_number().unwrap();
            let xref_pos = number.to_str().unwrap().parse::<f64>().unwrap() as i32;
            xref_pos
        }
    }
}
/*
 * This routine must be called with the file pointer located
 * at the start of the trailer.
 */
unsafe fn parse_trailer(pf: *mut pdf_file) -> Option<*mut pdf_obj> {
    /*
     * Fill work_buffer and hope trailer fits. This should
     * be made a bit more robust sometime.
     */
    let cur_pos = (*pf).handle.seek(SeekFrom::Current(0)).unwrap() as i32;
    let nmax = ((*pf).file_size - cur_pos).min(1024) as usize;
    let mut buf = vec![0u8; nmax];
    let r = (*pf).handle.read_exact(&mut buf);
    if r.is_err() || !buf.starts_with(b"trailer") {
        warn!("No trailer.  Are you sure this is a PDF file?");
        warn!("buffer:\n->{}<-\n", buf.as_slice().display());
        None
    } else {
        let mut p = &buf[b"trailer".len()..];
        p.skip_white();
        p.parse_pdf_dict(pf)
    }
}
/*
 * This routine tries to estimate an upper bound for character position
 * of the end of the object, so it knows how big the buffer must be.
 * The parsing routines require that the entire object be read into
 * memory. It would be a major pain to rewrite them.  The worst case
 * is that an object before an xref table will grab the whole table
 * :-(
 */
unsafe fn next_object_offset(pf: *mut pdf_file, obj_num: u32) -> i32 {
    let mut next: i32 = (*pf).file_size; /* Worst case */
    let curr = (*(*pf).xref_table.offset(obj_num as isize)).id.0 as i32;
    /* Check all other type 1 objects to find next one */
    for i in 0..(*pf).num_obj {
        if (*(*pf).xref_table.offset(i as isize)).typ as i32 == 1i32
            && (*(*pf).xref_table.offset(i as isize)).id.0 > curr as u32
            && (*(*pf).xref_table.offset(i as isize)).id.0 < next as u32
        {
            next = (*(*pf).xref_table.offset(i as isize)).id.0 as i32
        }
    }
    next
}

unsafe fn pdf_read_object(
    obj_num: u32,
    obj_gen: u16,
    pf: *mut pdf_file,
    offset: i32,
    limit: i32,
) -> *mut pdf_obj {
    let length = (limit - offset) as usize;
    if length <= 0 {
        return ptr::null_mut();
    }
    let mut buffer = vec![0u8; length + 1];
    (*pf).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    (*pf).handle.read_exact(&mut buffer[..length]);
    let p = buffer.as_slice();
    /* Check for obj_num and obj_gen */
    let mut q = p; /* <== p */
    q.skip_white();
    let sp = q.parse_unsigned();
    if sp.is_none() {
        return ptr::null_mut();
    }
    let n = sp.unwrap().to_str().unwrap().parse::<u32>().unwrap();
    q.skip_white();
    let sp = q.parse_unsigned();
    if sp.is_none() {
        return ptr::null_mut();
    }
    let g = sp.unwrap().to_str().unwrap().parse::<u32>().unwrap();
    if obj_num != 0 && (n != obj_num || g != obj_gen as u32) {
        return ptr::null_mut();
    }
    let mut p = q;
    p.skip_white();
    if !p.starts_with(b"obj") {
        warn!("Didn\'t find \"obj\".");
        return ptr::null_mut();
    }
    p = &p[b"obj".len()..];
    let result = p.parse_pdf_object(pf).unwrap_or(ptr::null_mut());
    p.skip_white();
    if !p.starts_with(b"endobj") {
        warn!("Didn\'t find \"endobj\".");
        pdf_release_obj(result);
        ptr::null_mut()
    } else {
        result
    }
}
unsafe fn read_objstm(pf: *mut pdf_file, num: u32) -> *mut pdf_obj {
    let current_block: u64;
    let (offset, gen) = (*(*pf).xref_table.offset(num as isize)).id;
    let limit: i32 = next_object_offset(pf, num);
    let mut data: *mut i8 = ptr::null_mut();
    let mut q: *mut i8 = ptr::null_mut();
    let mut objstm = pdf_read_object(num, gen, pf, offset as i32, limit);
    if !objstm.is_null() && (*objstm).is_stream() {
        let tmp: *mut pdf_obj = pdf_stream_uncompress(&mut *objstm);
        if !tmp.is_null() {
            pdf_release_obj(objstm);
            objstm = tmp;
            let dict = (*objstm).as_stream().get_dict();
            let typ = dict.get("Type").unwrap();
            if !(!typ.is_name() || typ.as_name().to_bytes() != b"ObjStm") {
                if let Some(n_obj) = dict.get("N").filter(|&no| (*no).is_number()) {
                    let n = n_obj.as_f64() as i32;
                    if let Some(first_obj) = dict.get("First").filter(|&fo| (*fo).is_number()) {
                        let first = first_obj.as_f64() as i32;
                        /* reject object streams without object data */
                        if !(first >= (*objstm).as_stream().len() as i32) {
                            let mut header = new(((2i32 * (n + 1i32)) as u32 as u64)
                                .wrapping_mul(::std::mem::size_of::<i32>() as u64)
                                as u32) as *mut i32;
                            set_objstm_data(&mut *objstm, header);
                            *header = n;
                            header = header.offset(1);
                            *header = first;
                            header = header.offset(1);
                            /* avoid parsing beyond offset table */
                            data = new(((first + 1i32) as u32 as u64)
                                .wrapping_mul(::std::mem::size_of::<i8>() as u64)
                                as u32) as *mut i8;
                            libc::memcpy(
                                data as *mut libc::c_void,
                                pdf_stream_dataptr(&*objstm),
                                first as usize,
                            );
                            *data.offset(first as isize) = 0_i8;
                            let mut p = data as *const i8;
                            let endptr = p.offset(first as isize);
                            let mut i = 2i32 * n;
                            loop {
                                let fresh22 = i;
                                i = i - 1;
                                if !(fresh22 != 0) {
                                    current_block = 3275366147856559585;
                                    break;
                                }
                                *header = strtoul(p, &mut q, 10i32) as i32;
                                header = header.offset(1);
                                if q == p as *mut i8 {
                                    current_block = 13429587009686472387;
                                    break;
                                }
                                p = q
                            }
                            match current_block {
                                13429587009686472387 => {}
                                _ => {
                                    /* Any garbage after last entry? */
                                    skip_white(&mut p, endptr);
                                    if !(p != endptr) {
                                        free(data as *mut libc::c_void);
                                        let ref mut fresh24 =
                                            (*(*pf).xref_table.offset(num as isize)).direct;
                                        *fresh24 = objstm;
                                        return *fresh24;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    warn!("Cannot parse object stream.");
    free(data as *mut libc::c_void);
    pdf_release_obj(objstm);
    ptr::null_mut()
}
/* Label without corresponding object definition are replaced by the
 * null object, as required by the PDF spec. This is important to parse
 * several cross-reference sections.
 */
unsafe fn pdf_get_object(pf: &mut pdf_file, obj_id: ObjectId) -> *mut pdf_obj {
    let (obj_num, obj_gen) = obj_id;
    if !(obj_num > 0_u32
        && obj_num < pf.num_obj as u32
        && ((*pf.xref_table.offset(obj_num as isize)).typ as i32 == 1i32
            && (*pf.xref_table.offset(obj_num as isize)).id.1 as i32 == obj_gen as i32
            || (*pf.xref_table.offset(obj_num as isize)).typ as i32 == 2i32 && obj_gen == 0))
    {
        warn!(
            "Trying to read nonexistent or deleted object: {} {}",
            obj_num, obj_gen,
        );
        return pdf_new_null();
    }
    let result = (*pf.xref_table.offset(obj_num as isize)).direct;
    if !result.is_null() {
        return pdf_link_obj(result);
    }
    let mut result = None;
    if (*pf.xref_table.offset(obj_num as isize)).typ as i32 == 1i32 {
        /* type == 1 */
        let offset = (*pf.xref_table.offset(obj_num as isize)).id.0;
        let limit = next_object_offset(pf, obj_num);
        result = Some(pdf_read_object(obj_num, obj_gen, pf, offset as i32, limit))
    } else {
        /* type == 2 */
        let (objstm_num, index) = (*pf.xref_table.offset(obj_num as isize)).id;
        let mut objstm: *mut pdf_obj = ptr::null_mut();
        if !(objstm_num >= pf.num_obj as u32)
            && (*pf.xref_table.offset(objstm_num as isize)).typ as i32 == 1i32
            && {
                objstm = (*pf.xref_table.offset(objstm_num as isize)).direct;
                !objstm.is_null() || {
                    objstm = read_objstm(pf, objstm_num);
                    !objstm.is_null()
                }
            }
        {
            let mut data = get_objstm_data(&*objstm);
            let n = *data;
            data = data.offset(1);
            let first = *data;
            data = data.offset(1);
            if !(index as i32 >= n)
                && *data.offset((2i32 * index as i32) as isize) as u32 == obj_num
            {
                let objstm_slice = &(*objstm).as_stream().content;

                let pdfobj_start = first + *data.offset(2 * index as isize + 1);
                let pdfobj_end = if index as i32 == n - 1 {
                    objstm_slice.len()
                } else {
                    (first + *data.offset(2 * index as isize + 3)) as usize
                };

                let mut pdfobj_slice = &objstm_slice[pdfobj_start as usize..pdfobj_end];
                result = pdfobj_slice.parse_pdf_object(pf);
            }
        }
    }

    if let Some(result) = result {
        /* Make sure the caller doesn't free this object */
        (*pf.xref_table.offset(obj_num as isize)).direct = pdf_link_obj(result);
        result
    } else {
        warn!("Could not read object from object stream.");
        pdf_new_null()
    }
}
unsafe fn pdf_new_ref(object: *mut pdf_obj) -> *mut pdf_obj {
    if (*object).label() == 0 {
        pdf_label_obj(object);
    }

    (pdf_indirect {
        pf: ptr::null_mut(),
        id: (*object).id,
        obj: object,
    })
    .into_obj()
}
/* pdf_deref_obj always returns a link instead of the original   */
/* It never return the null object, but the NULL pointer instead */

pub(crate) unsafe fn pdf_deref_obj(obj: Option<&mut pdf_obj>) -> *mut pdf_obj {
    let mut count = 30;
    let mut obj = match obj {
        None => ptr::null_mut(),
        Some(o) => o as *mut pdf_obj,
    };
    if !obj.is_null() {
        obj = pdf_link_obj(obj)
    }
    while !obj.is_null() && (*obj).is_indirect() && {
        count -= 1;
        count != 0
    } {
        if let Some(pf) = (*obj).as_indirect().pf.as_mut() {
            let obj_id = (*obj).as_indirect().id;
            pdf_release_obj(obj);
            obj = pdf_get_object(pf, obj_id)
        } else {
            let next_obj: *mut pdf_obj = (*obj).as_indirect().obj;
            if next_obj.is_null() {
                panic!("Undefined object reference");
            }
            pdf_release_obj(obj);
            obj = pdf_link_obj(next_obj)
        }
    }
    if count == 0 {
        panic!("Loop in object hierarchy detected. Broken PDF file?");
    }
    if !obj.is_null() && (&*obj).typ() == PdfObjType::NULL {
        pdf_release_obj(obj);
        ptr::null_mut()
    } else {
        obj
    }
}
unsafe fn extend_xref(pf: &mut pdf_file, new_size: i32) {
    pf.xref_table = renew(
        pf.xref_table as *mut libc::c_void,
        (new_size as u32 as u64).wrapping_mul(::std::mem::size_of::<xref_entry>() as u64) as u32,
    ) as *mut xref_entry;
    for i in (pf.num_obj as u32)..new_size as u32 {
        (*pf.xref_table.offset(i as isize)).direct = ptr::null_mut();
        (*pf.xref_table.offset(i as isize)).indirect = ptr::null_mut();
        (*pf.xref_table.offset(i as isize)).typ = 0_u8;
        (*pf.xref_table.offset(i as isize)).id = (0, 0);
    }
    pf.num_obj = new_size;
}
/* Returns < 0 for error, 1 for success, and 0 when xref stream found. */
unsafe fn parse_xref_table(pf: *mut pdf_file, xref_pos: i32) -> i32 {
    /*
     * This routine reads one xref segment. It may be called multiple times
     * on the same file.  xref tables sometimes come in pieces.
     */
    (*pf).handle.seek(SeekFrom::Start(xref_pos as u64)).unwrap();
    let buf = tt_mfreadln(255, &mut (*pf).handle);
    /* We should have already checked that "startxref" section exists. So, EOF
     * here (len = -1) is impossible. We don't treat too long line case
     * seriously.
     */
    if buf.is_err() {
        warn!("Something went wrong while reading xref table...giving up.");
        return -1;
    }
    let buf = buf.unwrap();
    let mut p = buf.as_slice();
    /* No skip_white() here. There should not be any white-spaces here. */
    if !p.starts_with(b"xref") {
        /* Might be an xref stream and not an xref table */
        return 0i32;
    }
    p = &p[b"xref".len()..];
    p.skip_white();
    if !p.is_empty() {
        warn!("Garbage after \"xref\" keyword found.");
        return -1;
    }
    loop
    /* Next line in file has first item and size of table */
    {
        let mut current_pos = (*pf).handle.seek(SeekFrom::Current(0)).unwrap();
        let buf = tt_mfreadln(255, &mut (*pf).handle);
        if buf.is_err() {
            warn!("Reading a line failed in xref table.");
            return -1i32;
        }
        let buf = buf.unwrap();
        if !buf.is_empty() {
            let mut p = buf.as_slice();
            p.skip_white();
            if !p.is_empty() {
                if p.starts_with(b"trailer") {
                    /* Backup... This is ugly, but it seems like the safest thing to
                     * do. It is possible the trailer dictionary starts on the same
                     * logical line as the word trailer. In that case, the mfgets call
                     * might have started to read the trailer dictionary and
                     * parse_trailer would fail.
                     */
                    current_pos += (buf.len() as u64) - (p.len() as u64); /* Jump to the beginning of "trailer" keyword. */
                    (*pf).handle.seek(SeekFrom::Start(current_pos)).unwrap();
                    break;
                } else {
                    /* Line containing something other than white-space characters found.
                     *
                     * Start reading xref subsection
                     *
                     * This section just reads two nusigned integers, namely, the object number
                     * of first object and the size of the xref subsection. PDF reference says
                     * that only "a space" is allowed between those two numbers but we allow
                     * more white-space characters.
                     */
                    /* Object number of the first object whithin this xref subsection. */
                    let q = p.parse_unsigned();
                    if q.is_none() {
                        warn!("An unsigned integer expected but could not find. (xref)");
                        return -1i32;
                    }
                    let q = q.unwrap();
                    let first = q.to_str().unwrap().parse::<i32>().unwrap() as u32;
                    p.skip_white();
                    /* Nnumber of objects in this xref subsection. */
                    let q = p.parse_unsigned();
                    if q.is_none() {
                        warn!("An unsigned integer expected but could not find. (xref)");
                        return -1i32;
                    }
                    let q = q.unwrap();
                    let size = q.to_str().unwrap().parse::<i32>().unwrap() as u32;
                    p.skip_white();
                    /* Check for unrecognized tokens */
                    if !p.is_empty() {
                        warn!("Unexpected token found in xref table.");
                        return -1i32;
                    }
                    /* The first line of a xref subsection OK. */
                    if ((*pf).num_obj as u32) < first.wrapping_add(size) {
                        extend_xref(&mut *pf, first.wrapping_add(size) as i32);
                    }
                    /* Start parsing xref subsection body... */
                    let mut i = first as i32;
                    /* Only white-spaces and/or comment. */
                    while (i as u32) < first.wrapping_add(size) {
                        /* PDF spec. requires each xref subsection lines being exactly 20 bytes
                         * long [including end-of-line marker(s)], offset 10 decimal digits,
                         * generation number being 5 decimal digits, and each entries delimitted
                         * by "a single space". However, we don't srtictly follow this rule:
                         * More than one "white-spaces" allowed, can be ended with a comment,
                         * and so on.
                         */
                        let buf = tt_mfreadln(255, &mut (*pf).handle);
                        if buf.is_err() {
                            warn!("Something went wrong while reading xref subsection...");
                            return -1i32;
                        }
                        let buf = buf.unwrap();
                        if !buf.is_empty() {
                            let mut p = buf.as_slice();
                            p.skip_white();
                            if p.is_empty() {
                                continue;
                            }
                            /*
                             * Don't overwrite positions that have already been set by a
                             * modified xref table.  We are working our way backwards
                             * through the reference table, so we only set "position"
                             * if it hasn't been set yet.
                             */
                            /* Offset value -- 10 digits (0 padded) */
                            let q_0 = if let Some(q_0) = p.parse_unsigned() {
                                if q_0.to_bytes().len() != 10 {
                                    /* exactly 10 digits */
                                    warn!("Offset must be a 10 digits number. (xref)");
                                    return -1i32;
                                }
                                q_0
                            } else {
                                warn!("An unsigned integer expected but could not find. (xref)");
                                return -1i32;
                            };
                            /* FIXME: Possible overflow here. Consider using strtoll(). */
                            let offset = q_0.to_str().unwrap().parse::<i32>().unwrap() as u32;
                            p.skip_white();
                            /* Generation number -- 5 digits (0 padded) */
                            let q_0 = if let Some(q_0) = p.parse_unsigned() {
                                if q_0.to_bytes().len() != 5 {
                                    /* exactly 5 digits */
                                    warn!("Expecting a 5 digits number. (xref)");
                                    return -1i32;
                                }
                                q_0
                            } else {
                                warn!("An unsigned integer expected but could not find. (xref)");
                                return -1i32;
                            };
                            let obj_gen = q_0.to_str().unwrap().parse::<i32>().unwrap() as u32;
                            p.skip_white();
                            if p.is_empty() {
                                warn!(
                                    "Unexpected EOL reached while reading a xref subsection entry."
                                );
                                return -1i32;
                            }
                            /* Flag -- a char */
                            let flag = p[0];
                            p = &p[1..];
                            p.skip_white();
                            if !p.is_empty() {
                                warn!("Garbage in xref subsection entry found...");
                                return -1;
                            } else {
                                if flag != b'n' && flag != b'f'
                                    || flag == b'n'
                                        && (offset >= (*pf).file_size as u32
                                            || offset > 0 && offset < 4)
                                {
                                    warn!(
                                        "Invalid xref table entry [{}]. PDF file is corrupt...",
                                        i,
                                    );
                                    return -1;
                                }
                            }
                            /* Everything seems to be OK. */
                            if (*(*pf).xref_table.offset(i as isize)).id.0 == 0 {
                                (*(*pf).xref_table.offset(i as isize)).typ =
                                    (flag == b'n') as i32 as u8; /* TODO: change! why? */
                                (*(*pf).xref_table.offset(i as isize)).id = (offset, obj_gen as u16)
                            }
                            i += 1
                        }
                    }
                }
            }
        }
    }
    1i32
}
unsafe fn parse_xrefstm_field(p: *mut *const i8, length: i32, def: u32) -> u32 {
    let mut val: u32 = 0_u32;
    if length == 0 {
        return def;
    }
    for _ in 0..length {
        val <<= 8;
        val |= **p as u8 as u32;
        *p = (*p).offset(1);
    }
    val
}
unsafe fn parse_xrefstm_subsec(
    pf: &mut pdf_file,
    p: *mut *const i8,
    length: *mut i32,
    W: *mut i32,
    wsum: i32,
    first: i32,
    size: i32,
) -> i32 {
    *length -= wsum * size;
    if *length < 0i32 {
        return -1i32;
    }
    if pf.num_obj < first + size {
        extend_xref(pf, first + size);
    }
    let mut e: *mut xref_entry = pf.xref_table.offset(first as isize);
    for _ in 0..size {
        let typ = parse_xrefstm_field(p, *W.offset(0), 1_u32) as u8;
        if typ as i32 > 2i32 {
            warn!("Unknown cross-reference stream entry type.");
        }
        let field2 = parse_xrefstm_field(p, *W.offset(1), 0_u32);
        let field3 = parse_xrefstm_field(p, *W.offset(2), 0_u32) as u16;
        if (*e).id.0 == 0 {
            (*e).typ = typ;
            (*e).id = (field2, field3)
        }
        e = e.offset(1)
    }
    0i32
}
unsafe fn parse_xref_stream(pf: &mut pdf_file, xref_pos: i32, trailer: *mut *mut pdf_obj) -> i32 {
    let mut current_block: u64;
    let mut W: [i32; 3] = [0; 3];
    let mut wsum: i32 = 0i32;
    let mut xrefstm = pdf_read_object(0_u32, 0_u16, pf, xref_pos, pf.file_size);
    if !xrefstm.is_null() && (*xrefstm).is_stream() {
        let tmp: *mut pdf_obj = pdf_stream_uncompress(&mut *xrefstm);
        if !tmp.is_null() {
            pdf_release_obj(xrefstm);
            xrefstm = tmp;
            *trailer = pdf_link_obj((*xrefstm).as_stream_mut().get_dict_obj());
            if let Some(size_obj) = (**trailer)
                .as_dict()
                .get("Size")
                .filter(|&so| (*so).is_number())
            {
                let size = size_obj.as_f64() as u32;
                let mut length = (*xrefstm).as_stream().len() as i32;
                let W_obj = (**trailer).as_dict().get("W").unwrap();
                if !(!W_obj.is_array() || W_obj.as_array().len() != 3) {
                    let mut i = 0;
                    loop {
                        if !(i < 3) {
                            current_block = 12147880666119273379;
                            break;
                        }
                        let tmp_0 = W_obj.as_array()[i];
                        if !(*tmp_0).is_number() {
                            current_block = 5131529843719913080;
                            break;
                        }
                        W[i] = (*tmp_0).as_f64() as i32;
                        wsum += W[i];
                        i += 1
                    }
                    match current_block {
                        5131529843719913080 => {}
                        _ => {
                            let mut p = pdf_stream_dataptr(&*xrefstm) as *const i8;
                            if let Some(index_obj) = (**trailer).as_dict().get("Index") {
                                let mut index_len = 0;
                                if !index_obj.is_array() || {
                                    index_len = index_obj.as_array().len();
                                    index_len.wrapping_rem(2) != 0
                                } {
                                    current_block = 5131529843719913080;
                                } else {
                                    let mut i = 0;
                                    loop {
                                        if !(i < index_len) {
                                            current_block = 652864300344834934;
                                            break;
                                        }
                                        let first = index_obj
                                            .as_array()
                                            .get(i)
                                            .filter(|&&o| (*o).is_number());
                                        i += 1;
                                        let size_obj = index_obj
                                            .as_array()
                                            .get(i)
                                            .filter(|&&o| (*o).is_number());
                                        i += 1;
                                        if let (Some(&first), Some(&size_obj)) = (first, size_obj) {
                                            if parse_xrefstm_subsec(
                                                pf,
                                                &mut p,
                                                &mut length,
                                                W.as_mut_ptr(),
                                                wsum,
                                                (*first).as_f64() as i32,
                                                (*size_obj).as_f64() as i32,
                                            ) != 0
                                            {
                                                current_block = 5131529843719913080;
                                                break;
                                            }
                                        } else {
                                            current_block = 5131529843719913080;
                                            break;
                                        }
                                    }
                                }
                            } else if parse_xrefstm_subsec(
                                pf,
                                &mut p,
                                &mut length,
                                W.as_mut_ptr(),
                                wsum,
                                0i32,
                                size as i32,
                            ) != 0
                            {
                                current_block = 5131529843719913080;
                            } else {
                                current_block = 652864300344834934;
                            }
                            match current_block {
                                5131529843719913080 => {}
                                _ => {
                                    if length != 0 {
                                        warn!("Garbage in xref stream.");
                                    }
                                    pdf_release_obj(xrefstm);
                                    return 1i32;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    warn!("Cannot parse cross-reference stream.");
    pdf_release_obj(xrefstm);
    if !(*trailer).is_null() {
        pdf_release_obj(*trailer);
        *trailer = ptr::null_mut()
    }
    0i32
}
/* TODO: parse Version entry */
unsafe fn read_xref(pf: &mut pdf_file) -> *mut pdf_obj {
    let mut current_block: u64;
    let mut trailer: *mut pdf_obj = ptr::null_mut();
    let mut main_trailer: *mut pdf_obj = ptr::null_mut();
    let mut xref_pos = find_xref(&mut (*pf).handle, (*pf).file_size);
    if xref_pos == 0 {
        current_block = 13794981049891343809;
    } else {
        current_block = 14916268686031723178;
    }
    loop {
        match current_block {
            14916268686031723178 => {
                if xref_pos != 0 {
                    let res: i32 = parse_xref_table(pf, xref_pos);
                    if res > 0i32 {
                        /* cross-reference table */
                        trailer = parse_trailer(pf).unwrap_or(ptr::null_mut());
                        if trailer.is_null() {
                            current_block = 13794981049891343809;
                            continue;
                        }
                        if main_trailer.is_null() {
                            main_trailer = pdf_link_obj(trailer)
                        }
                        if let Some(xrefstm) = (*trailer).as_dict().get("XRefStm") {
                            let mut new_trailer: *mut pdf_obj = ptr::null_mut();
                            if xrefstm.is_number()
                                && parse_xref_stream(pf, xrefstm.as_f64() as i32, &mut new_trailer)
                                    != 0
                            {
                                pdf_release_obj(new_trailer);
                            } else {
                                warn!("Skipping hybrid reference section.");
                            }
                            /* Many PDF 1.5 xref streams use DecodeParms, which we cannot
                               parse. This way we can use at least xref tables in hybrid
                               documents. Or should we better stop parsing the file?
                            */
                        }
                    } else {
                        if !(res == 0 && parse_xref_stream(pf, xref_pos, &mut trailer) != 0) {
                            current_block = 13794981049891343809;
                            continue;
                        }
                        /* cross-reference stream */
                        if main_trailer.is_null() {
                            main_trailer = pdf_link_obj(trailer)
                        }
                    }
                    if let Some(prev) = (*trailer).as_dict().get("Prev") {
                        if !prev.is_number() {
                            current_block = 13794981049891343809;
                            continue;
                        }
                        xref_pos = prev.as_f64() as i32
                    } else {
                        xref_pos = 0i32
                    }
                    pdf_release_obj(trailer);
                    current_block = 14916268686031723178;
                } else {
                    return main_trailer;
                }
            }
            _ => {
                warn!("Error while parsing PDF file.");
                pdf_release_obj(trailer);
                pdf_release_obj(main_trailer);
                return ptr::null_mut();
            }
        }
    }
}
use crate::dpx_dpxutil::HTHasher;
use once_cell::sync::Lazy;
use std::hash::BuildHasherDefault;

static mut pdf_files: Lazy<HashMap<Vec<u8>, Box<pdf_file>, BuildHasherDefault<HTHasher>>> =
    Lazy::new(|| HashMap::default());

impl pdf_file {
    fn new(mut handle: InFile) -> Box<Self> {
        let file_size = ttstub_input_get_size(&mut handle) as i32;
        handle.seek(SeekFrom::End(0)).unwrap();
        Box::new(Self {
            handle,
            trailer: ptr::null_mut(),
            xref_table: ptr::null_mut(),
            catalog: ptr::null_mut(),
            num_obj: 0,
            version: 0,
            file_size,
        })
    }
}
impl Drop for pdf_file {
    fn drop(&mut self) {
        unsafe {
            for i in 0..self.num_obj {
                pdf_release_obj((*self.xref_table.offset(i as isize)).direct);
                pdf_release_obj((*self.xref_table.offset(i as isize)).indirect);
            }
            free(self.xref_table as *mut libc::c_void);
            pdf_release_obj(self.trailer);
            pdf_release_obj(self.catalog);
        }
    }
}

pub unsafe fn pdf_files_init() {
    pdf_files.clear();
}

pub(crate) unsafe fn pdf_file_get_version(pf: &pdf_file) -> u32 {
    pf.version
}

pub(crate) unsafe fn pdf_file_get_trailer(pf: &pdf_file) -> *mut pdf_obj {
    pdf_link_obj(pf.trailer)
}

pub(crate) unsafe fn pdf_file_get_catalog(pf: &pdf_file) -> *mut pdf_obj {
    pf.catalog
}

pub unsafe fn pdf_open(ident: &str, mut handle: InFile) -> Option<&mut Box<pdf_file>> {
    let pf = if !ident.is_empty() {
        pdf_files.get_mut(ident.as_bytes())
    } else {
        None
    };
    if let Some(pf) = pf {
        pf.handle = handle;
        Some(pf)
    } else {
        let version = parse_pdf_version(&mut handle).unwrap_or(0);
        if version < 1 || version > pdf_version {
            warn!("pdf_open: Not a PDF 1.[1-{}] file.", pdf_version);
            /*
              Try to embed the PDF image, even if the PDF version is newer than
              the setting.
              return NULL;
            */
        }
        let mut pf = pdf_file::new(handle);
        pf.version = version;
        pf.trailer = read_xref(&mut pf);
        if pf.trailer.is_null() {
            return None;
        }
        if (*pf.trailer).as_dict().has("Encrypt") {
            warn!("PDF document is encrypted.");
            return None;
        }
        pf.catalog = pdf_deref_obj((*pf.trailer).as_dict_mut().get_mut("Root"));
        if !(!pf.catalog.is_null() && (*pf.catalog).is_dict()) {
            warn!("Cannot read PDF document catalog. Broken PDF file?");
            return None;
        }
        let new_version = pdf_deref_obj((*pf.catalog).as_dict_mut().get_mut("Version"));
        if !new_version.is_null() {
            let mut minor: u32 = 0;
            if (&*new_version).is_name() {
                let new_version_str = (*new_version).as_name().to_bytes();
                let minor_num_str = if new_version_str.starts_with(b"1.") {
                    std::str::from_utf8(&new_version_str[2..]).unwrap_or("")
                } else {
                    ""
                };
                if let Ok(minor_) = minor_num_str.parse::<u32>() {
                    minor = minor_;
                } else {
                    pdf_release_obj(new_version);
                    warn!("Illegal Version entry in document catalog. Broken PDF file?");
                    return None;
                }
            }
            if pf.version < minor {
                pf.version = minor
            }
            pdf_release_obj(new_version);
        }
        pdf_files.insert(ident.as_bytes().to_owned(), pf);
        pdf_files.get_mut(ident.as_bytes())
    }
}

pub unsafe fn pdf_files_close() {
    pdf_files.clear();
}

fn parse_pdf_version<R: Read + Seek>(handle: &mut R) -> Result<u32, ()> {
    handle.seek(SeekFrom::Start(0)).unwrap();

    let mut buffer_ = [0u8; 32];
    handle.read_exact(&mut buffer_).map_err(|_| ())?;

    let line = buffer_
        .split(|&c| c == b'\r' || c == b'\n' || c == b' ')
        .next()
        .ok_or(())?;

    let buffer = std::str::from_utf8(line)
        .map_err(|_| ())?
        .trim_end()
        .to_string();

    if !buffer.starts_with("%PDF-1.") {
        return Err(());
    }

    buffer["%PDF-1.".len()..].parse::<u32>().map_err(|_| ())
}

pub(crate) unsafe fn check_for_pdf<R: Read + Seek>(handle: &mut R) -> bool {
    match parse_pdf_version(handle) {
        Ok(version) => {
            if version <= pdf_version {
                true
            } else {
                warn!(
                    "Version of PDF file (1.{}) is newer than version limit specification.",
                    version
                );
                true
            }
        }
        Err(_) => false,
    }
}

#[inline]
unsafe fn import_dict(key: &pdf_name, value: &mut pdf_obj, pdata: &mut pdf_dict) -> i32 {
    let tmp = pdf_import_object(value);
    if tmp.is_null() {
        return -1i32;
    }
    pdata.set(key.to_bytes(), tmp); // TODO: check
    0i32
}
static mut loop_marker: pdf_obj = pdf_obj {
    id: (0, 0),
    refcount: 0_u32,
    flags: 0i32,
    data: PdfObjVariant::OBJ_INVALID,
};
unsafe fn pdf_import_indirect(object: *mut pdf_obj) -> *mut pdf_obj {
    let pf = &mut *(*object).as_indirect().pf;
    let (obj_num, obj_gen) = (*object).as_indirect().id;
    if !(obj_num > 0_u32
        && obj_num < pf.num_obj as u32
        && ((*pf.xref_table.offset(obj_num as isize)).typ as i32 == 1i32
            && (*pf.xref_table.offset(obj_num as isize)).id.1 as i32 == obj_gen as i32
            || (*pf.xref_table.offset(obj_num as isize)).typ as i32 == 2i32 && obj_gen == 0))
    {
        warn!("Can\'t resolve object: {} {}", obj_num, obj_gen as i32);
        return pdf_new_null();
    }
    let mut ref_0 = (*pf.xref_table.offset(obj_num as isize)).indirect;
    if !ref_0.is_null() {
        if ref_0 == &mut loop_marker as *mut pdf_obj {
            panic!("Loop in object hierarchy detected. Broken PDF file?");
        }
        return pdf_link_obj(ref_0);
    } else {
        let obj = pdf_get_object(&mut *pf, (obj_num, obj_gen));
        if obj.is_null() {
            warn!("Could not read object: {} {}", obj_num, obj_gen as i32);
            return ptr::null_mut();
        }
        /* We mark the reference to be able to detect loops */
        (*pf.xref_table.offset(obj_num as isize)).indirect = &mut loop_marker;
        let tmp = pdf_import_object(obj);
        ref_0 = pdf_ref_obj(tmp);
        (*pf.xref_table.offset(obj_num as isize)).indirect = ref_0;
        pdf_release_obj(tmp);
        pdf_release_obj(obj);
        return pdf_link_obj(ref_0);
    };
}
/*
 * pdf_import_object recursively copies the object and those
 * referenced by it and changes the indirect references so that
 * they refer to the current output file. New indirect references
 * are remembered, which avoids duplicating objects when they
 * are imported several times.
 */

pub(crate) unsafe fn pdf_import_object(object: *mut pdf_obj) -> *mut pdf_obj {
    match (*object).data {
        PdfObjVariant::INDIRECT(v) => {
            if !(*v).pf.is_null() {
                pdf_import_indirect(object)
            } else {
                pdf_link_obj(object)
            }
        }
        PdfObjVariant::STREAM(v) => {
            let tmp = pdf_import_object((&mut *v).get_dict_obj());
            if tmp.is_null() {
                return ptr::null_mut();
            }
            let mut imported = pdf_stream::new(0i32);
            let stream_dict = imported.get_dict_mut();
            stream_dict.merge((*tmp).as_dict());
            pdf_release_obj(tmp);
            imported.add_slice(&(*object).as_stream().content);
            imported.into_obj()
        }
        PdfObjVariant::DICT(v) => {
            let mut imported = pdf_dict::new();
            if (&mut *v).foreach(import_dict, &mut imported) < 0i32 {
                return ptr::null_mut();
            }
            imported.into_obj()
        }
        PdfObjVariant::ARRAY(v) => {
            let mut imported = vec![];
            for i in 0..(&*v).len() {
                let array = &mut *v;
                let tmp = if i < (&*array).len() {
                    pdf_import_object(array.values[i as usize])
                } else {
                    ptr::null_mut()
                };
                if tmp.is_null() {
                    return ptr::null_mut();
                }
                imported.push(tmp);
            }
            imported.into_obj()
        }
        _ => pdf_link_obj(object),
    }
}
/* returns 0 if indirect references point to the same object */

impl pdf_indirect {
    pub(crate) unsafe fn compare(&self, ref2: &Self) -> bool {
        self.pf != ref2.pf || self.id != ref2.id
    }
}

pub(crate) unsafe fn pdf_obj_reset_global_state() {
    pdf_output_handle = None;
    pdf_output_file_position = 0;
    pdf_output_line_position = 0;
    compression_saved = 0i32;
}

use self::png_crate_filter::{unfilter as png_unfilter_scanline, FilterType as PngFilterType};

/// The png crate doesn't export these, but we need them to implement the FlateDecode parameters
/// Each function does
/// MIT / Apache-2.0 dual-licenced.
mod png_crate_filter {

    // Snipped
    pub use png::FilterType;

    fn filter_paeth(a: u8, b: u8, c: u8) -> u8 {
        let ia = a as i16;
        let ib = b as i16;
        let ic = c as i16;

        let p = ia + ib - ic;

        let pa = (p - ia).abs();
        let pb = (p - ib).abs();
        let pc = (p - ic).abs();

        if pa <= pb && pa <= pc {
            a
        } else if pb <= pc {
            b
        } else {
            c
        }
    }

    pub fn unfilter(
        filter: FilterType,
        bpp: usize,
        previous: &[u8],
        current: &mut [u8],
    ) -> std::result::Result<(), &'static str> {
        use self::FilterType::*;
        assert!(bpp > 0);
        let len = current.len();

        match filter {
            NoFilter => Ok(()),
            Sub => {
                for i in bpp..len {
                    current[i] = current[i].wrapping_add(current[i - bpp]);
                }
                Ok(())
            }
            Up => {
                if previous.len() < len {
                    Err("Filtering failed: not enough data in previous row")
                } else {
                    for i in 0..len {
                        current[i] = current[i].wrapping_add(previous[i]);
                    }
                    Ok(())
                }
            }
            Avg => {
                if previous.len() < len {
                    Err("Filtering failed:  not enough data in previous row")
                } else if bpp > len {
                    Err("Filtering failed: bytes per pixel is greater than length of row")
                } else {
                    for i in 0..bpp {
                        current[i] = current[i].wrapping_add(previous[i] / 2);
                    }

                    for i in bpp..len {
                        current[i] = current[i].wrapping_add(
                            ((current[i - bpp] as i16 + previous[i] as i16) / 2) as u8,
                        );
                    }
                    Ok(())
                }
            }
            Paeth => {
                if previous.len() < len {
                    Err("Filtering failed: not enough data in previous row")
                } else if bpp > len {
                    Err("Filtering failed: bytes per pixel is greater than length of row")
                } else {
                    for i in 0..bpp {
                        current[i] = current[i].wrapping_add(filter_paeth(0, previous[i], 0));
                    }

                    for i in bpp..len {
                        current[i] = current[i].wrapping_add(filter_paeth(
                            current[i - bpp],
                            previous[i],
                            previous[i - bpp],
                        ));
                    }
                    Ok(())
                }
            }
        }
    }

    #[allow(unused)]
    pub fn filter(method: FilterType, bpp: usize, previous: &[u8], current: &mut [u8]) {
        use self::FilterType::*;
        assert!(bpp > 0);
        let len = current.len();

        match method {
            NoFilter => (),
            Sub => {
                for i in (bpp..len).rev() {
                    current[i] = current[i].wrapping_sub(current[i - bpp]);
                }
            }
            Up => {
                for i in 0..len {
                    current[i] = current[i].wrapping_sub(previous[i]);
                }
            }
            Avg => {
                for i in (bpp..len).rev() {
                    current[i] =
                        current[i].wrapping_sub(current[i - bpp].wrapping_add(previous[i]) / 2);
                }

                for i in 0..bpp {
                    current[i] = current[i].wrapping_sub(previous[i] / 2);
                }
            }
            Paeth => {
                for i in (bpp..len).rev() {
                    current[i] = current[i].wrapping_sub(filter_paeth(
                        current[i - bpp],
                        previous[i],
                        previous[i - bpp],
                    ));
                }

                for i in 0..bpp {
                    current[i] = current[i].wrapping_sub(filter_paeth(0, previous[i], 0));
                }
            }
        }
    }
}
