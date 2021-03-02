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

use crate::bridge::DisplayExt;
use std::ffi::CString;
use std::io::{Read, Seek, SeekFrom, Write};

use crate::dpx_pdfparse::{ParseNumber, ParsePdfObj, SkipWhite};
use crate::{info, warn};
use std::ffi::CStr;
use std::ptr;

use super::dpx_mfileio::{tt_mfgets, work_buffer};
use super::dpx_pdfdev::pdf_sprint_number;
use super::dpx_pdfencrypt::{pdf_enc_set_generation, pdf_enc_set_label, pdf_encrypt_data};
use super::dpx_pdfparse::skip_white;
use crate::bridge::{
    ttstub_input_get_size, ttstub_output_close, ttstub_output_open_stdout, ttstub_output_putc,
    ReadByte,
};
use libc::{strlen, strtoul};

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

const OBJSTM_MAX_OBJS: usize = 200; // the limit is only 100 for linearized PDF

/// (label, generation)
pub(crate) type ObjectId = (u32, u16);

pub struct pdf_obj {
    pub(crate) id: ObjectId,
    pub(crate) refcount: u32,
    pub(crate) flags: i32,
    pub(crate) data: Object,
}

impl pdf_obj {
    pub(crate) fn label(&self) -> u32 {
        self.id.0
    }
}

#[derive(Debug)]
pub enum Object {
    Invalid,
    Undefined,
    Null,
    Boolean(bool),
    Number(f64),
    String(pdf_string),
    Name(pdf_name),
    Array(Array),
    Dict(pdf_dict),
    Stream(pdf_stream),
    Indirect(pdf_indirect),
}

#[derive(Debug)]
pub struct Array(Vec<*mut pdf_obj>);
impl core::ops::Deref for Array {
    type Target = Vec<*mut pdf_obj>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl core::ops::DerefMut for Array {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl Drop for Array {
    fn drop(&mut self) {
        // TODO: check order
        while let Some(o) = self.pop() {
            unsafe { pdf_release_obj(o) }
        }
    }
}

impl Object {
    pub(crate) fn is_indirect(&self) -> bool {
        matches!(self, Self::Indirect(_))
    }
    pub(crate) fn is_invalid(&self) -> bool {
        matches!(self, Self::Invalid)
    }
    pub(crate) fn is_undefined(&self) -> bool {
        matches!(self, Self::Undefined)
    }
    pub(crate) unsafe fn as_f64(&self) -> f64 {
        if let Self::Number(v) = self {
            *v
        } else {
            panic!("invalid pdfobj::as_f64");
        }
    }
    pub(crate) unsafe fn as_dict(&self) -> &pdf_dict {
        if let Self::Dict(v) = self {
            v
        } else {
            panic!("invalid pdfobj::as_dict");
        }
    }
    pub(crate) unsafe fn as_dict_mut(&mut self) -> &mut pdf_dict {
        if let Self::Dict(v) = self {
            return v;
        }
        panic!("not a dict");
    }
    pub(crate) unsafe fn as_array(&self) -> &Vec<*mut pdf_obj> {
        if let Self::Array(v) = self {
            v
        } else {
            panic!("invalid pdfobj::as_array");
        }
    }
    pub(crate) unsafe fn as_array_mut(&mut self) -> &mut Vec<*mut pdf_obj> {
        if let Self::Array(v) = self {
            v
        } else {
            panic!("invalid pdfobj::as_array_mut");
        }
    }
    pub(crate) unsafe fn as_stream(&self) -> &pdf_stream {
        if let Self::Stream(v) = self {
            v
        } else {
            panic!("invalid pdfobj::as_stream");
        }
    }
    pub(crate) unsafe fn as_stream_mut(&mut self) -> &mut pdf_stream {
        if let Self::Stream(v) = self {
            v
        } else {
            panic!("invalid pdfobj::as_stream_mut");
        }
    }
    pub(crate) unsafe fn as_string(&self) -> &pdf_string {
        if let Self::String(v) = self {
            v
        } else {
            panic!("invalid pdfobj::as_string");
        }
    }
    pub(crate) unsafe fn as_name(&self) -> &CStr {
        if let Self::Name(v) = self {
            v.name.as_c_str()
        } else {
            panic!("invalid pdfobj::as_name");
        }
    }
    pub(crate) unsafe fn as_indirect(&self) -> &pdf_indirect {
        if let Self::Indirect(v) = self {
            &v
        } else {
            panic!("invalid pdfobj::as_indirect");
        }
    }
}
impl std::ops::Deref for pdf_obj {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl std::ops::DerefMut for pdf_obj {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[repr(C)]
pub struct pdf_file {
    pub(crate) handle: InFile,
    pub(crate) trailer: *mut pdf_obj,
    pub(crate) xref_table: Vec<xref_entry>,
    pub(crate) catalog: *mut pdf_obj,
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

#[derive(Debug)]
#[repr(C)]
pub struct pdf_dict {
    inner: IndexMap<pdf_name, *mut pdf_obj>,
}
#[derive(Clone, Debug)]
#[repr(C)]
pub struct pdf_stream {
    pub(crate) dict: *mut pdf_obj,
    pub(crate) content: Vec<u8>,
    pub(crate) objstm_data: Box<[i32]>,
    pub(crate) _flags: i32,
    pub(crate) decodeparms: decode_parms,
}
#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub(crate) struct decode_parms {
    pub(crate) predictor: i32,
    pub(crate) colors: i32,
    pub(crate) bits_per_component: i32,
    pub(crate) columns: i32,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct pdf_name {
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

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct pdf_indirect {
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
pub struct pdf_array {
    pub(crate) values: Vec<*mut pdf_obj>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct pdf_string {
    pub(crate) string: Vec<u8>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_number {
    pub(crate) value: f64,
}

pub(crate) trait IntoObj {
    fn into_pdf_obj(self) -> pdf_obj;
    #[inline(always)]
    fn into_obj_box(self) -> Box<pdf_obj>
    where
        Self: Sized,
    {
        Box::new(self.into_pdf_obj())
    }
    #[inline(always)]
    fn into_obj(self) -> *mut pdf_obj
    where
        Self: Sized,
    {
        Box::into_raw(self.into_obj_box())
    }
}
impl<T> IntoObj for T
where
    T: Into<Object>,
{
    #[inline(always)]
    fn into_pdf_obj(self) -> pdf_obj
    where
        Self: Sized,
    {
        let data: Object = self.into();
        let flags = if let Object::Stream(_) = &data {
            OBJ_NO_OBJSTM
        } else {
            0
        };
        pdf_obj {
            data,
            id: (0, 0),
            refcount: 1,
            flags,
        }
    }
}
impl IntoObj for *mut pdf_obj {
    fn into_pdf_obj(self) -> pdf_obj {
        unreachable!()
    }
    fn into_obj_box(self) -> Box<pdf_obj> {
        unreachable!()
    }
    #[inline(always)]
    fn into_obj(self) -> Self {
        self
    }
}

impl From<f64> for Object {
    #[inline(always)]
    fn from(f: f64) -> Self {
        Self::Number(f)
    }
}
impl From<bool> for Object {
    #[inline(always)]
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}
impl From<&str> for Object {
    #[inline(always)]
    fn from(s: &str) -> Self {
        pdf_name::new(s).into()
    }
}
impl From<Vec<*mut pdf_obj>> for Object {
    #[inline(always)]
    fn from(v: Vec<*mut pdf_obj>) -> Self {
        Self::Array(Array(v))
    }
}
impl From<pdf_name> for Object {
    #[inline(always)]
    fn from(n: pdf_name) -> Self {
        Self::Name(n)
    }
}
impl From<pdf_string> for Object {
    #[inline(always)]
    fn from(s: pdf_string) -> Self {
        Self::String(s)
    }
}
impl From<pdf_stream> for Object {
    #[inline(always)]
    fn from(s: pdf_stream) -> Self {
        Self::Stream(s)
    }
}
impl From<pdf_dict> for Object {
    fn from(d: pdf_dict) -> Self {
        Self::Dict(d)
    }
}
impl From<pdf_indirect> for Object {
    fn from(r: pdf_indirect) -> Self {
        Self::Indirect(r)
    }
}

pub(crate) trait IntoRef {
    unsafe fn into_ref(self) -> pdf_indirect;
    unsafe fn into_ref_with_no_encrypt(self) -> pdf_indirect;
}
impl<T> IntoRef for T
where
    T: Into<Object>,
{
    // Writes object data to file and convert it into `pdf_indirect`
    unsafe fn into_ref(self) -> pdf_indirect {
        let mut object: Object = self.into();
        let flags = if let Object::Stream(_) = &object {
            OBJ_NO_OBJSTM
        } else {
            0
        };
        let id = pdf_next_label();
        output_pdf_obj(&mut object, id, flags);

        pdf_indirect {
            pf: ptr::null_mut(),
            id: id,
            obj: ptr::null_mut(),
        }
    }
    unsafe fn into_ref_with_no_encrypt(self) -> pdf_indirect {
        let mut object: Object = self.into();
        let flags = if let Object::Stream(_) = &object {
            OBJ_NO_OBJSTM
        } else {
            0
        };
        let id = pdf_next_label();
        output_pdf_obj(&mut object, id, flags | OBJ_NO_ENCRYPT);

        pdf_indirect {
            pf: ptr::null_mut(),
            id: id,
            obj: ptr::null_mut(),
        }
    }
}

static mut pdf_output_handle: Option<OutputHandleWrapper> = None;
static mut pdf_output_file_position: usize = 0;
static mut pdf_output_line_position: usize = 0;
static mut compression_saved: i32 = 0;
static mut output_xref: Vec<xref_entry> = Vec::new();
static mut pdf_max_ind_objects: usize = 0;
static mut next_label: usize = 0;
static mut startxref: u32 = 0;
static mut output_stream: *mut pdf_stream = ptr::null_mut();
/* the limit is only 100 for linearized PDF */
static mut enc_mode: bool = false;
static mut doc_enc_mode: bool = false;
static mut trailer_dict: *mut pdf_obj = ptr::null_mut();
static mut xref_stream: *mut pdf_obj = ptr::null_mut();
static mut verbose: i32 = 0;
static mut compression_level: i8 = 9_i8;
static mut compression_use_predictor: i8 = 1_i8;

pub(crate) unsafe fn pdf_set_compression(level: i32) {
    if cfg!(not(feature = "libz-sys")) {
        panic!(
            "You don\'t have compression compiled in. Possibly libz wasn\'t found by configure."
        );
    }
    if cfg!(feature = "legacy-libz") && level != 0 {
        warn!("Unable to set compression level -- your zlib doesn\'t have compress2().");
    }
    if level >= 0 && level <= 9 {
        compression_level = level as i8
    } else {
        panic!("set_compression: invalid compression level: {}", level);
    };
}

pub(crate) unsafe fn pdf_set_use_predictor(bval: i32) {
    compression_use_predictor = (if bval != 0 { 1 } else { 0 }) as i8;
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

/*pub(crate) unsafe fn pdf_obj_get_verbose() -> i32 {
    verbose
}*/

pub(crate) unsafe fn pdf_obj_set_verbose(level: i32) {
    verbose = level;
}
static mut current_objstm: Option<(pdf_stream, ObjectId)> = None;
static mut do_objstm: bool = false;
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

pub(crate) unsafe fn pdf_out_init(filename: &str, do_encryption: bool, enable_object_stream: bool) {
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
            do_objstm = true;
        } else {
            trailer_dict = pdf_dict::new().into_obj();
            do_objstm = false;
        }
    } else {
        xref_stream = ptr::null_mut();
        trailer_dict = pdf_dict::new().into_obj();
        do_objstm = false;
    }
    output_stream = ptr::null_mut();
    if filename.is_empty() {
        panic!("stdout PDF output not supported");
    }
    pdf_output_handle = OutputHandleWrapper::open(&filename, 0);
    if pdf_output_handle.is_none() {
        if filename.len() < 128 {
            panic!("Unable to open \"{}\".", filename);
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
        pos >>= 8; /* offset (big-endian) */
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
        buf[poslen.wrapping_add(1_u32) as usize] = (f3 as i32 >> 8) as u8;
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
        if current_objstm.is_some() {
            release_objstm(current_objstm.take().unwrap());
        }
        /*
         * Label xref stream - we need the number of correct objects
         * for the xref stream dictionary (= trailer).
         * Labelling it in pdf_out_init (with 1)  does not work (why?).
         */
        if !xref_stream.is_null() {
            pdf_label_obj(&mut *xref_stream);
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
            if compression_level as i32 > 0 {
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

pub(crate) unsafe fn pdf_set_root(object: &mut pdf_obj) {
    if (*trailer_dict)
        .as_dict_mut()
        .set("Root", pdf_new_ref(object))
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
        object.flags |= OBJ_NO_OBJSTM;
    }
}
pub(crate) unsafe fn pdf_set_info(object: &mut pdf_obj) {
    if (*trailer_dict)
        .as_dict_mut()
        .set("Info", pdf_new_ref(object))
        != 0
    {
        panic!("Info object already set!");
    }
}
pub(crate) unsafe fn pdf_set_id(id: Vec<*mut pdf_obj>) {
    if (*trailer_dict).as_dict_mut().set("ID", id) != 0 {
        panic!("ID already set!");
    }
}
pub(crate) unsafe fn pdf_set_encrypt(encrypt: pdf_dict) {
    if (*trailer_dict)
        .as_dict_mut()
        .set("Encrypt", encrypt.into_ref_with_no_encrypt())
        != 0
    {
        panic!("Encrypt object already set!");
    }
}
unsafe fn pdf_out_char(handle: &mut OutputHandleWrapper, c: u8) {
    match output_stream.as_mut() {
        Some(os) if handle == pdf_output_handle.as_mut().unwrap() => {
            os.add_slice([c].as_ref());
        }
        _ => {
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
        }
    }
}
const xchar: &[u8; 17] = b"0123456789abcdef\x00";

unsafe fn pdf_out(handle: &mut OutputHandleWrapper, buffer: &[u8]) {
    let length = buffer.len();
    if !output_stream.is_null() && handle == pdf_output_handle.as_mut().unwrap() {
        (*output_stream).add_slice(buffer);
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

///  prints white-space character is necessary to separate objects
unsafe fn pdf_out_white(handle: &mut OutputHandleWrapper) {
    if handle == pdf_output_handle.as_mut().unwrap() && pdf_output_line_position >= 80 {
        pdf_out_char(handle, b'\n');
    } else {
        pdf_out_char(handle, b' ');
    };
}

unsafe fn pdf_label_obj(object: &mut pdf_obj) {
    if object.is_invalid() {
        panic!("pdf_label_obj(): passed invalid object.");
    }
    /*
     * Don't change label on an already labeled object. Ignore such calls.
     */
    if object.label() == 0 {
        object.id = pdf_next_label();
    };
}
fn pdf_next_label() -> ObjectId {
    unsafe {
        let id = (next_label as u32, 0);
        next_label += 1;
        id
    }
}

/*
 * Transfer the label assigned to the object src to the object dst.
 * The object dst must not yet have been labeled.
 */

pub(crate) unsafe fn pdf_transfer_label(dst: &mut pdf_obj, src: &mut pdf_obj) {
    assert!(dst.label() == 0);
    dst.id = src.id;
    src.id = (0, 0);
}
/*
 * This doesn't really copy the object, but allows it to be used without
 * fear that somebody else will free it.
 */

pub(crate) unsafe fn pdf_link_obj(mut object: *mut pdf_obj) -> *mut pdf_obj {
    if object.is_null() || (*object).is_invalid() {
        panic!("pdf_link_obj(): passed invalid object.");
    }
    (*object).refcount += 1;
    object
}

pub(crate) unsafe fn pdf_ref_obj(object: *mut pdf_obj) -> *mut pdf_obj {
    if object.is_null() || (*object).is_invalid() {
        panic!("pdf_ref_obj(): passed invalid object.");
    }
    let object = &mut *object;
    if object.refcount == 0 {
        info!("\nTrying to refer already released object!!!\n");
        pdf_write_obj(
            &mut object.data,
            ttstub_output_open_stdout().as_mut().unwrap(),
        );
        panic!("Cannot continue...");
    }
    if object.is_indirect() {
        pdf_link_obj(object)
    } else {
        pdf_new_ref(object).into_obj()
    }
}
unsafe fn write_indirect(indirect: &mut pdf_indirect, handle: &mut OutputHandleWrapper) {
    let (label, generation) = indirect.id;
    pdf_out(handle, format!("{} {} R", label, generation).as_bytes());
}
/* The undefined object is used as a placeholder in pdfnames.c
 * for objects which are referenced before they are defined.
 */

unsafe fn write_null(handle: &mut OutputHandleWrapper) {
    pdf_out(handle, b"null");
}

unsafe fn write_boolean(data: bool, handle: &mut OutputHandleWrapper) {
    pdf_out(handle, if data { b"true" } else { b"false" });
}

unsafe fn write_number(number: f64, handle: &mut OutputHandleWrapper) {
    let mut buf = Vec::new();
    pdf_sprint_number(&mut buf, number);
    pdf_out(handle, &buf);
}

pub(crate) unsafe fn pdf_set_number(object: &mut pdf_obj, value: f64) {
    if let Object::Number(v) = &mut object.data {
        *v = value;
    } else {
        panic!("not a number");
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
    let s;
    let mut nescc: i32 = 0;
    let len;
    let mut cipher;
    if enc_mode {
        cipher = pdf_encrypt_data(strn.to_bytes());
        len = cipher.len() as _;
        s = cipher.as_mut_ptr();
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
                xchar[(*s.offset(i as isize) as i32 >> 4 & 0xf) as usize],
            );
            pdf_out_char(handle, xchar[(*s.offset(i as isize) as i32 & 0xf) as usize]);
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
            pdfobj_escape_str(&mut wbuf, &mut *s.offset(i as isize), 1 as size_t);
            pdf_out(handle, wbuf.as_slice());
            wbuf.clear();
        }
        pdf_out_char(handle, b')');
    }
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

unsafe fn write_array(array: &Vec<*mut pdf_obj>, handle: &mut OutputHandleWrapper) {
    pdf_out_char(handle, b'[');
    if !array.is_empty() {
        let mut white_left = false;
        for i in 0..array.len() {
            if let Some(item) = array[i as usize].as_mut() {
                let white_right = !matches!(
                    item.data,
                    Object::String(_) | Object::Name(_) | Object::Array(_) | Object::Dict(_)
                );
                if white_left && white_right {
                    pdf_out_white(handle);
                }
                white_left = !matches!(
                    item.data,
                    Object::String(_) | Object::Array(_) | Object::Dict(_)
                );
                pdf_write_obj(&mut item.data, handle);
            } else {
                warn!("PDF array element {} undefined.", i);
            }
        }
    }
    pdf_out_char(handle, b']');
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
        if !matches!(
            (*v).data,
            Object::String(_) | Object::Name(_) | Object::Array(_) | Object::Dict(_)
        ) {
            pdf_out_white(handle);
        }
        if let Some(v) = v.as_mut() {
            pdf_write_obj(&mut v.data, handle);
        } else {
            write_null(handle);
        }
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
        if !value.is_null() && (*value).is_invalid() {
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
        let mut error: i32 = 0;
        for (k, &v) in self.inner.iter() {
            if error != 0 {
                break;
            }
            error = f(k, v, pdata);
        }
        error
    }

    pub(crate) fn has<K>(&self, name: K) -> bool
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

pub(crate) unsafe fn pdf_remove_dict<K>(dict: &mut pdf_dict, name: K)
where
    K: AsRef<[u8]>,
{
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
                predictor: 2,
                columns: 0,
                bits_per_component: 0,
                colors: 0,
            },
            objstm_data: Vec::new().into_boxed_slice(),
            content: Vec::new(),
        }
    }
}

pub(crate) unsafe fn pdf_stream_set_predictor(
    stream: &mut pdf_stream,
    predictor: i32,
    columns: i32,
    bpc: i32,
    colors: i32,
) {
    if columns < 0 || bpc < 0 || colors < 0 {
        return;
    }
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
fn filter_PNG15_apply_filter(
    raster: &[u8],
    columns: i32,
    rows: usize,
    bpc: i8,
    colors: i8,
) -> Vec<u8> {
    let bits_per_pixel: i32 = colors as i32 * bpc as i32;
    let bytes_per_pixel = ((bits_per_pixel + 7) / 8) as usize;
    let rowbytes = (columns as usize) * bytes_per_pixel;
    /* Result */
    let mut dst = vec![0_u8; (rowbytes + 1) * rows];
    for j in 0..rows {
        let pp = &mut dst[(j * (rowbytes + 1)) as usize..((j + 1) * (rowbytes + 1)) as usize];
        let p = &raster[(j * rowbytes) as usize..((j + 1) * rowbytes) as usize];
        let prev_row = if j > 0 {
            &raster[((j - 1) * rowbytes) as usize..(j * rowbytes) as usize]
        } else {
            &[]
        };
        let mut sum: [u32; 5] = [0; 5];
        /* First calculated sum of values to make a heuristic guess
         * of optimal predictor function.
         */
        for i in 0..rowbytes {
            let left: i32 = if i >= bytes_per_pixel {
                p[i - bytes_per_pixel] as i32
            } else {
                0
            };
            let up: i32 = if j > 0 { prev_row[i] as i32 } else { 0 };
            let uplft: i32 = if j > 0 {
                if i >= bytes_per_pixel {
                    prev_row[i - bytes_per_pixel] as i32
                } else {
                    0
                }
            } else {
                0
            };
            /* Type 0 -- None */
            sum[0] += p[i] as u32;
            /* Type 1 -- Sub */
            sum[1] += (p[i] as i32 - left).abs() as u32;
            /* Type 2 -- Up */
            sum[2] += (p[i] as i32 - up).abs() as u32;
            /* Type 3 -- Average */
            let tmp: i32 = (((up + left) / 2) as f64).floor() as i32;
            sum[3] += (p[i] as i32 - tmp).abs() as u32;
            /* Type 4 -- Peath */
            let q: i32 = left + up - uplft;
            let qa: i32 = (q - left).abs();
            let qb: i32 = (q - up).abs();
            let qc: i32 = (q - uplft).abs();
            if qa <= qb && qa <= qc {
                sum[4] += (p[i] as i32 - left).abs() as u32;
            } else if qb <= qc {
                sum[4] += (p[i] as i32 - up).abs() as u32;
            } else {
                sum[4] += (p[i] as i32 - uplft).abs() as u32;
            }
        }
        let mut min: i32 = sum[0] as i32;
        let mut min_idx = 0;
        for i in 0..5 {
            if sum[i] < min as u32 {
                min = sum[i] as i32;
                min_idx = i
            }
        }
        let typ = min_idx;
        /* Now we actually apply filter. */
        pp[0] = typ as u8;
        match typ {
            0 => {
                pp[1..].copy_from_slice(p);
            }
            1 => {
                for i in 0..rowbytes {
                    let left_0 = if i >= bytes_per_pixel {
                        p[i - bytes_per_pixel] as i32
                    } else {
                        0
                    };
                    pp[i + 1] = (p[i] as i32 - left_0) as u8;
                }
            }
            2 => {
                for i in 0..rowbytes {
                    let up_0: i32 = if j > 0 { prev_row[i] as i32 } else { 0 };
                    pp[i + 1] = (p[i] as i32 - up_0) as u8;
                }
            }
            3 => {
                for i in 0..rowbytes {
                    let up_1: i32 = if j > 0 { prev_row[i] as i32 } else { 0 };
                    let left_1: i32 = if i >= bytes_per_pixel {
                        p[i - bytes_per_pixel] as i32
                    } else {
                        0
                    };
                    let tmp_0: i32 = (((up_1 + left_1) / 2) as f64).floor() as i32;
                    pp[i + 1] = (p[i] as i32 - tmp_0) as u8;
                }
            }
            4 => {
                /* Peath */
                for i in 0..rowbytes {
                    let up_2: i32 = if j > 0 { prev_row[i] as i32 } else { 0 };
                    let left_2: i32 = if i >= bytes_per_pixel {
                        p[i - bytes_per_pixel] as i32
                    } else {
                        0
                    };
                    let uplft_0 = if j > 0 {
                        if i >= bytes_per_pixel {
                            prev_row[i - bytes_per_pixel] as i32
                        } else {
                            0
                        }
                    } else {
                        0
                    };
                    let q_0: i32 = left_2 + up_2 - uplft_0;
                    let qa_0: i32 = (q_0 - left_2).abs();
                    let qb_0: i32 = (q_0 - up_2).abs();
                    let qc_0: i32 = (q_0 - uplft_0).abs();
                    if qa_0 <= qb_0 && qa_0 <= qc_0 {
                        pp[i + 1] = (p[i] as i32 - left_2) as u8
                    } else if qb_0 <= qc_0 {
                        pp[i + 1] = (p[i] as i32 - up_2) as u8
                    } else {
                        pp[i + 1] = (p[i] as i32 - uplft_0) as u8
                    }
                }
            }
            _ => {}
        }
    }
    dst
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
fn apply_filter_TIFF2_1_2_4(raster: &mut [u8], width: i32, height: i32, bpc: i8, num_comp: i8) {
    let rowbytes = ((bpc as i32 * num_comp as i32 * width + 7) / 8) as usize;
    let mask: u8 = ((1 << bpc as i32) - 1) as u8;
    assert!(bpc as i32 > 0 && bpc as i32 <= 8);
    let mut prev = vec![0_u16; num_comp as _];
    /* Generic routine for 1 to 16 bit.
     * It supports, e.g., 7 bpc images too.
     * Actually, it is not necessary to have 16 bit inbuf and outbuf
     * since we only need 1, 2, and 4 bit support here. 8 bit is enough.
     */
    for j in 0..height as usize {
        prev.fill(0);
        let mut outbuf = 0 as u16;
        let mut inbuf = outbuf;
        let mut outbits = 0;
        let mut inbits = outbits;
        let mut k = j * rowbytes;
        let mut l = k;
        for _ in 0..width {
            for c in 0..num_comp as usize {
                if inbits < bpc as i32 {
                    /* need more byte */
                    inbuf = ((inbuf as i32) << 8 | raster[l] as i32) as u16; /* consumed bpc bits */
                    l += 1;
                    inbits += 8
                }
                let cur = (inbuf as i32 >> inbits - bpc as i32 & mask as i32) as u8;
                inbits -= bpc as i32;
                let mut sub = (cur as i32 - prev[c] as i32) as i8;
                prev[c] = cur as u16;
                if (sub as i32) < 0 {
                    sub = (sub as i32 + (1 << bpc as i32)) as i8
                }
                /* Append newly filtered component value */
                outbuf = ((outbuf as i32) << bpc as i32 | sub as i32) as u16;
                outbits += bpc as i32;
                /* flush */
                if outbits >= 8 {
                    raster[k] = (outbuf as i32 >> outbits - 8) as u8;
                    k += 1;
                    outbits -= 8;
                }
            }
        }
        if outbits > 0 {
            raster[k as usize] = ((outbuf as i32) << 8 - outbits) as u8;
        }
    }
}
#[cfg(feature = "libz-sys")]
fn filter_TIFF2_apply_filter(
    raster: &[u8],
    columns: i32,
    rows: i32,
    bpc: i8,
    colors: i8,
) -> Vec<u8> {
    let rowbytes: i32 = (bpc as i32 * colors as i32 * columns + 7) / 8;
    let mut dst = Vec::from(&raster[..(rowbytes * rows) as _]);
    match bpc as i32 {
        1 | 2 | 4 => {
            apply_filter_TIFF2_1_2_4(&mut dst, columns, rows, bpc, colors);
        }
        8 => {
            let mut prev = vec![0_u16; colors as _];
            for j in 0..rows {
                prev.fill(0);
                for i in 0..columns {
                    let pos = (colors as i32 * (columns * j + i)) as usize;
                    for c in 0..colors as usize {
                        let cur: u8 = raster[pos + c];
                        let sub: i32 = cur as i32 - prev[c] as i32;
                        prev[c] = cur as u16;
                        dst[pos + c] = sub as u8;
                    }
                }
            }
        }
        16 => {
            let mut prev = vec![0_u16; colors as _];
            for j in 0..rows {
                prev.fill(0);
                for i in 0..columns {
                    let pos = (2 * colors as i32 * (columns * j + i)) as usize;
                    for c in 0..colors as usize {
                        let cur: u16 = (raster[pos + 2 * c] as i32 * 256
                            + raster[pos + 2 * c + 1] as i32)
                            as u16;
                        let sub: u16 = (cur as i32 - prev[c] as i32) as u16;
                        prev[c] = cur;
                        dst[pos + 2 * c] = (sub as i32 >> 8 & 0xff) as u8;
                        dst[pos + 2 * c + 1] = (sub as i32 & 0xff) as u8;
                    }
                }
            }
        }
        _ => {}
    }
    dst
}
#[cfg(feature = "libz-sys")]
unsafe fn filter_create_predictor_dict(
    predictor: i32,
    columns: i32,
    bpc: i32,
    colors: i32,
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
    let mut filtered = stream.content.clone();
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
            && compression_level as i32 > 0
        {
            /* First apply predictor filter if requested. */
            if compression_use_predictor as i32 != 0
                && stream._flags & STREAM_USE_PREDICTOR != 0
                && !(*stream.dict).as_dict().has("DecodeParms")
            {
                let bits_per_pixel: i32 =
                    stream.decodeparms.colors * stream.decodeparms.bits_per_component;
                let len: i32 = (stream.decodeparms.columns * bits_per_pixel + 7) / 8;
                let rows: i32 = (stream.content.len() as i32) / len;
                let mut filtered2 = None;
                let parms = filter_create_predictor_dict(
                    stream.decodeparms.predictor,
                    stream.decodeparms.columns,
                    stream.decodeparms.bits_per_component,
                    stream.decodeparms.colors,
                );
                match stream.decodeparms.predictor {
                    2 => {
                        /* TIFF2 */
                        filtered2 = Some(filter_TIFF2_apply_filter(
                            &filtered,
                            stream.decodeparms.columns,
                            rows,
                            stream.decodeparms.bits_per_component as i8,
                            stream.decodeparms.colors as i8,
                        ));
                    }
                    15 => {
                        /* PNG optimun */
                        filtered2 = Some(filter_PNG15_apply_filter(
                            &filtered,
                            stream.decodeparms.columns,
                            rows as usize,
                            stream.decodeparms.bits_per_component as i8,
                            stream.decodeparms.colors as i8,
                        ));
                    }
                    _ => {
                        warn!(
                            "Unknown/unsupported Predictor function {}.",
                            stream.decodeparms.predictor
                        );
                    }
                }
                if let Some(filtered2) = filtered2 {
                    filtered = filtered2;
                    (*stream.dict).as_dict_mut().set("DecodeParms", parms);
                }
            }
            let filters = (*stream.dict).as_dict_mut().get_mut("Filter");
            let mut buffer_length = (filtered.len() as u32)
                .wrapping_add((filtered.len() as u32).wrapping_div(1000 as u32))
                .wrapping_add(14 as u32) as libz::uLong;
            let mut buffer = vec![0_u8; buffer_length as _];
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
                    buffer.as_mut_ptr(),
                    &mut buffer_length,
                    filtered.as_ptr(),
                    filtered.len() as _,
                    compression_level as i32,
                ) != 0
                {
                    panic!("Zlib error");
                }
            }
            #[cfg(feature = "legacy-libz")]
            {
                if libz::compress(
                    buffer.as_mut_ptr(),
                    &mut buffer_length,
                    filtered.as_ptr(),
                    filtered.len() as _,
                ) != 0
                {
                    panic!("Zlib error");
                }
            }
            compression_saved = (compression_saved as u64).wrapping_add(
                (filtered.len() as u64)
                    .wrapping_sub(buffer_length as u64)
                    .wrapping_sub(if has_filters {
                        strlen(b"/FlateDecode \x00" as *const u8 as *const i8)
                    } else {
                        strlen(b"/Filter/FlateDecode\n\x00" as *const u8 as *const i8)
                    } as u64),
            ) as i32;
            buffer.truncate(buffer_length as _);
            filtered = buffer;
        }
    }
    /* HAVE_ZLIB */
    /* AES will change the size of data! */
    if enc_mode {
        filtered = pdf_encrypt_data(&filtered);
    }
    (*stream.dict)
        .as_dict_mut()
        .set("Length", filtered.len() as f64);
    write_dict(stream.get_dict(), handle);
    pdf_out(handle, b"\nstream\n");
    if filtered.len() > 0 {
        pdf_out(
            handle, &filtered, //TODO: check
        );
    }
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
        let pdf_stream { dict, .. } = *self;
        unsafe {
            pdf_release_obj(dict);
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

pub(crate) unsafe fn pdf_stream_dataptr(stream: &pdf_stream) -> *const libc::c_void {
    stream.content.as_ptr() as *const libc::c_void
}

fn set_objstm_data(objstm: &mut pdf_stream, data: Vec<i32>) {
    objstm.objstm_data = data.into_boxed_slice();
}
fn get_objstm_data(objstm: &pdf_stream) -> &[i32] {
    &objstm.objstm_data
}
fn get_objstm_data_mut(objstm: &mut pdf_stream) -> &mut [i32] {
    &mut objstm.objstm_data
}

impl pdf_stream {
    pub(crate) unsafe fn add(&mut self, stream_data: *const libc::c_void, length: i32) {
        if length < 1 {
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
pub(crate) unsafe fn pdf_add_stream_flate(dst: &mut pdf_stream, data: &[u8]) -> i32 {
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
        ::std::mem::size_of::<libz::z_stream>() as u64 as i32,
    ) != 0
    {
        warn!("inflateInit() failed.");
        return -1;
    }
    loop {
        let status = libz::inflate(&mut z, 0);
        assert!(z.avail_out <= WBUF_SIZE as u32);
        if status == 1
        /* Z_STREAM_END */
        {
            break;
        }
        if status != 0 {
            warn!("inflate() failed. Broken PDF file?");
            libz::inflateEnd(&mut z);
            return -1;
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

    return if libz::inflateEnd(&mut z) == 0 { 0 } else { -1 };
}

#[cfg(feature = "libz-sys")]
unsafe fn get_decode_parms(dict: &mut pdf_dict) -> Option<decode_parms> {
    /* Fill with default values */
    let parms = decode_parms {
        predictor: match DerefObj::new(dict.get_mut("Predictor")) {
            Some(tmp) => tmp.as_f64() as i32,
            None => 1,
        },
        colors: match DerefObj::new(dict.get_mut("Colors")) {
            Some(tmp) => tmp.as_f64() as i32,
            None => 1,
        },
        bits_per_component: match DerefObj::new(dict.get_mut("BitsPerComponent")) {
            Some(tmp) => tmp.as_f64() as i32,
            None => 8,
        },
        columns: match DerefObj::new(dict.get_mut("Columns")) {
            Some(tmp) => tmp.as_f64() as i32,
            None => 1,
        },
    };

    if parms.bits_per_component != 1
        && parms.bits_per_component != 2
        && parms.bits_per_component != 4
        && parms.bits_per_component != 8
        && parms.bits_per_component != 16
    {
        warn!(
            "Invalid BPC value in DecodeParms: {}",
            parms.bits_per_component,
        );
        return None;
    } else {
        if parms.predictor <= 0 || parms.colors <= 0 || parms.columns <= 0 {
            return None;
        }
    }
    Some(parms)
}
/* From Xpdf version 3.04
 * I'm not sure if I properly ported... Untested.
 */
#[cfg(feature = "libz-sys")]
unsafe fn filter_row_TIFF2(dst: &mut [u8], src: *const u8, parms: &mut decode_parms) -> i32 {
    let p: *const u8 = src;
    /* bits_per_component < 8 here */
    let mask: i32 = (1 << parms.bits_per_component) - 1; /* 2 bytes buffer */
    let mut col = vec![0_u8; parms.colors as _];
    let mut outbuf = 0;
    let mut inbuf = outbuf;
    let mut outbits = 0;
    let mut inbits = outbits;
    let mut k = 0;
    let mut j = k;
    for _ in 0..parms.columns {
        /* expanding each color component into an 8-bits bytes array */
        for ci in 0..parms.colors as usize {
            if inbits < parms.bits_per_component {
                /* need more byte */
                inbuf = inbuf << 8 | *p.offset(j as isize) as i32;
                j += 1;
                inbits += 8;
            }
            /* predict current color component */
            col[ci] = (col[ci] as i32 + (inbuf >> inbits - parms.bits_per_component) & mask) as u8; /* consumed bpc bits */
            inbits -= parms.bits_per_component;
            /* append newly predicted color component value */
            outbuf = outbuf << parms.bits_per_component | col[ci] as i32;
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
    return 0;
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
) -> i32 {
    let bits_per_pixel: i32 = parms.colors * parms.bits_per_component;
    let bytes_per_pixel: i32 = (bits_per_pixel + 7) / 8;
    let length: i32 = (parms.columns * bits_per_pixel + 7) / 8;
    let len_usize = length as usize;
    let mut error = 0;
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
                if parms.bits_per_component == 8 {
                    let mut chunks = data.chunks_exact(len_usize);
                    while let Some(p) = chunks.next() {
                        for i in 0..len_usize {
                            let pixel_value: i32 = if i >= bytes_per_pixel {
                                buf[(i - bytes_per_pixel) as usize] as i32
                            } else {
                                0
                            };
                            buf[i] = ((p[i] as i32) + pixel_value & 0xff) as u8;
                        }
                        dst_stream.add_slice(&buf[..len_usize]);
                    }
                    assert!(chunks.remainder().is_empty());
                } else if parms.bits_per_component == 16 {
                    let mut chunks = data.chunks_exact(len_usize);
                    while let Some(p) = chunks.next() {
                        for i in (0..length).step_by(2) {
                            let b = (i - (bytes_per_pixel as i32)) as i32;
                            let hi = if b >= 0 { buf[b as usize] as i32 } else { 0 };
                            let lo = if b >= 0 {
                                buf[(b as usize) + 1] as i32
                            } else {
                                0
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
                error = -1;
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
                        error = -1;
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
                error = -1;
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
) -> i32 {
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
        ::std::mem::size_of::<libz::z_stream>() as u64 as i32,
    ) != 0
    {
        warn!("inflateInit() failed.");
        return -1;
    }
    let mut tmp_stream = pdf_stream::new(0);
    loop {
        let status = libz::inflate(&mut z, 0);
        if status == 1 {
            break;
        }
        if status != 0 {
            warn!("inflate() failed. Broken PDF file?");
            libz::inflateEnd(&mut z);
            return -1;
        }
        if z.avail_out == 0 {
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
    if error == 0 && libz::inflateEnd(&mut z) == 0 {
        0
    } else {
        -1
    }
}

pub(crate) unsafe fn pdf_concat_stream(dst: &mut pdf_stream, src: &mut pdf_stream) -> i32 {
    let mut error: i32 = 0;
    let stream_dict = (*(src as *mut pdf_stream)).get_dict_mut(); // TODO: fix hack
    let stream_data = &src.content;
    if stream_dict.get("Filter").is_some() {
        #[cfg(feature = "libz-sys")]
        {
            let mut parms = None;
            if stream_dict.has("DecodeParms") {
                /* Dictionary or array */
                let mut tmp =
                    if let Some(mut tmp) = DerefObj::new(stream_dict.get_mut("DecodeParms")) {
                        if let Object::Array(array) = &mut tmp.data {
                            if array.len() > 1 {
                                warn!("Unexpected size for DecodeParms array.");
                                return -1;
                            }

                            if !array.is_empty() {
                                DerefObj::new(Some(&mut *array[0]))
                            } else {
                                None
                            }
                        } else {
                            Some(tmp)
                        }
                    } else {
                        None
                    };
                match tmp.as_deref_mut() {
                    Some(pdf_obj {
                        data: Object::Dict(d),
                        ..
                    }) => {
                        parms = get_decode_parms(d)
                            .or_else(|| panic!("Invalid value(s) in DecodeParms dictionary."));
                    }
                    _ => {
                        warn!("PDF dict expected for DecodeParms...");
                        return -1;
                    }
                }
            }
            let mut filter = stream_dict.get("Filter").unwrap();
            if let Object::Array(filter_array) = &(*filter).data {
                if filter_array.len() > 1 {
                    warn!("Multiple DecodeFilter not supported.");
                    return -1;
                }
                filter = &**filter_array.get(0).expect("Broken PDF file?");
            }
            if let Object::Name(filter_name) = &(*filter).data {
                let filter_name = filter_name.to_bytes();
                error = if filter_name == b"FlateDecode" {
                    if let Some(parms) = parms.as_mut() {
                        pdf_add_stream_flate_filtered(dst, stream_data, parms)
                    } else {
                        pdf_add_stream_flate(dst, stream_data)
                    }
                } else {
                    warn!("DecodeFilter \"{}\" not supported.", filter_name.display());
                    -1
                };
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
unsafe fn pdf_stream_uncompress(src: &mut pdf_stream) -> Option<pdf_stream> {
    let mut dst = pdf_stream::new(0);
    dst.get_dict_mut().merge(src.get_dict());
    pdf_remove_dict(dst.get_dict_mut(), "Length");
    pdf_concat_stream(&mut dst, src);
    Some(dst)
}
unsafe fn pdf_write_obj(object: &mut Object, handle: &mut OutputHandleWrapper) {
    match object {
        Object::Boolean(v) => write_boolean(*v, handle),
        Object::Number(v) => write_number(*v, handle),
        Object::String(v) => write_string(v, handle),
        Object::Name(v) => write_name(v, handle),
        Object::Array(v) => write_array(v, handle),
        Object::Dict(v) => write_dict(v, handle),
        Object::Stream(v) => write_stream(v, handle),
        Object::Null => write_null(handle),
        Object::Indirect(v) => write_indirect(v, handle),
        Object::Invalid => panic!("Invalid object"),
        Object::Undefined => panic!("Undefined object"),
    }
}
/* Write the object to the file */
unsafe fn pdf_flush_obj(
    object: &mut Object,
    id: ObjectId,
    encrypt: bool,
    handle: &mut OutputHandleWrapper,
) {
    /*
     * Record file position
     */
    let (label, generation) = id;
    add_xref_entry(
        label as usize,
        1,
        (pdf_output_file_position as u32, generation),
    );
    let out = format!("{} {} obj\n", label, generation);
    enc_mode = encrypt;
    pdf_enc_set_label(label);
    pdf_enc_set_generation(generation as u32);
    pdf_out(handle, out.as_bytes());
    pdf_write_obj(object, handle);
    pdf_out(handle, b"\nendobj\n");
}
unsafe fn pdf_add_objstm(
    (objstm, id): &mut (pdf_stream, ObjectId),
    object: &mut Object,
    label: u32,
) -> usize {
    let len = objstm.len();
    let data = get_objstm_data_mut(objstm);
    data[0] += 1;
    let pos = data[0] as usize;
    data[2 * pos] = label as i32;
    data[2 * pos + 1] = len as i32;
    add_xref_entry(label as usize, 2_u8, (id.0, (pos - 1) as u16));
    /* redirect output into objstm */
    output_stream = objstm as *mut _;
    enc_mode = false;
    let handle = pdf_output_handle.as_mut().unwrap();
    pdf_write_obj(object, handle);
    pdf_out_char(handle, b'\n');
    output_stream = ptr::null_mut();
    pos
}
unsafe fn release_objstm((mut objstm, id): (pdf_stream, ObjectId)) {
    let pos: i32 = get_objstm_data(&objstm)[0];
    /* Precede stream data by offset table */
    /* Reserve 22 bytes for each entry (two 10 digit numbers plus two spaces) */
    let old_buf = std::mem::replace(&mut objstm.content, Vec::with_capacity(22 * pos as usize));
    let mut i = 2;
    for _ in 0..(2 * pos) {
        let slice = format!("{} ", get_objstm_data(&objstm)[i]);
        i += 1;
        objstm.add_slice(slice.as_bytes());
    }
    let len = objstm.content.len();
    let dict = objstm.get_dict_mut();
    dict.set("Type", "ObjStm");
    dict.set("N", pos as f64);
    dict.set("First", len as f64);
    objstm.add_slice(old_buf.as_ref());
    let mut objstm: Object = objstm.into();
    if id.0 != 0 {
        if let Some(handle) = pdf_output_handle.as_mut() {
            pdf_flush_obj(&mut objstm, id, doc_enc_mode, handle);
        }
    }
}

pub unsafe fn output_pdf_obj(object: &mut Object, id: ObjectId, flags: i32) {
    if let Some(handle) = pdf_output_handle.as_mut() {
        if !do_objstm
            || flags & OBJ_NO_OBJSTM != 0
            || doc_enc_mode && flags & OBJ_NO_ENCRYPT != 0
            || id.1 as i32 != 0
        {
            pdf_flush_obj(
                object,
                id,
                doc_enc_mode && flags & OBJ_NO_ENCRYPT == 0,
                handle,
            );
        } else {
            if current_objstm.is_none() {
                let data = vec![0; 2 * OBJSTM_MAX_OBJS + 2];
                let mut objstm = pdf_stream::new(STREAM_COMPRESS);
                set_objstm_data(&mut objstm, data);
                current_objstm = Some((objstm, pdf_next_label()));
            }
            if pdf_add_objstm(current_objstm.as_mut().unwrap(), object, id.0) == OBJSTM_MAX_OBJS {
                release_objstm(current_objstm.take().unwrap());
            }
        }
    }
}
pub unsafe fn pdf_release_obj(object: *mut pdf_obj) {
    if let Some(object) = object.as_mut() {
        if object.is_invalid() {
            panic!("Invalid object");
        }
        if object.refcount <= 0 {
            info!(
                "\npdf_release_obj: object={:p}, refcount={}\n",
                object, object.refcount,
            );
            pdf_write_obj(
                &mut object.data,
                ttstub_output_open_stdout().as_mut().unwrap(),
            );
            panic!("pdf_release_obj:  Called with invalid object.");
        }
        object.refcount -= 1;
        if object.refcount == 0 {
            /*
             * Nothing is using this object so it's okay to remove it.
             * Nonzero "label" means object needs to be written before it's destroyed.
             */
            if object.label() != 0 {
                let id = object.id;
                let flags = object.flags;
                output_pdf_obj(&mut object.data, id, flags);
            }
            /* This might help detect freeing already freed objects */
            object.data = Object::Invalid;
            let _ = Box::from_raw(object);
        }
    }
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
        c = handle.read_byte();
        if let Some(c) = c.filter(|&c| c != b'\n' && c != b'\r') {
            if buf.len() >= size {
                return Err(MfReadErr::NotEnoughSpace);
            }
            buf.push(c as u8);
        } else {
            break;
        }
    }
    if c.is_none() && buf.is_empty() {
        return Err(MfReadErr::Eof);
    }
    if c == Some(b'\r') {
        if handle.read_byte().filter(|&c| c != b'\n').is_some() {
            handle.seek(SeekFrom::Current(-1)).unwrap();
        }
    }
    Ok(buf)
}
unsafe fn backup_line<R: Read + Seek>(handle: &mut R) -> i32 {
    let mut ch = None;
    /* Note: this code should work even if \r\n is eol. It could fail on a
     * machine where \n is eol and there is a \r in the stream --- Highly
     * unlikely in the last few bytes where this is likely to be used.
     */
    match handle.seek(SeekFrom::Current(0)) {
        Ok(pos) if pos > 1 => loop {
            let pos = handle.seek(SeekFrom::Current(-2));
            match pos {
                Ok(pos)
                    if (pos > 0 && {
                        ch = handle.read_byte();
                        ch.filter(|&c| c != b'\n' && c != b'\r').is_some()
                    }) => {}
                _ => break,
            }
        },
        _ => {}
    }
    if ch.is_none() {
        0
    } else {
        1
    }
}
unsafe fn find_xref<R: Read + Seek>(handle: &mut R, file_size: i32) -> i32 {
    let mut tries: i32 = 10;
    loop {
        if backup_line(handle) == 0 {
            tries = 0;
            break;
        } else {
            let currentpos = handle.seek(SeekFrom::Current(0)).unwrap() as i32;
            let n = core::cmp::min(b"startxref".len() as i32, file_size - currentpos) as usize;
            let mut buf = vec![0; n];
            handle.read_exact(buf.as_mut_slice()).unwrap();
            handle.seek(SeekFrom::Start(currentpos as u64)).unwrap();
            tries -= 1;
            if !(tries > 0 && !buf.starts_with(b"startxref")) {
                break;
            }
        }
    }
    if tries <= 0 {
        return 0;
    }
    /* Skip rest of this line */
    tt_mfgets(work_buffer.as_mut_ptr(), 1024, handle);
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
unsafe fn parse_trailer(pf: *mut pdf_file) -> Option<pdf_dict> {
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
    let curr = (*pf).xref_table[obj_num as usize].id.0 as i32;
    /* Check all other type 1 objects to find next one */
    for item in &(*pf).xref_table {
        if item.typ as i32 == 1 && item.id.0 > curr as u32 && item.id.0 < next as u32 {
            next = item.id.0 as i32
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
) -> Option<*mut pdf_obj> {
    let length = (limit - offset) as usize;
    if length <= 0 {
        return None;
    }
    let mut buffer = vec![0u8; length + 1];
    (*pf).handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    (*pf).handle.read_exact(&mut buffer[..length]).unwrap();
    let p = buffer.as_slice();
    /* Check for obj_num and obj_gen */
    let mut q = p; /* <== p */
    q.skip_white();
    let sp = q.parse_unsigned();
    if sp.is_none() {
        return None;
    }
    let n = sp.unwrap().to_str().unwrap().parse::<u32>().unwrap();
    q.skip_white();
    let sp = q.parse_unsigned();
    if sp.is_none() {
        return None;
    }
    let g = sp.unwrap().to_str().unwrap().parse::<u32>().unwrap();
    if obj_num != 0 && (n != obj_num || g != obj_gen as u32) {
        return None;
    }
    let mut p = q;
    p.skip_white();
    if !p.starts_with(b"obj") {
        warn!("Didn\'t find \"obj\".");
        return None;
    }
    p = &p[b"obj".len()..];
    let result = p.parse_pdf_object(pf);
    p.skip_white();
    if !p.starts_with(b"endobj") {
        warn!("Didn\'t find \"endobj\".");
        if let Some(res) = result {
            pdf_release_obj(res);
        }
        None
    } else {
        result
    }
}
unsafe fn read_objstm(pf: *mut pdf_file, num: u32) -> *mut pdf_obj {
    let (offset, gen) = (*pf).xref_table[num as usize].id;
    let limit: i32 = next_object_offset(pf, num);
    if let Some(objstm) = pdf_read_object(num, gen, pf, offset as i32, limit) {
        if let Object::Stream(stream) = &mut (*objstm).data {
            if let Some(tmp) = pdf_stream_uncompress(stream) {
                pdf_release_obj(objstm);
                let objstm = tmp.into_obj();
                let dict = (*objstm).as_stream().get_dict();
                match &dict.get("Type").unwrap().data {
                    Object::Name(name) if name.to_bytes() == b"ObjStm" => {
                        if let Some(pdf_obj {
                            data: Object::Number(n),
                            ..
                        }) = dict.get("N")
                        {
                            let n = *n as i32;
                            if let Some(first_obj) = dict.get("First") {
                                if let Object::Number(first) = first_obj.data {
                                    let first = first as i32;
                                    /* reject object streams without object data */
                                    if !(first >= (*objstm).as_stream().len() as i32) {
                                        let header = vec![0; (2 * (n + 1)) as usize];
                                        set_objstm_data((*objstm).as_stream_mut(), header);
                                        let mut header =
                                            get_objstm_data_mut((*objstm).as_stream_mut());
                                        header[0] = n;
                                        header = &mut header[1..];
                                        header[0] = first;
                                        header = &mut header[1..];
                                        /* avoid parsing beyond offset table */
                                        let mut data = Vec::with_capacity(first as usize + 1);
                                        data.extend(
                                            &(*objstm).as_stream().content[..first as usize],
                                        );
                                        data.push(0);
                                        let mut p = data.as_ptr() as *const i8;
                                        let endptr = p.offset(first as isize);
                                        let mut i = 2 * n;
                                        let mut q: *mut i8 = ptr::null_mut();
                                        loop {
                                            if i == 0 {
                                                /* Any garbage after last entry? */
                                                skip_white(&mut p, endptr);
                                                if p == endptr {
                                                    (*pf).xref_table[num as usize].direct = objstm;
                                                    return objstm;
                                                }
                                                break;
                                            }
                                            header[0] = strtoul(p, &mut q, 10) as i32;
                                            header = &mut header[1..];
                                            if q == p as *mut i8 {
                                                break;
                                            }
                                            p = q;
                                            i -= 1;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        pdf_release_obj(objstm);
    }
    warn!("Cannot parse object stream.");
    ptr::null_mut()
}
/* Label without corresponding object definition are replaced by the
 * null object, as required by the PDF spec. This is important to parse
 * several cross-reference sections.
 */
unsafe fn pdf_get_object(pf: &mut pdf_file, obj_id: ObjectId) -> *mut pdf_obj {
    let (obj_num, obj_gen) = obj_id;
    if !(obj_num > 0
        && obj_num < pf.xref_table.len() as u32
        && (pf.xref_table[obj_num as usize].typ as i32 == 1
            && pf.xref_table[obj_num as usize].id.1 as i32 == obj_gen as i32
            || pf.xref_table[obj_num as usize].typ as i32 == 2 && obj_gen == 0))
    {
        warn!(
            "Trying to read nonexistent or deleted object: {} {}",
            obj_num, obj_gen,
        );
        return Object::Null.into_obj();
    }
    let result = pf.xref_table[obj_num as usize].direct;
    if !result.is_null() {
        return pdf_link_obj(result);
    }
    let mut result = None;
    if pf.xref_table[obj_num as usize].typ as i32 == 1 {
        /* type == 1 */
        let offset = pf.xref_table[obj_num as usize].id.0;
        let limit = next_object_offset(pf, obj_num);
        result = pdf_read_object(obj_num, obj_gen, pf, offset as i32, limit);
    } else {
        /* type == 2 */
        let (objstm_num, index) = pf.xref_table[obj_num as usize].id;
        let mut objstm: *mut pdf_obj = ptr::null_mut();
        if !(objstm_num >= pf.xref_table.len() as u32)
            && pf.xref_table[objstm_num as usize].typ as i32 == 1
            && {
                objstm = pf.xref_table[objstm_num as usize].direct;
                !objstm.is_null() || {
                    objstm = read_objstm(pf, objstm_num);
                    !objstm.is_null()
                }
            }
        {
            let mut data = get_objstm_data((*objstm).as_stream());
            let n = data[0];
            data = &data[1..];
            let first = data[0];
            data = &data[1..];
            if !(index as i32 >= n) && data[2 * index as usize] as u32 == obj_num {
                let objstm_slice = &(*objstm).as_stream().content;

                let pdfobj_start = first + data[2 * index as usize + 1];
                let pdfobj_end = if index as i32 == n - 1 {
                    objstm_slice.len()
                } else {
                    (first + data[2 * index as usize + 3]) as usize
                };

                let mut pdfobj_slice = &objstm_slice[pdfobj_start as usize..pdfobj_end];
                result = pdfobj_slice.parse_pdf_object(pf);
            }
        }
    }

    if let Some(result) = result {
        /* Make sure the caller doesn't free this object */
        pf.xref_table[obj_num as usize].direct = pdf_link_obj(result);
        result
    } else {
        warn!("Could not read object from object stream.");
        Object::Null.into_obj()
    }
}
pub(crate) unsafe fn pdf_new_ref(object: &mut pdf_obj) -> pdf_indirect {
    if object.label() == 0 {
        pdf_label_obj(object);
    }

    pdf_indirect {
        pf: ptr::null_mut(),
        id: object.id,
        obj: object,
    }
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
    if !obj.is_null() && matches!((*obj).data, Object::Null) {
        pdf_release_obj(obj);
        ptr::null_mut()
    } else {
        obj
    }
}

pub struct DerefObj(std::ptr::NonNull<pdf_obj>);
impl DerefObj {
    pub(crate) unsafe fn new(obj: Option<&mut pdf_obj>) -> Option<Self> {
        std::ptr::NonNull::new(pdf_deref_obj(obj)).map(DerefObj)
    }
}
impl Drop for DerefObj {
    fn drop(&mut self) {
        unsafe { pdf_release_obj(self.0.as_ptr()) }
    }
}
impl Clone for DerefObj {
    fn clone(&self) -> Self {
        unsafe {
            pdf_link_obj(self.0.as_ptr());
        }
        DerefObj(self.0)
    }
}

impl std::ops::Deref for DerefObj {
    type Target = pdf_obj;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl std::ops::DerefMut for DerefObj {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

unsafe fn extend_xref(pf: &mut pdf_file, new_size: usize) {
    pf.xref_table.resize_with(new_size as usize, || xref_entry {
        typ: 0,
        id: (0, 0),
        direct: ptr::null_mut(),
        indirect: ptr::null_mut(),
    });
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
        return 0;
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
            return -1;
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
                        return -1;
                    }
                    let q = q.unwrap();
                    let first = q.to_str().unwrap().parse::<i32>().unwrap() as u32;
                    p.skip_white();
                    /* Nnumber of objects in this xref subsection. */
                    let q = p.parse_unsigned();
                    if q.is_none() {
                        warn!("An unsigned integer expected but could not find. (xref)");
                        return -1;
                    }
                    let q = q.unwrap();
                    let size = q.to_str().unwrap().parse::<i32>().unwrap() as u32;
                    p.skip_white();
                    /* Check for unrecognized tokens */
                    if !p.is_empty() {
                        warn!("Unexpected token found in xref table.");
                        return -1;
                    }
                    /* The first line of a xref subsection OK. */
                    if ((*pf).xref_table.len() as u32) < first + size {
                        extend_xref(&mut *pf, (first + size) as usize);
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
                            return -1;
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
                                    return -1;
                                }
                                q_0
                            } else {
                                warn!("An unsigned integer expected but could not find. (xref)");
                                return -1;
                            };
                            /* FIXME: Possible overflow here. Consider using strtoll(). */
                            let offset = q_0.to_str().unwrap().parse::<i32>().unwrap() as u32;
                            p.skip_white();
                            /* Generation number -- 5 digits (0 padded) */
                            let q_0 = if let Some(q_0) = p.parse_unsigned() {
                                if q_0.to_bytes().len() != 5 {
                                    /* exactly 5 digits */
                                    warn!("Expecting a 5 digits number. (xref)");
                                    return -1;
                                }
                                q_0
                            } else {
                                warn!("An unsigned integer expected but could not find. (xref)");
                                return -1;
                            };
                            let obj_gen = q_0.to_str().unwrap().parse::<i32>().unwrap() as u32;
                            p.skip_white();
                            if p.is_empty() {
                                warn!(
                                    "Unexpected EOL reached while reading a xref subsection entry."
                                );
                                return -1;
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
                            if (*pf).xref_table[i as usize].id.0 == 0 {
                                (*pf).xref_table[i as usize].typ = (flag == b'n') as i32 as u8; /* TODO: change! why? */
                                (*pf).xref_table[i as usize].id = (offset, obj_gen as u16)
                            }
                            i += 1
                        }
                    }
                }
            }
        }
    }
    1
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
    first: usize,
    size: usize,
) -> i32 {
    *length -= wsum * (size as i32);
    if *length < 0 {
        return -1;
    }
    if pf.xref_table.len() < first + size {
        extend_xref(pf, first + size);
    }
    for e in &mut pf.xref_table[first..first + size] {
        let typ = parse_xrefstm_field(p, *W.offset(0), 1_u32) as u8;
        if typ as i32 > 2 {
            warn!("Unknown cross-reference stream entry type.");
        }
        let field2 = parse_xrefstm_field(p, *W.offset(1), 0_u32);
        let field3 = parse_xrefstm_field(p, *W.offset(2), 0_u32) as u16;
        if e.id.0 == 0 {
            e.typ = typ;
            e.id = (field2, field3)
        }
    }
    0
}
unsafe fn parse_xref_stream(pf: &mut pdf_file, xref_pos: i32, trailer: *mut *mut pdf_obj) -> i32 {
    let mut W: [i32; 3] = [0; 3];
    let mut wsum: i32 = 0;
    if let Some(xrefstm) = pdf_read_object(0_u32, 0_u16, pf, xref_pos, pf.file_size) {
        if let Object::Stream(stream) = &mut (*xrefstm).data {
            if let Some(tmp) = pdf_stream_uncompress(stream) {
                pdf_release_obj(xrefstm);
                let mut xrefstm = tmp;
                *trailer = pdf_link_obj(xrefstm.get_dict_obj());
                if let Some(size_obj) = (**trailer).as_dict().get("Size") {
                    if let Object::Number(size) = size_obj.data {
                        let mut length = xrefstm.len() as i32;
                        match &(**trailer).as_dict().get("W").unwrap().data {
                            Object::Array(W_obj) if W_obj.len() == 3 => {
                                let mut i = 0;
                                loop {
                                    if i >= 3 {
                                        let mut p = pdf_stream_dataptr(&xrefstm) as *const i8;
                                        if let Some(index_obj) = (**trailer).as_dict().get("Index")
                                        {
                                            match &index_obj.data {
                                                Object::Array(index) if index.len() % 2 == 0 => {
                                                    let index_len = index.len();
                                                    let mut i = 0;
                                                    loop {
                                                        if i >= index_len {
                                                            if length != 0 {
                                                                warn!("Garbage in xref stream.");
                                                            }
                                                            return 1;
                                                        }
                                                        let first = index.get(i);
                                                        i += 1;
                                                        let size_obj = index.get(i);
                                                        i += 1;
                                                        if let (Some(first), Some(size_obj)) =
                                                            (first, size_obj)
                                                        {
                                                            if let (
                                                                Object::Number(first),
                                                                Object::Number(size),
                                                            ) = (
                                                                &(**first).data,
                                                                &(**size_obj).data,
                                                            ) {
                                                                if parse_xrefstm_subsec(
                                                                    pf,
                                                                    &mut p,
                                                                    &mut length,
                                                                    W.as_mut_ptr(),
                                                                    wsum,
                                                                    *first as usize,
                                                                    *size as usize,
                                                                ) != 0
                                                                {
                                                                    break;
                                                                }
                                                            } else {
                                                                break;
                                                            }
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                }
                                                _ => {}
                                            }
                                        } else if parse_xrefstm_subsec(
                                            pf,
                                            &mut p,
                                            &mut length,
                                            W.as_mut_ptr(),
                                            wsum,
                                            0,
                                            size as usize,
                                        ) == 0
                                        {
                                            if length != 0 {
                                                warn!("Garbage in xref stream.");
                                            }
                                            return 1;
                                        }
                                        break;
                                    }
                                    if let Object::Number(tmp_0) = (*W_obj[i]).data {
                                        W[i] = tmp_0 as i32;
                                        wsum += W[i];
                                        i += 1
                                    } else {
                                        break;
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        pdf_release_obj(xrefstm);
    }
    warn!("Cannot parse cross-reference stream.");
    if !(*trailer).is_null() {
        pdf_release_obj(*trailer);
        *trailer = ptr::null_mut()
    }
    0
}
/* TODO: parse Version entry */
unsafe fn read_xref(pf: &mut pdf_file) -> *mut pdf_obj {
    let mut xref_pos = find_xref(&mut (*pf).handle, (*pf).file_size);
    if xref_pos == 0 {
        warn!("Error while parsing PDF file.");
        return ptr::null_mut();
    } else {
        let mut trailer: *mut pdf_obj = ptr::null_mut();
        let mut main_trailer: *mut pdf_obj = ptr::null_mut();
        loop {
            if xref_pos != 0 {
                let res: i32 = parse_xref_table(pf, xref_pos);
                if res > 0 {
                    /* cross-reference table */
                    trailer = parse_trailer(pf)
                        .map(IntoObj::into_obj)
                        .unwrap_or(ptr::null_mut());
                    if trailer.is_null() {
                        warn!("Error while parsing PDF file.");
                        pdf_release_obj(main_trailer);
                        return ptr::null_mut();
                    }
                    if main_trailer.is_null() {
                        main_trailer = pdf_link_obj(trailer)
                    }
                    if let Some(xrefstm) = (*trailer).as_dict().get("XRefStm") {
                        let mut new_trailer: *mut pdf_obj = ptr::null_mut();
                        match xrefstm.data {
                            Object::Number(xrefstm)
                                if parse_xref_stream(pf, xrefstm as i32, &mut new_trailer) != 0 =>
                            {
                                pdf_release_obj(new_trailer);
                            }
                            _ => warn!("Skipping hybrid reference section."),
                        }
                        /* Many PDF 1.5 xref streams use DecodeParms, which we cannot
                           parse. This way we can use at least xref tables in hybrid
                           documents. Or should we better stop parsing the file?
                        */
                    }
                } else {
                    if !(res == 0 && parse_xref_stream(pf, xref_pos, &mut trailer) != 0) {
                        warn!("Error while parsing PDF file.");
                        pdf_release_obj(trailer);
                        pdf_release_obj(main_trailer);
                        return ptr::null_mut();
                    }
                    /* cross-reference stream */
                    if main_trailer.is_null() {
                        main_trailer = pdf_link_obj(trailer)
                    }
                }
                if let Some(prev) = (*trailer).as_dict().get("Prev") {
                    if let Object::Number(prev) = prev.data {
                        xref_pos = prev as i32;
                    } else {
                        warn!("Error while parsing PDF file.");
                        pdf_release_obj(trailer);
                        pdf_release_obj(main_trailer);
                        return ptr::null_mut();
                    }
                } else {
                    xref_pos = 0
                }
                pdf_release_obj(trailer);
            } else {
                return main_trailer;
            }
        }
    }
}

use crate::dpx_dpxutil::HtTable;
use once_cell::sync::Lazy;
static mut pdf_files: Lazy<HtTable<pdf_file>> = Lazy::new(|| HtTable::new());

impl pdf_file {
    fn new(mut handle: InFile) -> Box<Self> {
        let file_size = ttstub_input_get_size(&mut handle) as i32;
        handle.seek(SeekFrom::End(0)).unwrap();
        Box::new(Self {
            handle,
            trailer: ptr::null_mut(),
            xref_table: Vec::new(),
            catalog: ptr::null_mut(),
            version: 0,
            file_size,
        })
    }
}
impl Drop for pdf_file {
    fn drop(&mut self) {
        unsafe {
            for item in &mut self.xref_table {
                pdf_release_obj(item.direct);
                pdf_release_obj(item.indirect);
            }
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

/*pub(crate) unsafe fn pdf_file_get_trailer(pf: &pdf_file) -> *mut pdf_obj {
    pdf_link_obj(pf.trailer)
}*/

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
        match pf.catalog.as_mut() {
            Some(pdf_obj {
                data: Object::Dict(catalog),
                ..
            }) => {
                if let Some(new_version) = DerefObj::new(catalog.get_mut("Version")) {
                    let minor = if let Object::Name(n) = &new_version.data {
                        let new_version_str = n.to_bytes();
                        let minor_num_str = if new_version_str.starts_with(b"1.") {
                            std::str::from_utf8(&new_version_str[2..]).unwrap_or("")
                        } else {
                            ""
                        };
                        if let Ok(minor_) = minor_num_str.parse::<u32>() {
                            minor_
                        } else {
                            warn!("Illegal Version entry in document catalog. Broken PDF file?");
                            return None;
                        }
                    } else {
                        0
                    };
                    if pf.version < minor {
                        pf.version = minor
                    }
                }
            }
            _ => {
                warn!("Cannot read PDF document catalog. Broken PDF file?");
                return None;
            }
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
        return -1;
    }
    pdata.set(key.to_bytes(), tmp); // TODO: check
    0
}
static mut loop_marker: pdf_obj = pdf_obj {
    id: (0, 0),
    refcount: 0_u32,
    flags: 0,
    data: Object::Invalid,
};
unsafe fn pdf_import_indirect(object: *mut pdf_obj) -> *mut pdf_obj {
    let pf = &mut *(*object).as_indirect().pf;
    let (obj_num, obj_gen) = (*object).as_indirect().id;
    if !(obj_num > 0
        && obj_num < pf.xref_table.len() as u32
        && (pf.xref_table[obj_num as usize].typ as i32 == 1
            && pf.xref_table[obj_num as usize].id.1 as i32 == obj_gen as i32
            || pf.xref_table[obj_num as usize].typ as i32 == 2 && obj_gen == 0))
    {
        warn!("Can\'t resolve object: {} {}", obj_num, obj_gen as i32);
        return Object::Null.into_obj();
    }
    let ref_0 = pf.xref_table[obj_num as usize].indirect;
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
        pf.xref_table[obj_num as usize].indirect = &mut loop_marker;
        let tmp = pdf_import_object(obj);
        let ref_0 = pdf_ref_obj(tmp);
        pf.xref_table[obj_num as usize].indirect = ref_0;
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
    match &mut (*object).data {
        Object::Indirect(v) => {
            if !v.pf.is_null() {
                pdf_import_indirect(object)
            } else {
                pdf_link_obj(object)
            }
        }
        Object::Stream(v) => {
            let tmp = pdf_import_object(v.get_dict_obj());
            if tmp.is_null() {
                return ptr::null_mut();
            }
            let mut imported = pdf_stream::new(0);
            let stream_dict = imported.get_dict_mut();
            stream_dict.merge((*tmp).as_dict());
            pdf_release_obj(tmp);
            imported.add_slice(&v.content);
            imported.into_obj()
        }
        Object::Dict(v) => {
            let mut imported = pdf_dict::new();
            if v.foreach(import_dict, &mut imported) < 0 {
                return ptr::null_mut();
            }
            imported.into_obj()
        }
        Object::Array(v) => {
            let mut imported = vec![];
            for i in 0..v.len() {
                let tmp = if i < v.len() {
                    pdf_import_object(v[i as usize])
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
    compression_saved = 0;
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
