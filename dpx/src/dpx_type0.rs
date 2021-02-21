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

use std::ptr;

use super::dpx_cid::{
    CIDFont, CIDFont_attach_parent, CIDFont_cache_close, CIDFont_cache_find, CIDFont_cache_get,
    CIDFont_get_CIDSysInfo, CIDFont_get_embedding, CIDFont_get_flag, CIDFont_get_opt_index,
    CIDFont_get_parent_id, CIDFont_get_resource, CIDFont_get_subtype, CIDFont_is_ACCFont,
    CIDFont_is_UCSFont,
};
use super::dpx_cmap::{CMap_cache_get, CMap_get_CIDSysInfo, CMap_get_wmode, CMap_is_Identity};
use super::dpx_mem::new;
use super::dpx_pdfencoding::pdf_load_ToUnicode_stream;
use super::dpx_pdfresource::{pdf_defineresource, pdf_findresource, pdf_get_resource_reference};
use super::dpx_tt_cmap::otf_create_ToUnicode_stream;
use crate::dpx_pdfobj::{
    pdf_dict, pdf_get_version, pdf_link_obj, pdf_name, pdf_obj, pdf_ref_obj, pdf_release_obj,
    pdf_stream, IntoObj, STREAM_COMPRESS,
};
use crate::shims::sprintf;
use libc::{free, memset};

#[derive(Clone)]
pub(crate) struct Type0Font {
    pub(crate) fontname: String,
    pub(crate) encoding: &'static str,
    pub(crate) used_chars: *mut i8,
    pub(crate) descendant: *mut CIDFont,
    pub(crate) flags: i32,
    pub(crate) wmode: i32,
    pub(crate) cmap_id: i32,
    pub(crate) indirect: *mut pdf_obj,
    pub(crate) fontdict: *mut pdf_obj,
    pub(crate) descriptor: *mut pdf_obj,
    /* _TYPE0_H_ */
}
use super::dpx_fontmap::fontmap_opt;

static mut __verbose: i32 = 0;

pub(crate) unsafe fn Type0Font_set_verbose(level: i32) {
    __verbose = level;
}
unsafe fn new_used_chars2() -> *mut i8 {
    let used_chars = new((8192usize).wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    memset(used_chars as *mut libc::c_void, 0, 8192);
    used_chars
}

impl Type0Font {
    pub(crate) const fn new() -> Self {
        Self {
            fontname: String::new(),
            fontdict: ptr::null_mut(),
            indirect: ptr::null_mut(),
            descriptor: ptr::null_mut(),
            encoding: "",
            used_chars: ptr::null_mut(),
            descendant: ptr::null_mut(),
            wmode: -1,
            cmap_id: -1,
            flags: 0,
        }
    }
}

unsafe fn Type0Font_clean(font: &mut Type0Font) {
    if !font.fontdict.is_null() {
        panic!("{}: Object not flushed.", "Type0");
    }
    if !font.indirect.is_null() {
        panic!("{}: Object not flushed.", "Type0");
    }
    if !font.descriptor.is_null() {
        panic!("{}: FontDescriptor unexpected for Type0 font.", "Type0");
    }
    if font.flags & 1 << 0 == 0 && !font.used_chars.is_null() {
        free(font.used_chars as *mut libc::c_void);
    }
    font.fontdict = ptr::null_mut();
    font.indirect = ptr::null_mut();
    font.descriptor = ptr::null_mut();
    font.used_chars = ptr::null_mut();
    font.fontname.clear();
}
/* PLEASE FIX THIS */
unsafe fn Type0Font_create_ToUnicode_stream(font: &Type0Font) -> *mut pdf_obj {
    let cidfont: *mut CIDFont = font.descendant;
    let used_chars = std::slice::from_raw_parts(Type0Font_get_usedchars(font) as *const u8, 8192);
    otf_create_ToUnicode_stream(
        &*(&*cidfont).ident,
        CIDFont_get_opt_index(&*cidfont),
        used_chars,
        font.cmap_id,
    )
}
/* Try to load ToUnicode CMap from file system first, if not found fallback to
 * font CMap reverse lookup. */
unsafe fn Type0Font_try_load_ToUnicode_stream(
    font: *mut Type0Font,
    cmap_base: &str,
) -> *mut pdf_obj {
    let mut cmap_name = format!("{}-UTF16", cmap_base);
    let mut tounicode = pdf_read_ToUnicode_file(&cmap_name);
    if tounicode.is_null() {
        cmap_name = format!("{}-UCS2", cmap_base);
        tounicode = pdf_read_ToUnicode_file(&cmap_name)
    }
    if tounicode.is_null() {
        tounicode = Type0Font_create_ToUnicode_stream(&*font)
    }
    tounicode
}
unsafe fn add_ToUnicode(font: *mut Type0Font) {
    /*
     * ToUnicode CMap:
     *
     *  ToUnicode CMaps are usually not required for standard character
     *  collections such as Adobe-Japan1. Identity-H is used for UCS
     *  ordering CID-keyed fonts. External resource must be loaded for
     *  others.
     */
    let cidfont = (*font).descendant;
    if cidfont.is_null() {
        panic!("{}: No descendant CID-keyed font.", "Type0");
    }
    if CIDFont_is_ACCFont(&*cidfont) {
        /* No need to embed ToUnicode */
        return;
    } else {
        if CIDFont_is_UCSFont(&*cidfont) {
            /*
             * Old version of dvipdfmx mistakenly used Adobe-Identity as Unicode.
             */
            let mut tounicode = pdf_read_ToUnicode_file("Adobe-Identity-UCS2");
            if tounicode.is_null() {
                /* This should work */
                tounicode = "Identity-H".into_obj();
            }
            (*(*font).fontdict)
                .as_dict_mut()
                .set("ToUnicode", tounicode);
            return;
        }
    }
    let tounicode;
    let csi = CIDFont_get_CIDSysInfo(&*cidfont);
    let mut fontname = &*(&*cidfont).fontname;
    if CIDFont_get_embedding(&*cidfont) != 0 {
        fontname = &fontname[7..]
        /* FIXME */
    }
    if csi.registry == "Adobe" && csi.ordering == "Identity" {
        match CIDFont_get_subtype(&*cidfont) {
            2 => {
                /* PLEASE FIX THIS */
                tounicode = Type0Font_create_ToUnicode_stream(&*font)
            }
            _ => {
                if CIDFont_get_flag(&*cidfont, 1 << 9) != 0 {
                    /* FIXME */
                    tounicode = Type0Font_create_ToUnicode_stream(&*font)
                } else if CIDFont_get_flag(&*cidfont, 1 << 8) != 0 {
                    /* FIXME */
                    /* Font loader will create ToUnicode and set. */
                    return;
                } else {
                    tounicode = Type0Font_try_load_ToUnicode_stream(font, fontname)
                }
            }
        }
    } else {
        let cmap_base = format!("{}-{}", csi.registry, csi.ordering);
        tounicode = Type0Font_try_load_ToUnicode_stream(font, &cmap_base);
    }
    if !tounicode.is_null() {
        (*(*font).fontdict)
            .as_dict_mut()
            .set("ToUnicode", tounicode);
    } else {
        warn!("Failed to load ToUnicode CMap for font \"{}\"", fontname);
    };
}

pub(crate) unsafe fn Type0Font_set_ToUnicode(font: &mut Type0Font, cmap_ref: *mut pdf_obj) {
    (*font.fontdict).as_dict_mut().set("ToUnicode", cmap_ref);
}
unsafe fn Type0Font_dofont(font: &mut Type0Font) {
    if font.indirect.is_null() {
        return;
    }
    if !(*font.fontdict).as_dict().has("ToUnicode") {
        /* FIXME */
        add_ToUnicode(font);
    };
}
unsafe fn Type0Font_flush(font: &mut Type0Font) {
    pdf_release_obj(font.fontdict);
    font.fontdict = ptr::null_mut();
    pdf_release_obj(font.indirect);
    font.indirect = ptr::null_mut();
    if !font.descriptor.is_null() {
        panic!("{}: FontDescriptor unexpected for Type0 font.", "Type0");
    }
    font.descriptor = ptr::null_mut();
}

pub(crate) unsafe fn Type0Font_get_wmode(font: &Type0Font) -> i32 {
    font.wmode
}

pub(crate) unsafe fn Type0Font_get_usedchars(font: &Type0Font) -> *mut i8 {
    font.used_chars
}

pub(crate) unsafe fn Type0Font_get_resource(font: &mut Type0Font) -> *mut pdf_obj {
    /*
     * This looks somewhat strange.
     */
    if font.indirect.is_null() {
        let mut array = vec![];
        array.push(CIDFont_get_resource(&mut *font.descendant));
        (*font.fontdict)
            .as_dict_mut()
            .set("DescendantFonts", array.into_obj());
        font.indirect = pdf_ref_obj(font.fontdict)
    }
    pdf_link_obj(font.indirect)
}

// Note: The elements are boxed to be able
// to get stable pointers to the cached data.
// (Type0Font_cache_get returns *mut Type0Font)
static mut __cache: Vec<Box<Type0Font>> = Vec::new();

pub(crate) unsafe fn Type0Font_cache_init() {
    __cache.clear();
}

pub(crate) unsafe fn Type0Font_cache_get(id: i32) -> *mut Type0Font {
    if id < 0 || id >= __cache.len() as i32 {
        panic!("{}: Invalid ID {}", "Type0", id);
    }
    &mut *__cache[id as usize] as *mut Type0Font
}

pub(crate) unsafe fn Type0Font_cache_find(
    map_name: &str,
    cmap_id: i32,
    fmap_opt: &mut fontmap_opt,
) -> i32 {
    let pdf_ver = pdf_get_version() as i32;
    if map_name.is_empty() || cmap_id < 0 || pdf_ver < 2 {
        return -1;
    }
    /*
     * Encoding is Identity-H or Identity-V according as thier WMode value.
     *
     * We do not use match against the map_name since fonts (TrueType) covers
     * characters across multiple character collection (eg, Adobe-Japan1 and
     * Adobe-Japan2) must be splited into multiple CID-keyed fonts.
     */
    let cmap = CMap_cache_get(cmap_id);
    let csi = if CMap_is_Identity(&*cmap) as i32 != 0 {
        ptr::null_mut()
    } else {
        CMap_get_CIDSysInfo(cmap)
    };
    let cid_id = CIDFont_cache_find(map_name, csi, fmap_opt);
    if cid_id < 0 {
        return -1;
    }
    /*
     * The descendant CID-keyed font has already been registerd.
     * If CID-keyed font with ID = cid_id is new font, then create new parent
     * Type 0 font. Otherwise, there already exists parent Type 0 font and
     * then we find him and return his ID. We must check against their WMode.
     */
    let wmode = CMap_get_wmode(&*cmap);
    /* Does CID-keyed font already have parent ? */
    let parent_id = CIDFont_get_parent_id(CIDFont_cache_get(cid_id), wmode); /* If so, we don't need new one. */
    if parent_id >= 0 {
        return parent_id;
    }
    /*
     * CIDFont does not have parent or his parent's WMode does not matched with
     * wmode. Create new Type0 font.
     */

    let font_id = __cache.len() as i32;
    let mut font = Type0Font::new();
    /*
     * All CJK double-byte characters are mapped so that resulting
     * character codes coincide with CIDs of given character collection.
     * So, the Encoding is always Identity-H for horizontal fonts or
     * Identity-V for vertical fonts.
     */
    if wmode != 0 {
        font.encoding = "Identity-V";
    } else {
        font.encoding = "Identity-H";
    }
    font.wmode = wmode;
    font.cmap_id = cmap_id;
    /*
     * Now we start font dictionary.
     */
    font.fontdict = pdf_dict::new().into_obj();
    (*font.fontdict).as_dict_mut().set("Type", "Font");
    (*font.fontdict).as_dict_mut().set("Subtype", "Type0");
    /*
     * Type0 font does not have FontDescriptor because it is not a simple font.
     * Instead, DescendantFonts appears here.
     *
     * Up to PDF version 1.5, Type0 font must have single descendant font which
     * is a CID-keyed font. Future PDF spec. will allow multiple desecendant
     * fonts.
     */
    let cidfont = CIDFont_cache_get(cid_id);
    font.descendant = cidfont;
    CIDFont_attach_parent(cidfont, font_id, wmode);
    /*
     * PostScript Font name:
     *
     *  Type0 font's fontname is usually descendant CID-keyed font's font name
     *  appended by -ENCODING.
     */
    let fontname = &*(&*cidfont).fontname; /* skip XXXXXX+ */
    if __verbose != 0 {
        if CIDFont_get_embedding(cidfont) != 0 && fontname.len() > 7 {
            info!("(CID:{})", &fontname[7..]);
        } else {
            info!("(CID:{})", fontname);
        }
    }
    /*
     * The difference between CID-keyed font and TrueType font appears here.
     *
     * Glyph substitution for vertical writing is done in CMap mapping process
     * for CID-keyed fonts. But we must rely on OpenType layout table in the
     * case of TrueType fonts. So, we must use different used_chars for each
     * horizontal and vertical fonts in that case.
     *
     * In most PDF file, encoding name is not appended to fontname for Type0
     * fonts having CIDFontType 2 font as their descendant.
     */
    font.used_chars = ptr::null_mut();
    font.flags = 0;
    match CIDFont_get_subtype(cidfont) {
        1 => {
            font.fontname = format!("{}-{}", fontname, font.encoding);
            (*font.fontdict)
                .as_dict_mut()
                .set("BaseFont", pdf_name::new(font.fontname.as_bytes()));
            /*
             * Need used_chars to write W, W2.
             */
            let parent_id = CIDFont_get_parent_id(cidfont, if wmode != 0 { 0 } else { 1 });
            if parent_id < 0 {
                font.used_chars = new_used_chars2()
            } else {
                /* Don't allocate new one. */
                font.used_chars = Type0Font_get_usedchars(&*Type0Font_cache_get(parent_id));
                font.flags |= 1 << 0
            }
        }
        2 => {
            /*
             * TrueType:
             *
             *  Use different used_chars for H and V.
             */
            (*font.fontdict)
                .as_dict_mut()
                .set("BaseFont", pdf_name::new(fontname.as_bytes()));
            font.used_chars = new_used_chars2()
        }
        _ => {
            panic!("Unrecognized CIDFont Type");
        }
    }
    (*font.fontdict).as_dict_mut().set(
        "Encoding",
        pdf_name::new(font.encoding.as_bytes()).into_obj(),
    );
    __cache.push(Box::new(font));
    font_id
}
/* ******************************* CACHE ********************************/

pub(crate) unsafe fn Type0Font_cache_close() {
    /*
     * This need to be fixed.
     *
     * CIDFont_cache_close() before Type0Font_release because of used_chars.
     * ToUnicode support want descendant CIDFont's CSI and fontname.
     */
    for font in &mut __cache {
        Type0Font_dofont(&mut **font);
    }
    CIDFont_cache_close();
    for font in &mut __cache {
        Type0Font_flush(&mut **font);
        Type0Font_clean(&mut **font);
    }
    __cache.clear();
}
/* ******************************* COMPAT ********************************/
unsafe fn create_dummy_CMap() -> pdf_stream {
    let mut buf: [u8; 32] = [0; 32];
    let mut stream = pdf_stream::new(STREAM_COMPRESS);
    stream.add_str("%!PS-Adobe-3.0 Resource-CMap\n%%DocumentNeededResources: ProcSet (CIDInit)\n%%IncludeResource: ProcSet (CIDInit)\n%%BeginResource: CMap (Adobe-Identity-UCS2)\n%%Title: (Adobe-Identity-UCS2 Adobe UCS2 0)\n%%Version: 1.0\n%%Copyright:\n%% ---\n%%EndComments\n\n");
    stream.add_str("/CIDInit /ProcSet findresource begin\n\n12 dict begin\n\nbegincmap\n\n/CIDSystemInfo 3 dict dup begin\n  /Registry (Adobe) def\n  /Ordering (UCS2) def\n  /Supplement 0 def\nend def\n\n/CMapName /Adobe-Identity-UCS2 def\n/CMapVersion 1.0 def\n/CMapType 2 def\n\n2 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n");
    stream.add_str("\n100 beginbfrange\n");
    for i in 0..0x64 {
        let n = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b"<%02X00> <%02XFF> <%02X00>\n\x00" as *const u8 as *const i8,
            i,
            i,
            i,
        ) as usize;
        stream.add_slice(&buf[..n]);
    }
    stream.add_str("endbfrange\n\n");
    stream.add_str("\n100 beginbfrange\n");
    for i in 0x64..0xc8 {
        let n = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b"<%02X00> <%02XFF> <%02X00>\n\x00" as *const u8 as *const i8,
            i,
            i,
            i,
        ) as usize;
        stream.add_slice(&buf[..n]);
    }
    stream.add_str("endbfrange\n\n");
    stream.add_str("\n48 beginbfrange\n");
    for i in 0xc8..=0xd7 {
        let n = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b"<%02X00> <%02XFF> <%02X00>\n\x00" as *const u8 as *const i8,
            i,
            i,
            i,
        ) as usize;
        stream.add_slice(&buf[..n]);
    }
    for i in 0xe0..=0xff {
        let n = sprintf(
            buf.as_mut_ptr() as *mut i8,
            b"<%02X00> <%02XFF> <%02X00>\n\x00" as *const u8 as *const i8,
            i,
            i,
            i,
        ) as usize;
        stream.add_slice(&buf[..n]);
    }
    stream.add_str("endbfrange\n\n");
    stream.add_str("endcmap\n\nCMapName currentdict /CMap defineresource pop\n\nend\nend\n\n%%EndResource\n%%EOF\n");
    stream
}
unsafe fn pdf_read_ToUnicode_file(cmap_name: &str) -> *mut pdf_obj {
    assert!(!cmap_name.is_empty());
    let mut res_id = pdf_findresource("CMap", cmap_name);
    if res_id < 0 {
        let stream = if cmap_name == "Adobe-Identity-UCS2" {
            Some(create_dummy_CMap())
        } else {
            pdf_load_ToUnicode_stream(cmap_name)
        };
        if let Some(stream) = stream {
            res_id = pdf_defineresource("CMap", cmap_name, stream.into_obj(), 1)
        }
    }
    if res_id < 0 {
        ptr::null_mut()
    } else {
        pdf_get_resource_reference(res_id)
    }
}
/* !WITHOUT_COMPAT */
