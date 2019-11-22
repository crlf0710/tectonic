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

use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_cid::{
    CIDFont, CIDFont_attach_parent, CIDFont_cache_close, CIDFont_cache_find, CIDFont_cache_get,
    CIDFont_get_CIDSysInfo, CIDFont_get_embedding, CIDFont_get_flag, CIDFont_get_fontname,
    CIDFont_get_ident, CIDFont_get_opt_index, CIDFont_get_parent_id, CIDFont_get_resource,
    CIDFont_get_subtype, CIDFont_is_ACCFont, CIDFont_is_UCSFont,
};
use super::dpx_cmap::{CMap_cache_get, CMap_get_CIDSysInfo, CMap_get_wmode, CMap_is_Identity};
use super::dpx_mem::{new, renew};
use super::dpx_pdfencoding::pdf_load_ToUnicode_stream;
use super::dpx_pdfresource::{pdf_defineresource, pdf_findresource, pdf_get_resource_reference};
use super::dpx_tt_cmap::otf_create_ToUnicode_stream;
use crate::dpx_pdfobj::{
    pdf_copy_name, pdf_dict, pdf_get_version, pdf_link_obj, pdf_obj, pdf_ref_obj, pdf_release_obj,
    pdf_stream, IntoObj, STREAM_COMPRESS,
};
use crate::shims::sprintf;
use crate::streq_ptr;
use libc::{free, memset, strcpy, strlen};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Type0Font {
    pub fontname: *mut i8,
    pub encoding: *mut i8,
    pub used_chars: *mut i8,
    pub descendant: *mut CIDFont,
    pub flags: i32,
    pub wmode: i32,
    pub cmap_id: i32,
    pub indirect: *mut pdf_obj,
    pub fontdict: *mut pdf_obj,
    pub descriptor: *mut pdf_obj,
    /* _TYPE0_H_ */
}
use super::dpx_fontmap::fontmap_opt;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct font_cache {
    pub count: i32,
    pub capacity: i32,
    pub fonts: *mut Type0Font,
}

static mut __verbose: i32 = 0i32;

pub unsafe fn Type0Font_set_verbose(level: i32) {
    __verbose = level;
}
unsafe fn new_used_chars2() -> *mut i8 {
    let used_chars = new((8192usize).wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    memset(used_chars as *mut libc::c_void, 0i32, 8192);
    used_chars
}
/* MUST BE NULL */
unsafe fn Type0Font_init_font_struct(mut font: *mut Type0Font) {
    assert!(!font.is_null());
    (*font).fontname = ptr::null_mut();
    (*font).fontdict = ptr::null_mut();
    (*font).indirect = ptr::null_mut();
    (*font).descriptor = ptr::null_mut();
    (*font).encoding = ptr::null_mut();
    (*font).used_chars = ptr::null_mut();
    (*font).descendant = ptr::null_mut();
    (*font).wmode = -1i32;
    (*font).cmap_id = -1i32;
    (*font).flags = 0i32;
}
unsafe fn Type0Font_clean(mut font: *mut Type0Font) {
    if !font.is_null() {
        if !(*font).fontdict.is_null() {
            panic!("{}: Object not flushed.", "Type0",);
        }
        if !(*font).indirect.is_null() {
            panic!("{}: Object not flushed.", "Type0",);
        }
        if !(*font).descriptor.is_null() {
            panic!("{}: FontDescriptor unexpected for Type0 font.", "Type0",);
        }
        if (*font).flags & 1i32 << 0i32 == 0 && !(*font).used_chars.is_null() {
            free((*font).used_chars as *mut libc::c_void);
        }
        free((*font).encoding as *mut libc::c_void);
        free((*font).fontname as *mut libc::c_void);
        (*font).fontdict = ptr::null_mut();
        (*font).indirect = ptr::null_mut();
        (*font).descriptor = ptr::null_mut();
        (*font).used_chars = ptr::null_mut();
        (*font).encoding = ptr::null_mut();
        (*font).fontname = ptr::null_mut()
    };
}
/* PLEASE FIX THIS */
unsafe fn Type0Font_create_ToUnicode_stream(font: *mut Type0Font) -> *mut pdf_obj {
    let cidfont: *mut CIDFont = (*font).descendant;
    otf_create_ToUnicode_stream(
        CIDFont_get_ident(cidfont),
        CIDFont_get_opt_index(cidfont),
        Type0Font_get_usedchars(font),
        (*font).cmap_id,
    )
}
/* Try to load ToUnicode CMap from file system first, if not found fallback to
 * font CMap reverse lookup. */
unsafe fn Type0Font_try_load_ToUnicode_stream(
    font: *mut Type0Font,
    cmap_base: *mut i8,
) -> *mut pdf_obj {
    let cmap_name: *mut i8 = new((strlen(cmap_base)
        .wrapping_add(strlen(b"-UTF-16\x00" as *const u8 as *const i8))
        as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
        as *mut i8;
    sprintf(
        cmap_name,
        b"%s-UTF16\x00" as *const u8 as *const i8,
        cmap_base,
    );
    let mut tounicode = pdf_read_ToUnicode_file(cmap_name);
    if tounicode.is_null() {
        sprintf(
            cmap_name,
            b"%s-UCS2\x00" as *const u8 as *const i8,
            cmap_base,
        );
        tounicode = pdf_read_ToUnicode_file(cmap_name)
    }
    free(cmap_name as *mut libc::c_void);
    if tounicode.is_null() {
        tounicode = Type0Font_create_ToUnicode_stream(font)
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
        panic!("{}: No descendant CID-keyed font.", "Type0",);
    }
    if CIDFont_is_ACCFont(cidfont) {
        /* No need to embed ToUnicode */
        return;
    } else {
        if CIDFont_is_UCSFont(cidfont) {
            /*
             * Old version of dvipdfmx mistakenly used Adobe-Identity as Unicode.
             */
            let mut tounicode =
                pdf_read_ToUnicode_file(b"Adobe-Identity-UCS2\x00" as *const u8 as *const i8);
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
    let csi = CIDFont_get_CIDSysInfo(cidfont);
    let mut fontname = CIDFont_get_fontname(cidfont);
    if CIDFont_get_embedding(cidfont) != 0 {
        fontname = fontname.offset(7)
        /* FIXME */
    }
    if streq_ptr((*csi).registry, b"Adobe\x00" as *const u8 as *const i8) as i32 != 0
        && streq_ptr((*csi).ordering, b"Identity\x00" as *const u8 as *const i8) as i32 != 0
    {
        match CIDFont_get_subtype(cidfont) {
            2 => {
                /* PLEASE FIX THIS */
                tounicode = Type0Font_create_ToUnicode_stream(font)
            }
            _ => {
                if CIDFont_get_flag(cidfont, 1i32 << 9i32) != 0 {
                    /* FIXME */
                    tounicode = Type0Font_create_ToUnicode_stream(font)
                } else if CIDFont_get_flag(cidfont, 1i32 << 8i32) != 0 {
                    /* FIXME */
                    /* Font loader will create ToUnicode and set. */
                    return;
                } else {
                    tounicode = Type0Font_try_load_ToUnicode_stream(font, fontname)
                }
            }
        }
    } else {
        let cmap_base: *mut i8 = new((strlen((*csi).registry)
            .wrapping_add(strlen((*csi).ordering))
            .wrapping_add(2))
        .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        sprintf(
            cmap_base,
            b"%s-%s\x00" as *const u8 as *const i8,
            (*csi).registry,
            (*csi).ordering,
        );
        tounicode = Type0Font_try_load_ToUnicode_stream(font, cmap_base);
        free(cmap_base as *mut libc::c_void);
    }
    if !tounicode.is_null() {
        (*(*font).fontdict)
            .as_dict_mut()
            .set("ToUnicode", tounicode);
    } else {
        warn!(
            "Failed to load ToUnicode CMap for font \"{}\"",
            CStr::from_ptr(fontname).display(),
        );
    };
}

pub unsafe fn Type0Font_set_ToUnicode(font: *mut Type0Font, cmap_ref: *mut pdf_obj) {
    assert!(!font.is_null());
    (*(*font).fontdict).as_dict_mut().set("ToUnicode", cmap_ref);
}
unsafe fn Type0Font_dofont(font: *mut Type0Font) {
    if font.is_null() || (*font).indirect.is_null() {
        return;
    }
    if !(*(*font).fontdict).as_dict().has("ToUnicode") {
        /* FIXME */
        add_ToUnicode(font);
    };
}
unsafe fn Type0Font_flush(mut font: *mut Type0Font) {
    if !font.is_null() {
        pdf_release_obj((*font).fontdict);
        (*font).fontdict = ptr::null_mut();
        pdf_release_obj((*font).indirect);
        (*font).indirect = ptr::null_mut();
        if !(*font).descriptor.is_null() {
            panic!("{}: FontDescriptor unexpected for Type0 font.", "Type0",);
        }
        (*font).descriptor = ptr::null_mut()
    };
}

pub unsafe fn Type0Font_get_wmode(font: *mut Type0Font) -> i32 {
    assert!(!font.is_null());
    (*font).wmode
}

pub unsafe fn Type0Font_get_usedchars(font: *mut Type0Font) -> *mut i8 {
    assert!(!font.is_null());
    (*font).used_chars
}

pub unsafe fn Type0Font_get_resource(mut font: *mut Type0Font) -> *mut pdf_obj {
    assert!(!font.is_null());
    /*
     * This looks somewhat strange.
     */
    if (*font).indirect.is_null() {
        let mut array = vec![];
        array.push(CIDFont_get_resource((*font).descendant));
        (*(*font).fontdict)
            .as_dict_mut()
            .set("DescendantFonts", array.into_obj());
        (*font).indirect = pdf_ref_obj((*font).fontdict)
    }
    pdf_link_obj((*font).indirect)
}
static mut __cache: font_cache = font_cache {
    count: 0i32,
    capacity: 0i32,
    fonts: std::ptr::null_mut(),
};

pub unsafe fn Type0Font_cache_init() {
    if !__cache.fonts.is_null() {
        panic!("{}: Already initialized.", "Type0",);
    }
    __cache.count = 0i32;
    __cache.capacity = 0i32;
    __cache.fonts = ptr::null_mut();
}

pub unsafe fn Type0Font_cache_get(id: i32) -> *mut Type0Font {
    if id < 0i32 || id >= __cache.count {
        panic!("{}: Invalid ID {}", "Type0", id,);
    }
    &mut *__cache.fonts.offset(id as isize) as *mut Type0Font
}

pub unsafe fn Type0Font_cache_find(
    map_name: *const i8,
    cmap_id: i32,
    fmap_opt: *mut fontmap_opt,
) -> i32 {
    let pdf_ver = pdf_get_version() as i32;
    if map_name.is_null() || cmap_id < 0i32 || pdf_ver < 2i32 {
        return -1i32;
    }
    /*
     * Encoding is Identity-H or Identity-V according as thier WMode value.
     *
     * We do not use match against the map_name since fonts (TrueType) covers
     * characters across multiple character collection (eg, Adobe-Japan1 and
     * Adobe-Japan2) must be splited into multiple CID-keyed fonts.
     */
    let cmap = CMap_cache_get(cmap_id);
    let csi = if CMap_is_Identity(cmap) as i32 != 0 {
        ptr::null_mut()
    } else {
        CMap_get_CIDSysInfo(cmap)
    };
    let cid_id = CIDFont_cache_find(map_name, csi, fmap_opt);
    if cid_id < 0i32 {
        return -1i32;
    }
    /*
     * The descendant CID-keyed font has already been registerd.
     * If CID-keyed font with ID = cid_id is new font, then create new parent
     * Type 0 font. Otherwise, there already exists parent Type 0 font and
     * then we find him and return his ID. We must check against their WMode.
     */
    let cidfont = CIDFont_cache_get(cid_id);
    let wmode = CMap_get_wmode(cmap);
    /* Does CID-keyed font already have parent ? */
    let parent_id = CIDFont_get_parent_id(cidfont, wmode); /* If so, we don't need new one. */
    if parent_id >= 0i32 {
        return parent_id;
    }
    /*
     * CIDFont does not have parent or his parent's WMode does not matched with
     * wmode. Create new Type0 font.
     */
    if __cache.count >= __cache.capacity {
        __cache.capacity = (__cache.capacity as u32).wrapping_add(16u32) as i32 as i32;
        __cache.fonts = renew(
            __cache.fonts as *mut libc::c_void,
            (__cache.capacity as u32 as u64).wrapping_mul(::std::mem::size_of::<Type0Font>() as u64)
                as u32,
        ) as *mut Type0Font
    }
    let font_id = __cache.count;
    let font = &mut *__cache.fonts.offset(font_id as isize) as *mut Type0Font;
    Type0Font_init_font_struct(font);
    /*
     * All CJK double-byte characters are mapped so that resulting
     * character codes coincide with CIDs of given character collection.
     * So, the Encoding is always Identity-H for horizontal fonts or
     * Identity-V for vertical fonts.
     */
    if wmode != 0 {
        (*font).encoding = new(
            (strlen(b"Identity-V\x00" as *const u8 as *const i8).wrapping_add(1))
                .wrapping_mul(::std::mem::size_of::<i8>()) as _,
        ) as *mut i8;
        strcpy(
            (*font).encoding,
            b"Identity-V\x00" as *const u8 as *const i8,
        );
    } else {
        (*font).encoding = new(
            (strlen(b"Identity-H\x00" as *const u8 as *const i8).wrapping_add(1))
                .wrapping_mul(::std::mem::size_of::<i8>()) as _,
        ) as *mut i8;
        strcpy(
            (*font).encoding,
            b"Identity-H\x00" as *const u8 as *const i8,
        );
    }
    (*font).wmode = wmode;
    (*font).cmap_id = cmap_id;
    /*
     * Now we start font dictionary.
     */
    (*font).fontdict = pdf_dict::new().into_obj();
    (*(*font).fontdict).as_dict_mut().set("Type", "Font");
    (*(*font).fontdict).as_dict_mut().set("Subtype", "Type0");
    /*
     * Type0 font does not have FontDescriptor because it is not a simple font.
     * Instead, DescendantFonts appears here.
     *
     * Up to PDF version 1.5, Type0 font must have single descendant font which
     * is a CID-keyed font. Future PDF spec. will allow multiple desecendant
     * fonts.
     */
    (*font).descendant = cidfont;
    CIDFont_attach_parent(cidfont, font_id, wmode);
    /*
     * PostScript Font name:
     *
     *  Type0 font's fontname is usually descendant CID-keyed font's font name
     *  appended by -ENCODING.
     */
    let fontname = CIDFont_get_fontname(cidfont); /* skip XXXXXX+ */
    if __verbose != 0 {
        if CIDFont_get_embedding(cidfont) != 0 && strlen(fontname) > 7 {
            info!("(CID:{})", CStr::from_ptr(fontname.offset(7)).display());
        } else {
            info!("(CID:{})", CStr::from_ptr(fontname).display());
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
    (*font).used_chars = ptr::null_mut();
    (*font).flags = 0i32;
    match CIDFont_get_subtype(cidfont) {
        1 => {
            (*font).fontname = new((strlen(fontname)
                .wrapping_add(strlen((*font).encoding))
                .wrapping_add(2))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
            sprintf(
                (*font).fontname,
                b"%s-%s\x00" as *const u8 as *const i8,
                fontname,
                (*font).encoding,
            );
            (*(*font).fontdict)
                .as_dict_mut()
                .set("BaseFont", pdf_copy_name((*font).fontname));
            /*
             * Need used_chars to write W, W2.
             */
            let parent_id = CIDFont_get_parent_id(cidfont, if wmode != 0 { 0i32 } else { 1i32 });
            if parent_id < 0i32 {
                (*font).used_chars = new_used_chars2()
            } else {
                /* Don't allocate new one. */
                (*font).used_chars = Type0Font_get_usedchars(Type0Font_cache_get(parent_id));
                (*font).flags |= 1i32 << 0i32
            }
        }
        2 => {
            /*
             * TrueType:
             *
             *  Use different used_chars for H and V.
             */
            (*(*font).fontdict)
                .as_dict_mut()
                .set("BaseFont", pdf_copy_name(fontname));
            (*font).used_chars = new_used_chars2()
        }
        _ => {
            panic!("Unrecognized CIDFont Type");
        }
    }
    (*(*font).fontdict)
        .as_dict_mut()
        .set("Encoding", pdf_copy_name((*font).encoding));
    __cache.count += 1;
    font_id
}
/* ******************************* CACHE ********************************/

pub unsafe fn Type0Font_cache_close() {
    /*
     * This need to be fixed.
     *
     * CIDFont_cache_close() before Type0Font_release because of used_chars.
     * ToUnicode support want descendant CIDFont's CSI and fontname.
     */
    if !__cache.fonts.is_null() {
        for font_id in 0..__cache.count {
            Type0Font_dofont(&mut *__cache.fonts.offset(font_id as isize));
        }
    }
    CIDFont_cache_close();
    if !__cache.fonts.is_null() {
        for font_id in 0..__cache.count {
            Type0Font_flush(&mut *__cache.fonts.offset(font_id as isize));
            Type0Font_clean(&mut *__cache.fonts.offset(font_id as isize));
        }
        free(__cache.fonts as *mut libc::c_void);
    }
    __cache.fonts = ptr::null_mut();
    __cache.count = 0i32;
    __cache.capacity = 0i32;
}
/* ******************************* COMPAT ********************************/
unsafe fn create_dummy_CMap() -> *mut pdf_obj {
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
    stream.into_obj()
}
unsafe fn pdf_read_ToUnicode_file(cmap_name: *const i8) -> *mut pdf_obj {
    assert!(!cmap_name.is_null());
    let mut res_id = pdf_findresource(b"CMap\x00" as *const u8 as *const i8, cmap_name);
    if res_id < 0i32 {
        let stream = if streq_ptr(
            cmap_name,
            b"Adobe-Identity-UCS2\x00" as *const u8 as *const i8,
        ) {
            create_dummy_CMap()
        } else {
            pdf_load_ToUnicode_stream(cmap_name)
        };
        if !stream.is_null() {
            res_id = pdf_defineresource(
                b"CMap\x00" as *const u8 as *const i8,
                cmap_name,
                stream,
                1i32,
            )
        }
    }
    if res_id < 0i32 {
        ptr::null_mut()
    } else {
        pdf_get_resource_reference(res_id)
    }
}
/* !WITHOUT_COMPAT */
