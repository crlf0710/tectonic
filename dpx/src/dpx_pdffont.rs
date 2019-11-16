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

use crate::DisplayExt;
use std::ffi::CStr;
use std::ptr;

use super::dpx_agl::{agl_close_map, agl_init_map, agl_set_verbose};
use super::dpx_cid::CIDFont_set_verbose;
use super::dpx_cidtype0::t1_load_UnicodeCMap;
use super::dpx_cmap::{
    CMap_cache_close, CMap_cache_find, CMap_cache_get, CMap_cache_init, CMap_get_name,
    CMap_get_profile, CMap_get_type, CMap_set_verbose,
};
use super::dpx_fontmap::pdf_lookup_fontmap_record;
use super::dpx_mem::{new, renew};
use super::dpx_pdfencoding::{
    pdf_close_encodings, pdf_encoding_add_usedchars, pdf_encoding_complete,
    pdf_encoding_findresource, pdf_encoding_get_name, pdf_encoding_get_tounicode,
    pdf_encoding_set_verbose, pdf_get_encoding_obj, pdf_init_encodings, pdf_load_ToUnicode_stream,
};
use super::dpx_pkfont::{pdf_font_load_pkfont, pdf_font_open_pkfont, PKFont_set_dpi};
use super::dpx_truetype::{pdf_font_load_truetype, pdf_font_open_truetype};
use super::dpx_tt_cmap::{otf_cmap_set_verbose, otf_load_Unicode_CMap};
use super::dpx_type0::{
    Type0Font_cache_close, Type0Font_cache_find, Type0Font_cache_get,
    Type0Font_cache_init, Type0Font_get_resource, Type0Font_get_usedchars, Type0Font_get_wmode,
    Type0Font_set_verbose,
};
use super::dpx_type1::{pdf_font_load_type1, pdf_font_open_type1};
use super::dpx_type1c::{pdf_font_load_type1c, pdf_font_open_type1c};
use crate::dpx_pdfobj::{
    pdf_add_dict, pdf_copy_name, pdf_link_obj, pdf_new_dict, pdf_new_name,
    pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_stream_length,
};
use crate::mfree;
use crate::shims::{snprintf, sprintf};
use crate::streq_ptr;
use crate::{info, warn};
use libc::{free, memset, rand, srand, strcpy, strlen, strstr};

/* Options */
use super::dpx_fontmap::fontmap_rec;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct pdf_font {
    pub ident: *mut i8,
    pub subtype: i32,
    pub map_name: *mut i8,
    pub encoding_id: i32,
    pub font_id: i32,
    pub index: i32,
    pub fontname: *mut i8,
    pub uniqueID: [i8; 7],
    pub reference: *mut pdf_obj,
    pub resource: *mut pdf_obj,
    pub descriptor: *mut pdf_obj,
    pub usedchars: *mut i8,
    pub flags: i32,
    pub point_size: f64,
    pub design_size: f64,
    /* _PDFFONT_H_ */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub count: i32,
    pub capacity: i32,
    pub fonts: *mut pdf_font,
}

/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
static mut __verbose: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn pdf_font_set_verbose(mut level: i32) {
    __verbose = level;
    CMap_set_verbose(level);
    Type0Font_set_verbose(level);
    CIDFont_set_verbose(level);
    pdf_encoding_set_verbose(level);
    agl_set_verbose(level);
    otf_cmap_set_verbose(level);
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_verbose() -> i32 {
    __verbose
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_set_dpi(mut font_dpi: i32) {
    PKFont_set_dpi(font_dpi);
}
/* Here is the complete list of PDF object types */
/* A deeper object hierarchy will be considered as (illegal) loop. */
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

use std::time::SystemTime;
pub fn get_unique_time_if_given() -> Option<SystemTime> {
    use std::time::Duration;

    let env = std::env::var("SOURCE_DATE_EPOCH");

    env.ok()
        .map(|x| {
            x.trim()
                .parse::<u64>()
                .ok()
                .map(|x| SystemTime::UNIX_EPOCH.checked_add(Duration::new(x, 0)))
        })
        .unwrap_or(None)
        .unwrap_or(None)
}

static mut unique_tag_state: i32 = 1i32;
static mut unique_tags_deterministic: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn pdf_font_reset_unique_tag_state() {
    unique_tag_state = 1i32;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_set_deterministic_unique_tags(mut value: i32) {
    unique_tags_deterministic = value;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_make_uniqueTag(mut tag: *mut i8) {
    if unique_tags_deterministic != 0 {
        snprintf(
            tag,
            7,
            b"%06d\x00" as *const u8 as *const i8,
            unique_tag_state,
        );
        unique_tag_state += 1;
        return;
    }
    if unique_tag_state != 0 {
        srand(0);
        unique_tag_state = 0i32
    }
    for i in 0..6 {
        let ch = (rand() % 26i32) as i8;
        *tag.offset(i as isize) = (ch as i32 + 'A' as i32) as i8;
    }
    *tag.offset(6) = '\u{0}' as i32 as i8;
}
unsafe fn pdf_init_font_struct(mut font: *mut pdf_font) {
    assert!(!font.is_null());
    (*font).ident = ptr::null_mut();
    (*font).map_name = ptr::null_mut();
    (*font).subtype = -1i32;
    (*font).font_id = -1i32;
    (*font).fontname = ptr::null_mut();
    memset((*font).uniqueID.as_mut_ptr() as *mut libc::c_void, 0i32, 7);
    (*font).index = 0i32;
    (*font).encoding_id = -1i32;
    (*font).reference = ptr::null_mut();
    (*font).resource = ptr::null_mut();
    (*font).descriptor = ptr::null_mut();
    (*font).point_size = 0i32 as f64;
    (*font).design_size = 0i32 as f64;
    (*font).usedchars = ptr::null_mut();
    (*font).flags = 0i32;
}
unsafe fn pdf_flush_font(mut font: *mut pdf_font) {
    if font.is_null() {
        return;
    }
    if !(*font).resource.is_null() && !(*font).reference.is_null() {
        if (*font).subtype != 2i32 {
            if pdf_font_get_flag(font, 1i32 << 0i32) != 0 {
                pdf_add_dict(
                    &mut *(*font).resource,
                    "BaseFont",
                    pdf_copy_name((*font).fontname),
                );
                if !(*font).descriptor.is_null() {
                    pdf_add_dict(
                        &mut *(*font).descriptor,
                        "FontName",
                        pdf_copy_name((*font).fontname),
                    );
                }
            } else {
                if (*font).fontname.is_null() {
                    panic!(
                        "Undefined in fontname... ({})",
                        CStr::from_ptr((*font).ident).display()
                    );
                }
                let fontname = new(((7usize)
                    .wrapping_add(strlen((*font).fontname))
                    .wrapping_add(1))
                .wrapping_mul(::std::mem::size_of::<i8>()) as _)
                    as *mut i8;
                let uniqueTag = pdf_font_get_uniqueTag(font);
                sprintf(
                    fontname,
                    b"%6s+%s\x00" as *const u8 as *const i8,
                    uniqueTag,
                    (*font).fontname,
                );
                pdf_add_dict(&mut *(*font).resource, "BaseFont", pdf_copy_name(fontname));
                if !(*font).descriptor.is_null() {
                    pdf_add_dict(&mut *(*font).descriptor, "FontName", pdf_copy_name(fontname));
                }
                free(fontname as *mut libc::c_void);
            }
            if !(*font).descriptor.is_null() {
                pdf_add_dict(
                    &mut *(*font).resource,
                    "FontDescriptor",
                    pdf_ref_obj((*font).descriptor),
                );
            }
        }
    }
    pdf_release_obj((*font).resource);
    pdf_release_obj((*font).descriptor);
    pdf_release_obj((*font).reference);
    (*font).reference = ptr::null_mut();
    (*font).resource = ptr::null_mut();
    (*font).descriptor = ptr::null_mut();
}
unsafe fn pdf_clean_font_struct(mut font: *mut pdf_font) {
    if !font.is_null() {
        free((*font).ident as *mut libc::c_void);
        free((*font).map_name as *mut libc::c_void);
        free((*font).fontname as *mut libc::c_void);
        free((*font).usedchars as *mut libc::c_void);
        if !(*font).reference.is_null() {
            panic!("pdf_font>> Object not flushed.");
        }
        if !(*font).resource.is_null() {
            panic!("pdf_font> Object not flushed.");
        }
        if !(*font).descriptor.is_null() {
            panic!("pdf_font>> Object not flushed.");
        }
        (*font).ident = ptr::null_mut();
        (*font).map_name = ptr::null_mut();
        (*font).fontname = ptr::null_mut();
        (*font).usedchars = ptr::null_mut()
    };
}
static mut font_cache: C2RustUnnamed_0 = C2RustUnnamed_0 {
        count: 0i32,
        capacity: 0i32,
        fonts: std::ptr::null_mut(),
    };
#[no_mangle]
pub unsafe extern "C" fn pdf_init_fonts() {
    assert!(font_cache.fonts.is_null());
    agl_init_map();
    CMap_cache_init();
    pdf_init_encodings();
    Type0Font_cache_init();
    font_cache.count = 0i32;
    font_cache.capacity = 16u32 as i32;
    font_cache.fonts = new((font_cache.capacity as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<pdf_font>() as u64) as u32)
        as *mut pdf_font;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_get_font_reference(mut font_id: i32) -> *mut pdf_obj {
    if font_id < 0i32 || font_id >= font_cache.count {
        panic!("Invalid font ID: {}", font_id);
    }
    let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
    if (*font).subtype == 4i32 {
        let t0font = Type0Font_cache_get((*font).font_id);
        return Type0Font_get_resource(t0font);
    } else {
        if (*font).reference.is_null() {
            (*font).reference = pdf_ref_obj(pdf_font_get_resource(&mut *font))
        }
    }
    pdf_link_obj((*font).reference)
}
#[no_mangle]
pub unsafe extern "C" fn pdf_get_font_usedchars(mut font_id: i32) -> *mut i8 {
    if font_id < 0i32 || font_id >= font_cache.count {
        panic!("Invalid font ID: {}", font_id);
    }
    let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
    if (*font).subtype == 4i32 {
        let t0font = Type0Font_cache_get((*font).font_id);
        return Type0Font_get_usedchars(t0font);
    } else {
        if (*font).usedchars.is_null() {
            (*font).usedchars =
                new((256_u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32) as *mut i8;
            memset(
                (*font).usedchars as *mut libc::c_void,
                0i32,
                (256usize).wrapping_mul(::std::mem::size_of::<i8>()),
            );
        }
        return (*font).usedchars;
    };
}
#[no_mangle]
pub unsafe extern "C" fn pdf_get_font_wmode(mut font_id: i32) -> i32 {
    if font_id < 0i32 || font_id >= font_cache.count {
        panic!("Invalid font ID: {}", font_id);
    }
    let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
    if (*font).subtype == 4i32 {
        let t0font = Type0Font_cache_get((*font).font_id);
        return Type0Font_get_wmode(t0font);
    } else {
        return 0i32;
    };
}
#[no_mangle]
pub unsafe extern "C" fn pdf_get_font_subtype(mut font_id: i32) -> i32 {
    if font_id < 0i32 || font_id >= font_cache.count {
        panic!("Invalid font ID: {}", font_id);
    }
    let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
    (*font).subtype
}
#[no_mangle]
pub unsafe extern "C" fn pdf_get_font_encoding(mut font_id: i32) -> i32 {
    if font_id < 0i32 || font_id >= font_cache.count {
        panic!("Invalid font ID: {}", font_id);
    }
    let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
    (*font).encoding_id
}
/* The rule for ToUnicode creation is:
 *
 *  If "tounicode" option is specified in fontmap, use that.
 *  If there is ToUnicode CMap with same name as TFM, use that.
 *  If no "tounicode" option is used and no ToUnicode CMap with
 *  same name as TFM is found, create ToUnicode CMap from glyph
 *  names and AGL file.
 */
unsafe fn try_load_ToUnicode_CMap(mut font: *mut pdf_font) -> i32 {
    /* Be sure fontmap is still alive here */
    assert!(!font.is_null());
    /* We are using different encoding for Type0 font.
     * This feature is unavailable for them.
     */
    if (*font).subtype == 4i32 {
        return 0i32;
    } /* _FIXME_ */
    assert!(!(*font).map_name.is_null());
    let mrec = pdf_lookup_fontmap_record(CStr::from_ptr((*font).map_name).to_bytes());
    let cmap_name = if !mrec.is_null() && !(*mrec).opt.tounicode.is_null() {
        (*mrec).opt.tounicode
    } else {
        (*font).map_name
    };
    let fontdict = pdf_font_get_resource(&mut *font);
    let tounicode = pdf_load_ToUnicode_stream(cmap_name);
    if tounicode.is_null() && (!mrec.is_null() && !(*mrec).opt.tounicode.is_null()) {
        warn!(
            "Failed to read ToUnicode mapping \"{}\"...",
            CStr::from_ptr((*mrec).opt.tounicode).display(),
        );
    } else if !tounicode.is_null() {
        if !(*tounicode).is_stream() {
            panic!("Object returned by pdf_load_ToUnicode_stream() not stream object! (This must be bug)");
        } else {
            if pdf_stream_length(&*tounicode) > 0i32 {
                pdf_add_dict(fontdict, "ToUnicode", pdf_ref_obj(tounicode));
                if __verbose != 0 {
                    info!(
                        "pdf_font>> ToUnicode CMap \"{}\" attached to font id=\"{}\".\n",
                        CStr::from_ptr(cmap_name).display(),
                        CStr::from_ptr((*font).map_name).display(),
                    );
                }
            }
        }
        pdf_release_obj(tounicode);
    }
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_close_fonts() {
    let mut font_id = 0;
    while font_id < font_cache.count {
        let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
        if __verbose != 0 {
            if (*font).subtype != 4i32 {
                info!("({}", CStr::from_ptr(pdf_font_get_ident(font)).display(),);
                if __verbose > 2i32 && pdf_font_get_flag(font, 1i32 << 0i32) == 0 {
                    info!(
                        "[{}+{}]",
                        CStr::from_ptr(pdf_font_get_uniqueTag(font)).display(),
                        CStr::from_ptr(pdf_font_get_fontname(font)).display(),
                    );
                } else if __verbose > 1i32 {
                    info!(
                        "[{}]",
                        CStr::from_ptr(pdf_font_get_fontname(font)).display()
                    );
                }
                if __verbose > 1i32 {
                    if pdf_font_get_encoding(font) >= 0i32 {
                        info!(
                            "[{}]",
                            CStr::from_ptr(pdf_encoding_get_name(pdf_font_get_encoding(font)))
                                .display()
                        );
                    } else {
                        info!("[built-in]");
                    }
                }
            }
        }
        /* Must come before load_xxx */
        try_load_ToUnicode_CMap(font);
        /* Type 0 is handled separately... */
        match (*font).subtype {
            0 => {
                if __verbose != 0 {
                    info!("[Type1]");
                }
                if pdf_font_get_flag(font, 1i32 << 2i32) == 0 {
                    pdf_font_load_type1(font);
                }
            }
            1 => {
                if __verbose != 0 {
                    info!("[Type1C]");
                }
                pdf_font_load_type1c(font);
            }
            3 => {
                if __verbose != 0 {
                    info!("[TrueType]");
                }
                pdf_font_load_truetype(font);
            }
            2 => {
                if __verbose != 0 {
                    info!("[Type3/PK]");
                }
                pdf_font_load_pkfont(font);
            }
            4 => {}
            _ => {
                panic!("Unknown font type: {}", (*font).subtype);
            }
        }
        if (*font).encoding_id >= 0i32 && (*font).subtype != 4i32 {
            pdf_encoding_add_usedchars((*font).encoding_id, (*font).usedchars);
        }
        if __verbose != 0 {
            if (*font).subtype != 4i32 {
                info!(")");
            }
        }
        font_id += 1
    }
    pdf_encoding_complete();
    let mut font_id = 0;
    while font_id < font_cache.count {
        let mut font_0: *mut pdf_font =
            &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
        if (*font_0).encoding_id >= 0i32 && (*font_0).subtype != 4i32 {
            let mut enc_obj: *mut pdf_obj = pdf_get_encoding_obj((*font_0).encoding_id);
            let mut tounicode: *mut pdf_obj = ptr::null_mut();
            /* Predefined encodings (and those simplified to them) are embedded
            as direct objects, but this is purely a matter of taste. */
            if !enc_obj.is_null() {
                pdf_add_dict(
                    &mut *(*font_0).resource,
                    "Encoding",
                    if !enc_obj.is_null() && (*enc_obj).is_name() {
                        pdf_link_obj(enc_obj)
                    } else {
                        pdf_ref_obj(enc_obj)
                    },
                );
            }
            if !(*(*font_0).resource).as_dict().has("ToUnicode") && {
                tounicode = pdf_encoding_get_tounicode((*font_0).encoding_id);
                !tounicode.is_null()
            } {
                pdf_add_dict(&mut *(*font_0).resource, "ToUnicode", pdf_ref_obj(tounicode));
            }
        } else if (*font_0).subtype == 3i32 {
            /* encoding_id < 0 means MacRoman here (but not really)
             * We use MacRoman as "default" encoding. */
            pdf_add_dict(
                &mut *(*font_0).resource,
                "Encoding",
                pdf_new_name("MacRomanEncoding"),
            ); /* After encoding */
        }
        pdf_flush_font(font_0);
        pdf_clean_font_struct(font_0);
        font_id += 1
    }
    font_cache.fonts = mfree(font_cache.fonts as *mut libc::c_void) as *mut pdf_font;
    font_cache.count = 0i32;
    font_cache.capacity = 0i32;
    Type0Font_cache_close();
    CMap_cache_close();
    pdf_close_encodings();
    agl_close_map();
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_findresource(
    mut tex_name: *const i8,
    mut font_scale: f64,
    mut mrec: *mut fontmap_rec,
) -> i32 {
    let mut font_id;
    let mut encoding_id: i32 = -1i32;
    let mut cmap_id: i32 = -1i32;
    /*
     * Get appropriate info from map file. (PK fonts at two different
     * point sizes would be looked up twice unecessarily.)
     */
    let fontname = if !mrec.is_null() {
        (*mrec).font_name as *const i8
    } else {
        tex_name
    };
    if !mrec.is_null() && !(*mrec).enc_name.is_null() {
        if strstr((*mrec).enc_name, b".enc\x00" as *const u8 as *const i8).is_null()
            || !strstr((*mrec).enc_name, b".cmap\x00" as *const u8 as *const i8).is_null()
        {
            cmap_id = CMap_cache_find((*mrec).enc_name);
            if cmap_id >= 0i32 {
                let cmap = CMap_cache_get(cmap_id);
                let cmap_type = CMap_get_type(cmap);
                let minbytes = CMap_get_profile(cmap, 0i32);
                /*
                 * Check for output encoding.
                 */
                if cmap_type != 0i32 && cmap_type != 1i32 && cmap_type != 2i32 {
                    warn!("Only 16-bit encoding supported for output encoding.");
                }
                /*
                 * Turn on map option.
                 */
                if minbytes == 2i32 && (*mrec).opt.mapc < 0i32 {
                    if __verbose != 0 {
                        info!("\n");
                        info!(
                            "pdf_font>> Input encoding \"{}\" requires at least 2 bytes.\n",
                            CStr::from_ptr(CMap_get_name(cmap)).display()
                        );
                        info!(
                            "pdf_font>> The -m <00> option will be assumed for \"{}\".\n",
                            CStr::from_ptr((*mrec).font_name).display()
                        );
                    }
                    (*mrec).opt.mapc = 0i32
                    /* _FIXME_ */
                }
            } else if streq_ptr((*mrec).enc_name, b"unicode\x00" as *const u8 as *const i8) {
                cmap_id = otf_load_Unicode_CMap(
                    (*mrec).font_name,
                    (*mrec).opt.index,
                    (*mrec).opt.otl_tags,
                    if (*mrec).opt.flags & 1i32 << 2i32 != 0 {
                        1i32
                    } else {
                        0i32
                    },
                );
                if cmap_id < 0i32 {
                    cmap_id = t1_load_UnicodeCMap(
                        (*mrec).font_name,
                        (*mrec).opt.otl_tags,
                        if (*mrec).opt.flags & 1i32 << 2i32 != 0 {
                            1i32
                        } else {
                            0i32
                        },
                    )
                }
                if cmap_id < 0i32 {
                    panic!("Failed to read UCS2/UCS4 TrueType cmap...");
                }
            }
        }
        if cmap_id < 0i32 {
            encoding_id = pdf_encoding_findresource((*mrec).enc_name);
            if encoding_id < 0i32 {
                panic!(
                    "Could not find encoding file \"{}\".",
                    CStr::from_ptr((*mrec).enc_name).display()
                );
            }
        }
    }
    if !mrec.is_null() && cmap_id >= 0i32 {
        /*
         * Composite Font
         */
        let mut found: i32 = 0i32;
        let type0_id = Type0Font_cache_find((*mrec).font_name, cmap_id, &mut (*mrec).opt);
        if type0_id < 0i32 {
            return -1i32;
        }
        font_id = 0i32;
        while font_id < font_cache.count {
            let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
            if (*font).subtype == 4i32
                && (*font).font_id == type0_id
                && (*font).encoding_id == cmap_id
            {
                found = 1i32;
                if __verbose != 0 {
                    info!(
                        "\npdf_font>> Type0 font \"{}\" (cmap_id={}) found at font_id={}.\n",
                        CStr::from_ptr((*mrec).font_name).display(),
                        cmap_id,
                        font_id,
                    );
                }
                break;
            } else {
                font_id += 1
            }
        }
        if found == 0 {
            font_id = font_cache.count;
            if font_cache.count >= font_cache.capacity {
                font_cache.capacity =
                    (font_cache.capacity as u32).wrapping_add(16u32) as i32 as i32;
                font_cache.fonts = renew(
                    font_cache.fonts as *mut libc::c_void,
                    (font_cache.capacity as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<pdf_font>() as u64)
                        as u32,
                ) as *mut pdf_font
            }
            let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
            pdf_init_font_struct(font);
            (*font).font_id = type0_id;
            (*font).subtype = 4i32;
            (*font).encoding_id = cmap_id;
            font_cache.count += 1;
            if __verbose != 0 {
                info!(
                    "\npdf_font>> Type0 font \"{}\"",
                    CStr::from_ptr(fontname).display()
                );
                info!(
                    " cmap_id=<{},{}>",
                    CStr::from_ptr((*mrec).enc_name).display(),
                    (*font).encoding_id,
                );
                info!(
                    " opened at font_id=<{},{}>.\n",
                    CStr::from_ptr(tex_name).display(),
                    font_id,
                );
            }
        }
    } else {
        /*
         * Simple Font - always embed.
         */
        let mut found_0: i32 = 0i32;
        font_id = 0i32;
        while font_id < font_cache.count {
            let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
            match (*font).subtype {
                0 | 1 | 3 => {
                    /* fontname here is font file name.
                     * We must compare both font file name and encoding
                     *
                     * TODO: Embed a font only once if it is used
                     *       with two different encodings
                     */
                    if streq_ptr(fontname, (*font).ident) as i32 != 0
                        && encoding_id == (*font).encoding_id
                    {
                        if !mrec.is_null() && (*mrec).opt.index == (*font).index {
                            found_0 = 1i32
                        }
                    }
                }
                2 => {
                    /* There shouldn't be any encoding specified for PK font.
                     * It must be always font's build-in encoding.
                     *
                     * TODO: a PK font with two encodings makes no sense. Change?
                     */
                    if streq_ptr(fontname, (*font).ident) as i32 != 0
                        && font_scale == (*font).point_size
                    {
                        found_0 = 1i32
                    }
                }
                4 => {}
                _ => {
                    panic!("Unknown font type: {}", (*font).subtype);
                }
            }
            if found_0 != 0 {
                if __verbose != 0 {
                    info!(
                        "\npdf_font>> Simple font \"{}\" (enc_id={}) found at id={}.\n",
                        CStr::from_ptr(fontname).display(),
                        encoding_id,
                        font_id,
                    );
                }
                break;
            } else {
                font_id += 1
            }
        }
        if found_0 == 0 {
            font_id = font_cache.count;
            if font_cache.count >= font_cache.capacity {
                font_cache.capacity =
                    (font_cache.capacity as u32).wrapping_add(16u32) as i32 as i32;
                font_cache.fonts = renew(
                    font_cache.fonts as *mut libc::c_void,
                    (font_cache.capacity as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<pdf_font>() as u64)
                        as u32,
                ) as *mut pdf_font
            }
            let font = &mut *font_cache.fonts.offset(font_id as isize) as *mut pdf_font;
            pdf_init_font_struct(font);
            (*font).point_size = font_scale;
            (*font).encoding_id = encoding_id;
            (*font).ident = new(
                (strlen(fontname).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
            ) as *mut i8;
            strcpy((*font).ident, fontname);
            (*font).map_name = new(
                (strlen(tex_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
            ) as *mut i8;
            strcpy((*font).map_name, tex_name);
            (*font).index = if !mrec.is_null() && (*mrec).opt.index != 0 {
                (*mrec).opt.index
            } else {
                0i32
            };
            if pdf_font_open_type1(font) >= 0i32 {
                (*font).subtype = 0i32
            } else if pdf_font_open_type1c(font) >= 0i32 {
                (*font).subtype = 1i32
            } else if pdf_font_open_truetype(font) >= 0i32 {
                (*font).subtype = 3i32
            } else if pdf_font_open_pkfont(font) >= 0i32 {
                (*font).subtype = 2i32
            } else {
                pdf_clean_font_struct(font);
                return -1i32;
            }
            font_cache.count += 1;
            if __verbose != 0 {
                info!(
                    "\npdf_font>> Simple font \"{}\"",
                    CStr::from_ptr(fontname).display()
                );
                info!(
                    " enc_id=<{},{}>",
                    if !mrec.is_null() && !(*mrec).enc_name.is_null() {
                        CStr::from_ptr((*mrec).enc_name as *const i8)
                            .to_str()
                            .unwrap()
                    } else {
                        "builtin"
                    },
                    (*font).encoding_id,
                );
                info!(
                    " opened at font_id=<{},{}>.\n",
                    CStr::from_ptr(tex_name).display(),
                    font_id,
                );
            }
        }
    }
    font_id
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_is_in_use(mut font: *mut pdf_font) -> bool {
    assert!(!font.is_null());
    return if !(*font).reference.is_null() {
        1i32
    } else {
        0i32
    } != 0;
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_index(mut font: *mut pdf_font) -> i32 {
    assert!(!font.is_null());
    (*font).index
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_ident(mut font: *mut pdf_font) -> *mut i8 {
    assert!(!font.is_null());
    (*font).ident
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_mapname(mut font: *mut pdf_font) -> *mut i8 {
    assert!(!font.is_null());
    (*font).map_name
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_fontname(mut font: *mut pdf_font) -> *mut i8 {
    assert!(!font.is_null());
    (*font).fontname
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_resource(font: &mut pdf_font) -> &mut pdf_obj {
    if (*font).resource.is_null() {
        (*font).resource = pdf_new_dict();
        pdf_add_dict(&mut *(*font).resource, "Type", pdf_new_name("Font"));
        match (*font).subtype {
            0 | 1 => {
                pdf_add_dict(&mut *(*font).resource, "Subtype", pdf_new_name("Type1"));
            }
            2 => {
                pdf_add_dict(&mut *(*font).resource, "Subtype", pdf_new_name("Type3"));
            }
            3 => {
                pdf_add_dict(&mut *(*font).resource, "Subtype", pdf_new_name("TrueType"));
            }
            _ => unreachable!()
        }
    }
    &mut *(*font).resource
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_descriptor(mut font: *mut pdf_font) -> *mut pdf_obj {
    assert!(!font.is_null());
    if (*font).descriptor.is_null() {
        (*font).descriptor = pdf_new_dict();
        pdf_add_dict(&mut *(*font).descriptor, "Type", pdf_new_name("FontDescriptor"));
    }
    (*font).descriptor
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_usedchars(mut font: *mut pdf_font) -> *mut i8 {
    assert!(!font.is_null());
    (*font).usedchars
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_encoding(mut font: *mut pdf_font) -> i32 {
    assert!(!font.is_null());
    (*font).encoding_id
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_flag(mut font: *mut pdf_font, mut mask: i32) -> i32 {
    assert!(!font.is_null());
    return if (*font).flags & mask != 0 {
        1i32
    } else {
        0i32
    };
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_param(mut font: *mut pdf_font, mut param_type: i32) -> f64 {
    let mut param: f64 = 0.0f64;
    assert!(!font.is_null());
    match param_type {
        1 => param = (*font).design_size,
        2 => param = (*font).point_size,
        _ => {}
    }
    param
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_get_uniqueTag(mut font: *mut pdf_font) -> *mut i8 {
    assert!(!font.is_null());
    if (*font).uniqueID[0] as i32 == '\u{0}' as i32 {
        pdf_font_make_uniqueTag((*font).uniqueID.as_mut_ptr());
    }
    (*font).uniqueID.as_mut_ptr()
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_set_fontname(
    mut font: *mut pdf_font,
    mut fontname: *const i8,
) -> i32 {
    assert!(!font.is_null() && !fontname.is_null());
    if strlen(fontname) > 127 {
        panic!("Unexpected error...");
    }
    if !(*font).fontname.is_null() {
        free((*font).fontname as *mut libc::c_void);
    }
    (*font).fontname =
        new((strlen(fontname).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*font).fontname, fontname);
    0i32
}
#[no_mangle]
pub unsafe extern "C" fn pdf_font_set_subtype(mut font: *mut pdf_font, mut subtype: i32) -> i32 {
    assert!(!font.is_null());
    (*font).subtype = subtype;
    0i32
}
/* pdf_open_document() call them. */
/* font_name is used when mrec is NULL.
 * font_scale (point size) used by PK font.
 * It might be necessary if dvipdfmx supports font format with
 * various optical sizes supported in the future.
 */
/* Each font drivers use the followings. */
/* without unique tag */
#[no_mangle]
pub unsafe extern "C" fn pdf_font_set_flags(mut font: *mut pdf_font, mut flags: i32) -> i32 {
    assert!(!font.is_null());
    (*font).flags |= flags;
    0i32
}
