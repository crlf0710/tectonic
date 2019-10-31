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

use crate::dpx_pdfparse::ParsePdfObj;
use crate::mfree;
use crate::{info, warn};
use crate::{streq_ptr, strstartswith};

use super::dpx_cff::cff_release_charsets;
use super::dpx_cidtype0::{
    CIDFont_type0_dofont, CIDFont_type0_open, CIDFont_type0_set_flags, CIDFont_type0_set_verbose,
    CIDFont_type0_t1cdofont, CIDFont_type0_t1dofont,
};
use super::dpx_cidtype2::{
    CIDFont_type2_dofont, CIDFont_type2_open, CIDFont_type2_set_flags, CIDFont_type2_set_verbose,
};
use super::dpx_mem::{new, renew};
use crate::dpx_pdfobj::{
    pdf_add_dict, pdf_copy_name, pdf_file, pdf_get_version, pdf_link_obj,
    pdf_name_value, pdf_new_name, pdf_number_value, pdf_obj, pdf_ref_obj,
    pdf_release_obj, pdf_remove_dict, pdf_string_value,
};
use libc::{free, memcpy, memset, strcat, strchr, strcmp, strcpy, strlen, strncmp, strtoul};

#[derive(Copy, Clone)]
#[repr(C)]
pub struct CIDSysInfo {
    pub registry: *mut i8,
    pub ordering: *mut i8,
    pub supplement: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct CIDFont {
    pub ident: *mut i8,
    pub name: *mut i8,
    pub fontname: *mut i8,
    pub subtype: i32,
    pub flags: i32,
    pub parent: [i32; 2],
    pub csi: *mut CIDSysInfo,
    pub options: *mut cid_opt,
    pub indirect: *mut pdf_obj,
    pub fontdict: *mut pdf_obj,
    pub descriptor: *mut pdf_obj,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct cid_opt {
    pub name: *mut i8,
    pub csi: *mut CIDSysInfo,
    pub index: i32,
    pub style: i32,
    pub embed: i32,
    pub stemv: i32,
    pub cff_charsets: *mut libc::c_void,
}
use super::dpx_fontmap::fontmap_opt;
/*
 * Unicode and PDF Standard Character Collections.
 *
 *  Adobe-Identity is only for TrueType fonts and it means font's
 *  internal glyph ordering.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_0 {
    pub registry: *const i8,
    pub ordering: *const i8,
    pub supplement: [i32; 16],
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct FontCache {
    pub num: i32,
    pub max: i32,
    pub fonts: *mut *mut CIDFont,
}
use super::dpx_cff::cff_charsets;
/* PLEASE SEND INFORMATION ON FONTS
 *
 * Those fonts are only for fixed-pitch glyphs (full-, half-, third-,
 * and quarter- widths). Glyph widths should be determined solely from
 * CID and CIDSystemInfo for those never-embedded fonts. Fixed-pitch
 * pre-rotated forms are not supported yet.
 *
 * Font dictionaly entry Subtype and CIDSystemInfo is mandatory here.
 * Font descriptor entry CapHeight, Ascent, Descent, ItalicAngle, Flags,
 * FontBBox, and StemV is required. However, CapHeight, Ascent, Descent,
 * and ItalicAngle is irrelevant for font-matching of CJK fonts. Panose
 * entry in Style dictionary may affect in Acrobat. Serif font should
 * have flag bit position 2 (2) set and all CJK fonts should also have
 * bit position 3 (4) set: CJK font serif -> Flags 6, sans serif -> 4.
 *
 * Please always supply DW entry in font dictionary although this can
 * be optional. PDF reference 1.2 had been described default value for
 * DW as 0 (correct value if 1000) and several versions of Mac OS X
 * implement so!
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_2 {
    pub fontname: *const i8,
    pub fontdict: *const i8,
    pub descriptor: *const i8,
}
/*
 * Optional supplement after alias name.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct C2RustUnnamed_3 {
    pub name: *const i8,
    pub index: i32,
}
/* tectonic/core-strutils.h: miscellaneous C string utilities
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
/* Note that we explicitly do *not* change this on Windows. For maximum
 * portability, we should probably accept *either* forward or backward slashes
 * as directory separators. */
/* tectonic/core-memory.h: basic dynamic memory helpers
   Copyright 2016-2018 the Tectonic Project
   Licensed under the MIT License.
*/
static mut CIDFont_stdcc_def: [C2RustUnnamed_0; 7] = [
    C2RustUnnamed_0 {
            registry: b"Adobe\x00" as *const u8 as *const i8,
            ordering: b"UCS\x00" as *const u8 as *const i8,
            supplement: [
                -1i32, -1i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
    C2RustUnnamed_0 {
            registry: b"Adobe\x00" as *const u8 as *const i8,
            ordering: b"GB1\x00" as *const u8 as *const i8,
            supplement: [
                -1i32, -1i32, 0i32, 2i32, 4i32, 4i32, 4i32, 4i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
    C2RustUnnamed_0 {
            registry: b"Adobe\x00" as *const u8 as *const i8,
            ordering: b"CNS1\x00" as *const u8 as *const i8,
            supplement: [
                -1i32, -1i32, 0i32, 0i32, 3i32, 4i32, 4i32, 4i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
    C2RustUnnamed_0 {
            registry: b"Adobe\x00" as *const u8 as *const i8,
            ordering: b"Japan1\x00" as *const u8 as *const i8,
            supplement: [
                -1i32, -1i32, 2i32, 2i32, 4i32, 5i32, 6i32, 6i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
    C2RustUnnamed_0 {
            registry: b"Adobe\x00" as *const u8 as *const i8,
            ordering: b"Korea1\x00" as *const u8 as *const i8,
            supplement: [
                -1i32, -1i32, 1i32, 1i32, 2i32, 2i32, 2i32, 2i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
    C2RustUnnamed_0 {
            registry: b"Adobe\x00" as *const u8 as *const i8,
            ordering: b"Identity\x00" as *const u8 as *const i8,
            supplement: [
                -1i32, -1i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
    C2RustUnnamed_0 {
            registry: 0 as *const i8,
            ordering: 0 as *const i8,
            supplement: [
                0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0i32, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        },
];
static mut registry_Adobe: [i8; 6] = [65, 100, 111, 98, 101, 0];
static mut ordering_Identity: [i8; 9] = [73, 100, 101, 110, 116, 105, 116, 121, 0];
static mut ordering_UCS: [i8; 4] = [85, 67, 83, 0];
#[no_mangle]
pub static mut CSI_IDENTITY: CIDSysInfo = unsafe {
    CIDSysInfo {
            registry: registry_Adobe.as_ptr() as *mut _,
            ordering: ordering_Identity.as_ptr() as *mut _,
            supplement: 0i32,
        }
};
#[no_mangle]
pub static mut CSI_UNICODE: CIDSysInfo = unsafe {
    CIDSysInfo {
            registry: registry_Adobe.as_ptr() as *mut _,
            ordering: ordering_UCS.as_ptr() as *mut _,
            supplement: 0i32,
        }
};
static mut CIDFont_stdcc_alias: [C2RustUnnamed_3; 19] = [
    C2RustUnnamed_3 {
            name: b"AU\x00" as *const u8 as *const i8,
            index: 0i32,
        },
    C2RustUnnamed_3 {
            name: b"AG1\x00" as *const u8 as *const i8,
            index: 1i32,
        },
    C2RustUnnamed_3 {
            name: b"AC1\x00" as *const u8 as *const i8,
            index: 2i32,
        },
    C2RustUnnamed_3 {
            name: b"AJ1\x00" as *const u8 as *const i8,
            index: 3i32,
        },
    C2RustUnnamed_3 {
            name: b"AK1\x00" as *const u8 as *const i8,
            index: 4i32,
        },
    C2RustUnnamed_3 {
            name: b"AI\x00" as *const u8 as *const i8,
            index: 5i32,
        },
    C2RustUnnamed_3 {
            name: b"UCS\x00" as *const u8 as *const i8,
            index: 0i32,
        },
    C2RustUnnamed_3 {
            name: b"GB1\x00" as *const u8 as *const i8,
            index: 1i32,
        },
    C2RustUnnamed_3 {
            name: b"CNS1\x00" as *const u8 as *const i8,
            index: 2i32,
        },
    C2RustUnnamed_3 {
            name: b"Japan1\x00" as *const u8 as *const i8,
            index: 3i32,
        },
    C2RustUnnamed_3 {
            name: b"Korea1\x00" as *const u8 as *const i8,
            index: 4i32,
        },
    C2RustUnnamed_3 {
            name: b"Identity\x00" as *const u8 as *const i8,
            index: 5i32,
        },
    C2RustUnnamed_3 {
            name: b"U\x00" as *const u8 as *const i8,
            index: 0i32,
        },
    C2RustUnnamed_3 {
            name: b"G\x00" as *const u8 as *const i8,
            index: 1i32,
        },
    C2RustUnnamed_3 {
            name: b"C\x00" as *const u8 as *const i8,
            index: 2i32,
        },
    C2RustUnnamed_3 {
            name: b"J\x00" as *const u8 as *const i8,
            index: 3i32,
        },
    C2RustUnnamed_3 {
            name: b"K\x00" as *const u8 as *const i8,
            index: 4i32,
        },
    C2RustUnnamed_3 {
            name: b"I\x00" as *const u8 as *const i8,
            index: 5i32,
        },
    C2RustUnnamed_3 {
            name: 0 as *const i8,
            index: 0i32,
        },
];
static mut __verbose: i32 = 0i32;
static mut cidoptflags: i32 = 0i32;
#[no_mangle]
pub unsafe extern "C" fn CIDFont_set_verbose(mut level: i32) {
    CIDFont_type0_set_verbose(level);
    CIDFont_type2_set_verbose(level);
    __verbose = level;
}
unsafe fn CIDFont_new() -> *mut CIDFont {
    let font =
        new((1_u64).wrapping_mul(::std::mem::size_of::<CIDFont>() as u64) as u32) as *mut CIDFont;
    (*font).name = 0 as *mut i8;
    (*font).fontname = 0 as *mut i8;
    (*font).ident = 0 as *mut i8;
    /*
     * CIDFont
     */
    (*font).subtype = -1i32; /* Horizontal */
    (*font).flags = 0i32; /* Vertical   */
    (*font).csi = 0 as *mut CIDSysInfo;
    (*font).options = 0 as *mut cid_opt;
    (*font).parent[0] = -1i32;
    (*font).parent[1] = -1i32;
    /*
     * PDF Font Resource
     */
    (*font).indirect = 0 as *mut pdf_obj;
    (*font).fontdict = 0 as *mut pdf_obj;
    (*font).descriptor = 0 as *mut pdf_obj;
    font
}
/* It does write PDF objects. */
unsafe fn CIDFont_flush(mut font: *mut CIDFont) {
    if !font.is_null() {
        pdf_release_obj((*font).indirect);
        (*font).indirect = 0 as *mut pdf_obj;
        pdf_release_obj((*font).fontdict);
        (*font).fontdict = 0 as *mut pdf_obj;
        pdf_release_obj((*font).descriptor);
        (*font).descriptor = 0 as *mut pdf_obj
    };
}
unsafe fn CIDFont_release(mut font: *mut CIDFont) {
    if !font.is_null() {
        if !(*font).indirect.is_null() {
            panic!("{}: Object not flushed.", "CIDFont",);
        }
        if !(*font).fontdict.is_null() {
            panic!("{}: Object not flushed.", "CIDFont",);
        }
        if !(*font).descriptor.is_null() {
            panic!("{}: Object not flushed.", "CIDFont",);
        }
        free((*font).fontname as *mut libc::c_void);
        free((*font).name as *mut libc::c_void);
        free((*font).ident as *mut libc::c_void);
        if !(*font).csi.is_null() {
            free((*(*font).csi).registry as *mut libc::c_void);
            free((*(*font).csi).ordering as *mut libc::c_void);
            free((*font).csi as *mut libc::c_void);
        }
        if !(*font).options.is_null() {
            release_opt((*font).options);
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_fontname(mut font: *mut CIDFont) -> *mut i8 {
    assert!(!font.is_null());
    (*font).fontname
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_ident(mut font: *mut CIDFont) -> *mut i8 {
    assert!(!font.is_null());
    (*font).ident
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_opt_index(mut font: *mut CIDFont) -> i32 {
    assert!(!font.is_null());
    if !(*font).options.is_null() {
        (*(*font).options).index
    } else {
        0
    }
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_subtype(mut font: *mut CIDFont) -> i32 {
    assert!(!font.is_null());
    (*font).subtype
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_embedding(mut font: *mut CIDFont) -> i32 {
    assert!(!font.is_null());
    (*(*font).options).embed
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_CIDSysInfo(mut font: *mut CIDFont) -> *mut CIDSysInfo {
    assert!(!font.is_null());
    (*font).csi
}
/*
 * Returns ID of parent Type0 font
 *  wmode: 0 for horizontal, 1 for vertical
 */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_parent_id(mut font: *mut CIDFont, mut wmode: i32) -> i32 {
    assert!(!font.is_null());
    if wmode < 0i32 || wmode > 1i32 {
        panic!("{}: Invalid wmode value.", "CIDFont",);
    }
    (*font).parent[wmode as usize]
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_resource(mut font: *mut CIDFont) -> *mut pdf_obj {
    assert!(!font.is_null());
    if (*font).indirect.is_null() {
        (*font).indirect = pdf_ref_obj((*font).fontdict)
    }
    pdf_link_obj((*font).indirect)
}
/*
 * Set parent Type0 font.
 */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_attach_parent(
    mut font: *mut CIDFont,
    mut parent_id: i32,
    mut wmode: i32,
) {
    assert!(!font.is_null());
    if wmode < 0i32 || wmode > 1i32 {
        panic!("{}: Invalid wmode value.", "CIDFont",);
    }
    if (*font).parent[wmode as usize] >= 0i32 {
        warn!("{}: CIDFont already have a parent Type1 font.", "CIDFont");
    }
    (*font).parent[wmode as usize] = parent_id;
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_is_ACCFont(mut font: *mut CIDFont) -> bool {
    assert!(!font.is_null());
    if (*font).csi.is_null() {
        panic!("{}: CIDSystemInfo undefined.", "CIDFont",);
    }
    for i in 1..=4 {
        if streq_ptr(
            (*(*font).csi).registry,
            CIDFont_stdcc_def[i as usize].registry,
        ) as i32
            != 0
            && streq_ptr(
                (*(*font).csi).ordering,
                CIDFont_stdcc_def[i as usize].ordering,
            ) as i32
                != 0
        {
            return true;
        }
    }
    false
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_is_UCSFont(mut font: *mut CIDFont) -> bool {
    assert!(!font.is_null());
    return streq_ptr(
        (*(*font).csi).ordering,
        b"UCS\x00" as *const u8 as *const i8,
    ) as i32
        != 0
        || streq_ptr(
            (*(*font).csi).ordering,
            b"UCS2\x00" as *const u8 as *const i8,
        ) as i32
            != 0;
}
/* FIXME */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_get_flag(mut font: *mut CIDFont, mut mask: i32) -> i32 {
    assert!(!font.is_null());
    return if (*font).flags & mask != 0 {
        1i32
    } else {
        0i32
    };
}
unsafe fn CIDFont_dofont(mut font: *mut CIDFont) {
    if font.is_null() || (*font).indirect.is_null() {
        return;
    }
    if __verbose != 0 {
        info!(":{}", CStr::from_ptr((*font).ident).display());
    }
    if __verbose > 1i32 {
        if !(*font).fontname.is_null() {
            info!("[{}]", CStr::from_ptr((*font).fontname).display());
        }
    }
    match (*font).subtype {
        1 => {
            if __verbose != 0 {
                info!("[CIDFontType0]");
            }
            if CIDFont_get_flag(font, 1i32 << 8i32) != 0 {
                CIDFont_type0_t1dofont(font);
            } else if CIDFont_get_flag(font, 1i32 << 9i32) != 0 {
                CIDFont_type0_t1cdofont(font);
            } else {
                CIDFont_type0_dofont(font);
            }
        }
        2 => {
            if __verbose != 0 {
                info!("[CIDFontType2]");
            }
            CIDFont_type2_dofont(font);
        }
        _ => {
            panic!("{}: Unknown CIDFontType {}.", "CIDFont", (*font).subtype,);
        }
    };
}
/*
 *
 */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_is_BaseFont(mut font: *mut CIDFont) -> bool {
    assert!(!font.is_null());
    (*font).flags & 1i32 << 0i32 != 0
}
static mut cid_basefont: [C2RustUnnamed_2; 21] = [
    C2RustUnnamed_2{fontname:
                                 b"Ryumin-Light\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /CapHeight 709 /Ascent 723 /Descent -241 /StemV 69 /FontBBox [-170 -331 1024 903] /ItalicAngle 0 /Flags 6 /Style << /Panose <010502020300000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"GothicBBB-Medium\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo <<  /Registry (Adobe) /Ordering (Japan1) /Supplement 2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /CapHeight 737 /Ascent 752 /Descent -271 /StemV 99 /FontBBox [-174 -268 1001 944] /ItalicAngle 0 /Flags 4 /Style << /Panose <0801020b0500000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"MHei-Medium-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 0 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-45 -250 1015 887] /ItalicAngle 0 /Flags 4 /XHeight 553 /Style << /Panose <000001000600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"MSung-Light-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 0 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-160 -249 1015 888] /ItalicAngle 0 /Flags 6 /XHeight 553 /Style << /Panose <000000000400000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"STSong-Light-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 2 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-25 -254 1000 880] /ItalicAngle 0 /Flags 6 /XHeight 599 /Style << /Panose <000000000400000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"STHeiti-Regular-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 1 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-34 -250 1000 882] /ItalicAngle 0 /Flags 4 /XHeight 599 /Style << /Panose <000001000600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"HeiseiKakuGo-W5-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement  2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -221 /StemV 114 /FontBBox [-92 -250 1010 922] /ItalicAngle 0 /Flags 4 /XHeight 553 /Style << /Panose <0801020b0600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"HeiseiMin-W3-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 723 /CapHeight 709 /Descent -241 /StemV 69 /FontBBox [-123 -257 1001 910] /ItalicAngle 0 /Flags 6 /XHeight 450 /Style << /Panose <010502020400000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"HYGoThic-Medium-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 1 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-6 -145 1003 880] /ItalicAngle 0 /Flags 4 /XHeight 553 /Style << /Panose <000001000600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"HYSMyeongJo-Medium-Acro\x00" as *const u8
                                     as *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 1 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-0 -148 1001 880] /ItalicAngle 0 /Flags 6 /XHeight 553 /Style << /Panose <000000000600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"MSungStd-Light-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 4 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /CapHeight 662 /Descent -120 /StemV 54 /FontBBox [-160 -249 1015 1071] /ItalicAngle 0 /Flags 6 /Style << /Panose <000000000400000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"STSongStd-Light-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 4 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /CapHeight 626 /Descent -120 /StemV 44 /FontBBox [-134 -254 1001 905] /ItalicAngle 0 /Flags 6 /Style << /Panose <000000000400000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"HYSMyeongJoStd-Medium-Acro\x00" as
                                     *const u8 as *const i8,
                             fontdict:
                                 b"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 2 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /CapHeight 720 /Descent -120 /StemV 60 /FontBBox [-28 -148 1001 880] /ItalicAngle 0 /Flags 6 /Style << /Panose <000000000600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"AdobeMingStd-Light-Acro\x00" as *const u8
                                     as *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 4 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 48 /CapHeight 731 /FontBBox [-38 -121 1002 918] /ItalicAngle 0 /Flags 6 /XHeight 466 /Style << /Panose <000002020300000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"AdobeSongStd-Light-Acro\x00" as *const u8
                                     as *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 4 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 66 /CapHeight 626 /FontBBox [-134 -254 1001 905] /ItalicAngle 0 /Flags 6 /XHeight 416 /Style << /Panose <000002020300000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"KozMinPro-Regular-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 4 >> /DW 1000 /W [  231   632 500  8718 [500 500]  9738  9757 250  9758  9778 333 12063 12087 500 ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 86 /CapHeight 740 /FontBBox [-195 -272 1110 1075] /ItalicAngle 0 /Flags 6 /XHeight 502 /Style << /Panose <000002020400000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"KozGoPro-Medium-Acro\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering(Japan1) /Supplement 4 >> /DW 1000 /W [  231   632 500  8718 [500 500]  9738  9757 250  9758  9778 333 12063 12087 500 ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 99 /CapHeight 763 /FontBBox [-149 -374 1254 1008] /ItalicAngle 0 /Flags 4 /XHeight 549 /Style << /Panose <0000020b0700000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"AdobeMyungjoStd-Medium-Acro\x00" as
                                     *const u8 as *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 2 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 99 /CapHeight 719 /FontBBox [-28 -148 1001 880] /ItalicAngle 0 /Flags 6 /XHeight 478 /Style << /Panose <000002020600000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"KozMinProVI-Regular\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo <<   /Registry (Adobe)   /Ordering (Japan1)   /Supplement 6 >> /DW 1000 /W [  231   632 500   8718 [500 500]   9738  9757 250   9758  9778 333   12063 12087 500 ]        >>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 86 /CapHeight 742 /FontBBox [-437 -340 1144 1317] /ItalicAngle 0 /Flags 6 /XHeight 503 /Style <<   /Panose <000002020400000000000000> >>      >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2{fontname:
                                 b"AdobeHeitiStd-Regular\x00" as *const u8 as
                                     *const i8,
                             fontdict:
                                 b"<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 4 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>\x00"
                                     as *const u8 as *const i8,
                             descriptor:
                                 b"<< /Ascent 880 /Descent -120 /StemV 66 /CapHeight 626 /FontBBox [-134 -254 1001 905] /ItalicAngle 0 /Flags 6 /XHeight 416 /Style << /Panose <000002020300000000000000> >> >>\x00"
                                     as *const u8 as *const i8,
    },
    C2RustUnnamed_2 {
            fontname: 0 as *const i8,
            fontdict: 0 as *const i8,
            descriptor: 0 as *const i8,
        },
];
unsafe fn CIDFont_base_open(
    mut font: *mut CIDFont,
    mut name: *const i8,
    mut cmap_csi: *mut CIDSysInfo,
    mut opt: *mut cid_opt,
) -> i32 {
    assert!(!font.is_null());
    let mut idx = 0;
    while !cid_basefont[idx].fontname.is_null() {
        if streq_ptr(name, cid_basefont[idx].fontname) as i32 != 0
            || strlen(name)
                == strlen(cid_basefont[idx].fontname)
                    .wrapping_sub(strlen(b"-Acro\x00" as *const u8 as *const i8))
                && strncmp(
                    name,
                    cid_basefont[idx].fontname,
                    strlen(cid_basefont[idx].fontname)
                        .wrapping_sub(strlen(b"-Acro\x00" as *const u8 as *const i8)),
                ) == 0
        {
            break;
        }
        idx += 1
    }
    if cid_basefont[idx].fontname.is_null() {
        return -1i32;
    }
    let fontname =
        new((strlen(name).wrapping_add(12)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    memset(
        fontname as *mut libc::c_void,
        0i32,
        strlen(name).wrapping_add(12),
    );
    strcpy(fontname, name);
    match (*opt).style {
        1 => {
            strcat(fontname, b",Bold\x00" as *const u8 as *const i8);
        }
        2 => {
            strcat(fontname, b",Italic\x00" as *const u8 as *const i8);
        }
        3 => {
            strcat(fontname, b",BoldItalic\x00" as *const u8 as *const i8);
        }
        _ => {}
    }
    let mut start = CStr::from_ptr(cid_basefont[idx].fontdict).to_bytes();
    let fontdict = start.parse_pdf_dict(0 as *mut pdf_file).unwrap();
    let mut start = CStr::from_ptr(cid_basefont[idx].descriptor).to_bytes();
    let descriptor = start.parse_pdf_dict(0 as *mut pdf_file).unwrap();
    (*font).fontname = fontname;
    (*font).flags |= 1i32 << 0i32;
    let tmp = (*fontdict).as_dict().get("CIDSystemInfo")
        .filter(|&tmp| (*tmp).is_dict())
        .unwrap();
    let registry = pdf_string_value(tmp.as_dict().get("Registry").unwrap()) as *mut i8;
    let ordering = pdf_string_value(tmp.as_dict().get("Ordering").unwrap()) as *mut i8;
    let supplement = pdf_number_value(tmp.as_dict().get("Supplement").unwrap()) as i32;
    if !cmap_csi.is_null() {
        /* NULL for accept any */
        if strcmp(registry, (*cmap_csi).registry) != 0
            || strcmp(ordering, (*cmap_csi).ordering) != 0
        {
            panic!(
                "Inconsistent CMap used for CID-keyed font {}.",
                CStr::from_ptr(cid_basefont[idx].fontname).display()
            );
        } else {
            if supplement < (*cmap_csi).supplement {
                warn!(
                    "CMap has higher supplement number than CIDFont: {}",
                    CStr::from_ptr(fontname).display(),
                );
                warn!("Some chracters may not be displayed or printed.");
            }
        }
    }
    (*font).csi = new((1_u64).wrapping_mul(::std::mem::size_of::<CIDSysInfo>() as u64) as u32)
        as *mut CIDSysInfo;
    (*(*font).csi).registry =
        new((strlen(registry).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    (*(*font).csi).ordering =
        new((strlen(ordering).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
    strcpy((*(*font).csi).registry, registry);
    strcpy((*(*font).csi).ordering, ordering);
    (*(*font).csi).supplement = supplement;
    let tmp = (*fontdict).as_dict().get("Subtype")
        .filter(|&tmp| (*tmp).is_name())
        .unwrap();
    let typ = pdf_name_value(&*tmp).to_string_lossy();
    if typ == "CIDFontType0" {
        (*font).subtype = 1i32
    } else if typ == "CIDFontType2" {
        (*font).subtype = 2i32
    } else {
        panic!("Unknown CIDFontType \"{}\"", typ);
    }
    if cidoptflags & 1i32 << 1i32 != 0 {
        if (*fontdict).as_dict().has("W") {
            pdf_remove_dict(&mut *fontdict, "W");
        }
        if (*fontdict).as_dict().has("W2") {
            pdf_remove_dict(&mut *fontdict, "W2");
        }
    }
    pdf_add_dict(&mut *fontdict, "Type", pdf_new_name("Font"));
    pdf_add_dict(&mut *fontdict, "BaseFont", pdf_copy_name(fontname));
    pdf_add_dict(&mut *descriptor, "Type", pdf_new_name("FontDescriptor"));
    pdf_add_dict(&mut *descriptor, "FontName", pdf_copy_name(fontname));
    (*font).fontdict = fontdict;
    (*font).descriptor = descriptor;
    (*opt).embed = 0i32;
    0i32
}
static mut __cache: *mut FontCache = 0 as *const FontCache as *mut FontCache;
unsafe fn CIDFont_cache_init() {
    if !__cache.is_null() {
        panic!("{}: Already initialized.", "CIDFont",);
    }
    __cache = new((1_u64).wrapping_mul(::std::mem::size_of::<FontCache>() as u64) as u32)
        as *mut FontCache;
    (*__cache).max = 16u32 as i32;
    (*__cache).fonts = new(((*__cache).max as u32 as u64)
        .wrapping_mul(::std::mem::size_of::<*mut CIDFont>() as u64)
        as u32) as *mut *mut CIDFont;
    (*__cache).num = 0i32;
}
#[no_mangle]
pub unsafe extern "C" fn CIDFont_cache_get(mut font_id: i32) -> *mut CIDFont {
    if __cache.is_null() {
        panic!("{}: CIDFont cache not initialized.", "CIDFont",);
    }
    if font_id < 0i32 || font_id >= (*__cache).num {
        panic!("{}: Invalid ID {}", "CIDFont", font_id,);
    }
    *(*__cache).fonts.offset(font_id as isize)
}
/*
 * cmap_csi is NULL if CMap is Identity.
 */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_cache_find(
    mut map_name: *const i8,
    mut cmap_csi: *mut CIDSysInfo,
    mut fmap_opt: *mut fontmap_opt,
) -> i32 {
    let mut font: *mut CIDFont = 0 as *mut CIDFont;
    if __cache.is_null() {
        CIDFont_cache_init();
    }
    let opt =
        new((1_u64).wrapping_mul(::std::mem::size_of::<cid_opt>() as u64) as u32) as *mut cid_opt;
    (*opt).style = (*fmap_opt).style;
    (*opt).index = (*fmap_opt).index;
    (*opt).embed = if (*fmap_opt).flags & 1i32 << 1i32 != 0 {
        0i32
    } else {
        1i32
    };
    (*opt).name = 0 as *mut i8;
    (*opt).csi = get_cidsysinfo(map_name, fmap_opt);
    (*opt).stemv = (*fmap_opt).stemv;
    (*opt).cff_charsets = 0 as *mut libc::c_void;
    if (*opt).csi.is_null() && !cmap_csi.is_null() {
        /*
         * No CIDSystemInfo supplied explicitly. Copy from CMap's one if available.
         * It is not neccesary for CID-keyed fonts. But TrueType requires them.
         */
        (*opt).csi = new((1_u64).wrapping_mul(::std::mem::size_of::<CIDSysInfo>() as u64) as u32)
            as *mut CIDSysInfo;
        (*(*opt).csi).registry = new((strlen((*cmap_csi).registry).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*opt).csi).registry, (*cmap_csi).registry);
        (*(*opt).csi).ordering = new((strlen((*cmap_csi).ordering).wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
            as *mut i8;
        strcpy((*(*opt).csi).ordering, (*cmap_csi).ordering);
        (*(*opt).csi).supplement = (*cmap_csi).supplement
    }
    /*
     * Here, we do not compare font->ident and map_name because of
     * implicit CIDSystemInfo supplied by CMap for TrueType.
     */
    let mut font_id = 0;
    while font_id < (*__cache).num {
        font = *(*__cache).fonts.offset(font_id as isize);
        if streq_ptr((*font).name, map_name) as i32 != 0
            && (*(*font).options).style == (*opt).style
            && (*(*font).options).index == (*opt).index
        {
            if (*(*font).options).embed == (*opt).embed {
                /*
                 * Case 1: CSI not available (Identity CMap)
                 *         Font is TrueType --> continue
                 *         Font is CIDFont  --> break
                 * Case 2: CSI matched      --> break
                 */
                if (*opt).csi.is_null() {
                    if !((*font).subtype == 2i32) {
                        break;
                    }
                } else if streq_ptr((*(*font).csi).registry, (*(*opt).csi).registry) as i32 != 0
                    && streq_ptr((*(*font).csi).ordering, (*(*opt).csi).ordering) as i32 != 0
                {
                    if (*font).subtype == 2i32 {
                        (*(*font).csi).supplement =
                            if (*(*opt).csi).supplement > (*(*font).csi).supplement {
                                (*(*opt).csi).supplement
                            } else {
                                (*(*font).csi).supplement
                            }
                    }
                    break;
                }
            } else if CIDFont_is_BaseFont(font) {
                (*opt).embed = 0i32;
                break;
            }
        }
        font_id += 1
    }
    if font_id < (*__cache).num && !cmap_csi.is_null() {
        if strcmp((*(*font).csi).registry, (*cmap_csi).registry) != 0
            || strcmp((*(*font).csi).ordering, (*cmap_csi).ordering) != 0
        {
            panic!(
                "{}: Incompatible CMap for CIDFont \"{}\"",
                "CIDFont",
                CStr::from_ptr(map_name).display(),
            );
        }
    }
    if font_id == (*__cache).num {
        font = CIDFont_new();
        if CIDFont_type0_open(font, map_name, cmap_csi, opt, 0i32) < 0i32
            && CIDFont_type2_open(font, map_name, cmap_csi, opt) < 0i32
            && CIDFont_type0_open(font, map_name, cmap_csi, opt, 1i32 << 8i32) < 0i32
            && CIDFont_type0_open(font, map_name, cmap_csi, opt, 1i32 << 9i32) < 0i32
            && CIDFont_base_open(font, map_name, cmap_csi, opt) < 0i32
        {
            CIDFont_release(font);
            release_opt(opt);
            return -1i32;
        } else {
            if (*__cache).num >= (*__cache).max {
                (*__cache).max = ((*__cache).max as u32).wrapping_add(16u32) as i32 as i32;
                (*__cache).fonts = renew(
                    (*__cache).fonts as *mut libc::c_void,
                    ((*__cache).max as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<*mut CIDFont>() as u64)
                        as u32,
                ) as *mut *mut CIDFont
            }
            (*font).name = new(
                (strlen(map_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
            ) as *mut i8;
            strcpy((*font).name, map_name);
            (*font).ident = new(
                (strlen(map_name).wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _
            ) as *mut i8;
            strcpy((*font).ident, map_name);
            (*font).options = opt;
            let ref mut fresh0 = *(*__cache).fonts.offset(font_id as isize);
            *fresh0 = font;
            (*__cache).num += 1;
            (*fmap_opt).cff_charsets = (*opt).cff_charsets
        }
    } else if !opt.is_null() {
        release_opt(opt);
    }
    font_id
}
/* FIXME */
/* Converted from Type 1 */
/* FIXME */
/* FIXME */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_cache_close() {
    if !__cache.is_null() {
        for font_id in 0..(*__cache).num {
            let font = *(*__cache).fonts.offset(font_id as isize);
            if __verbose != 0 {
                info!("(CID");
            }
            CIDFont_dofont(font);
            CIDFont_flush(font);
            CIDFont_release(font);
            free(font as *mut libc::c_void);
            if __verbose != 0 {
                info!(")");
            }
        }
        free((*__cache).fonts as *mut libc::c_void);
        __cache = mfree(__cache as *mut libc::c_void) as *mut FontCache
    };
}
/* ****************************** OPTIONS *******************************/
/*
 * FORMAT:
 *
 *   (:int:)?!?string(/string)?(,string)?
 */
unsafe fn release_opt(mut opt: *mut cid_opt) {
    if !(*opt).csi.is_null() {
        free((*(*opt).csi).registry as *mut libc::c_void);
        free((*(*opt).csi).ordering as *mut libc::c_void);
        free((*opt).csi as *mut libc::c_void);
        if !(*opt).cff_charsets.is_null() {
            cff_release_charsets((*opt).cff_charsets as *mut cff_charsets);
        }
    }
    free(opt as *mut libc::c_void);
}
unsafe fn get_cidsysinfo(
    mut map_name: *const i8,
    mut fmap_opt: *mut fontmap_opt,
) -> *mut CIDSysInfo {
    let mut csi: *mut CIDSysInfo = 0 as *mut CIDSysInfo;
    let mut csi_idx: i32 = -1i32;
    let pdf_ver = pdf_get_version() as i32;
    if fmap_opt.is_null() || (*fmap_opt).charcoll.is_null() {
        return 0 as *mut CIDSysInfo;
    }
    /* First try alias for standard one. */
    let mut i = 0; /* Use heighest supported value for current output PDF version. */
    while !CIDFont_stdcc_alias[i].name.is_null() {
        let n = strlen(CIDFont_stdcc_alias[i].name) as u64;
        if !strstartswith((*fmap_opt).charcoll, CIDFont_stdcc_alias[i].name).is_null() {
            csi_idx = CIDFont_stdcc_alias[i].index;
            csi = new((1_u64).wrapping_mul(::std::mem::size_of::<CIDSysInfo>() as u64) as u32)
                as *mut CIDSysInfo;
            (*csi).registry = new((strlen(CIDFont_stdcc_def[csi_idx as usize].registry)
                .wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
            strcpy(
                (*csi).registry,
                CIDFont_stdcc_def[csi_idx as usize].registry,
            );
            (*csi).ordering = new((strlen(CIDFont_stdcc_def[csi_idx as usize].ordering)
                .wrapping_add(1))
            .wrapping_mul(::std::mem::size_of::<i8>()) as _)
                as *mut i8;
            strcpy(
                (*csi).ordering,
                CIDFont_stdcc_def[csi_idx as usize].ordering,
            );
            if strlen((*fmap_opt).charcoll) > n as usize {
                (*csi).supplement = strtoul(
                    &mut *(*fmap_opt).charcoll.offset(n as isize),
                    0 as *mut *mut i8,
                    10i32,
                ) as i32
            } else {
                (*csi).supplement = CIDFont_stdcc_def[csi_idx as usize].supplement[pdf_ver as usize]
            }
            break;
        } else {
            i += 1
        }
    }
    if csi.is_null() {
        //let mut p = (*fmap_opt).charcoll; TODO: check
        csi = new((1_u64).wrapping_mul(::std::mem::size_of::<CIDSysInfo>() as u64) as u32)
            as *mut CIDSysInfo;
        /* Full REGISTRY-ORDERING-SUPPLEMENT */
        let mut p = strchr((*fmap_opt).charcoll, '-' as i32);
        if p.is_null() || *p.offset(1) as i32 == '\u{0}' as i32 {
            panic!(
                "{}: String can\'t be converted to REGISTRY-ORDERING-SUPPLEMENT: {}",
                "CIDFont",
                CStr::from_ptr((*fmap_opt).charcoll).display(),
            );
        }
        p = p.offset(1);
        let mut q = strchr(p, '-' as i32);
        if q.is_null() || *q.offset(1) as i32 == '\u{0}' as i32 {
            panic!(
                "{}: String can\'t be converted to REGISTRY-ORDERING-SUPPLEMENT: {}",
                "CIDFont",
                CStr::from_ptr((*fmap_opt).charcoll).display(),
            );
        }
        q = q.offset(1);
        if !(*q.offset(0) as u8).is_ascii_digit() {
            panic!(
                "{}: String can\'t be converted to REGISTRY-ORDERING-SUPPLEMENT: {}",
                "CIDFont",
                CStr::from_ptr((*fmap_opt).charcoll).display(),
            );
        }
        let n = strlen((*fmap_opt).charcoll)
            .wrapping_sub(strlen(p))
            .wrapping_sub(1) as u64;
        (*csi).registry = new((n.wrapping_add(1i32 as u64) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
            as *mut i8;
        memcpy(
            (*csi).registry as *mut libc::c_void,
            (*fmap_opt).charcoll as *const libc::c_void,
            n as _,
        );
        *(*csi).registry.offset(n as isize) = '\u{0}' as i32 as i8;
        let m = strlen(p).wrapping_sub(strlen(q)).wrapping_sub(1) as i32;
        (*csi).ordering =
            new(((m + 1i32) as u32 as u64).wrapping_mul(::std::mem::size_of::<i8>() as u64) as u32)
                as *mut i8;
        memcpy(
            (*csi).ordering as *mut libc::c_void,
            p as *const libc::c_void,
            m as _,
        );
        *(*csi).ordering.offset(m as isize) = '\u{0}' as i32 as i8;
        (*csi).supplement = strtoul(q, 0 as *mut *mut i8, 10i32) as i32;
        /* Check for standart character collections. */
        let mut i = 0;
        while !CIDFont_stdcc_def[i].ordering.is_null() {
            if !CIDFont_stdcc_def[i].registry.is_null()
                && streq_ptr((*csi).registry, CIDFont_stdcc_def[i].registry) as i32 != 0
                && streq_ptr((*csi).ordering, CIDFont_stdcc_def[i].ordering) as i32 != 0
            {
                csi_idx = i as i32;
                break;
            } else {
                i += 1
            }
        }
    }
    if !csi.is_null() && csi_idx >= 0i32 {
        if (*csi).supplement > CIDFont_stdcc_def[csi_idx as usize].supplement[pdf_ver as usize]
            && (*fmap_opt).flags & 1i32 << 1i32 != 0
        {
            warn!(
                "{}: Heighest supplement number supported in PDF-1.{} for {}-{} is {}.",
                "CIDFont",
                pdf_ver,
                CStr::from_ptr((*csi).registry).display(),
                CStr::from_ptr((*csi).ordering).display(),
                CIDFont_stdcc_def[csi_idx as usize].supplement[pdf_ver as usize],
            );
            warn!(
                "{}: Some character may not shown without embedded font (--> {}).",
                "CIDFont",
                CStr::from_ptr(map_name).display(),
            );
        }
    }
    csi
}
/* CIDFont types */
#[no_mangle]
pub unsafe extern "C" fn CIDFont_set_flags(mut flags: i32) {
    CIDFont_type0_set_flags(flags);
    CIDFont_type2_set_flags(flags);
    cidoptflags |= flags;
}
