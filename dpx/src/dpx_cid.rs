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
use std::ptr;
use std::rc::Rc;

use crate::dpx_pdfobj::IntoObj;
use crate::dpx_pdfparse::ParsePdfObj;
use crate::{info, warn};

use super::dpx_cidtype0::{
    CIDFont_type0_dofont, CIDFont_type0_open, CIDFont_type0_set_flags, CIDFont_type0_set_verbose,
    CIDFont_type0_t1cdofont, CIDFont_type0_t1dofont,
};
use super::dpx_cidtype2::{
    CIDFont_type2_dofont, CIDFont_type2_open, CIDFont_type2_set_flags, CIDFont_type2_set_verbose,
};
use crate::dpx_pdfobj::{pdf_get_version, pdf_link_obj, pdf_name, pdf_obj, pdf_ref_obj, Object};
use std::borrow::Cow;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CidFont {
    Type0 = 1,
    Type2 = 2,
}

#[derive(Clone)]
pub(crate) struct CIDSysInfo {
    pub(crate) registry: Cow<'static, str>,
    pub(crate) ordering: Cow<'static, str>,
    pub(crate) supplement: i32,
}
#[derive(Clone)]
pub(crate) struct CIDFont {
    pub(crate) ident: String,
    pub(crate) name: String,
    pub(crate) fontname: String,
    pub(crate) subtype: CidFont,
    pub(crate) flags: i32,
    pub(crate) parent: [i32; 2],
    pub(crate) csi: Box<CIDSysInfo>,
    pub(crate) options: Box<cid_opt>,
    pub(crate) indirect: *mut pdf_obj,
    pub(crate) fontdict: *mut pdf_obj,
    pub(crate) descriptor: *mut pdf_obj,
}
#[derive(Clone)]
pub(crate) struct cid_opt {
    pub(crate) name: String,
    pub(crate) csi: Option<Box<CIDSysInfo>>,
    pub(crate) index: i32,
    pub(crate) style: i32,
    pub(crate) embed: i32,
    pub(crate) stemv: i32,
    pub(crate) cff_charsets: Option<Rc<Charsets>>,
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
pub(crate) struct C2RustUnnamed_0 {
    pub(crate) registry: &'static str,
    pub(crate) ordering: &'static str,
    pub(crate) supplement: [i32; 16],
}
use super::dpx_cff::Charsets;
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
pub(crate) struct C2RustUnnamed_2 {
    pub(crate) fontname: &'static str,
    pub(crate) fontdict: &'static str,
    pub(crate) descriptor: &'static str,
}
/*
 * Optional supplement after alias name.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed_3 {
    pub(crate) name: &'static str,
    pub(crate) index: i32,
}
static mut CIDFont_stdcc_def: [C2RustUnnamed_0; 6] = [
    C2RustUnnamed_0 {
        registry: "Adobe",
        ordering: "UCS",
        supplement: [-1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    },
    C2RustUnnamed_0 {
        registry: "Adobe",
        ordering: "GB1",
        supplement: [-1, -1, 0, 2, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0],
    },
    C2RustUnnamed_0 {
        registry: "Adobe",
        ordering: "CNS1",
        supplement: [-1, -1, 0, 0, 3, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0],
    },
    C2RustUnnamed_0 {
        registry: "Adobe",
        ordering: "Japan1",
        supplement: [-1, -1, 2, 2, 4, 5, 6, 6, 0, 0, 0, 0, 0, 0, 0, 0],
    },
    C2RustUnnamed_0 {
        registry: "Adobe",
        ordering: "Korea1",
        supplement: [-1, -1, 1, 1, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0],
    },
    C2RustUnnamed_0 {
        registry: "Adobe",
        ordering: "Identity",
        supplement: [-1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    },
];

pub(crate) static mut CSI_IDENTITY: CIDSysInfo = unsafe {
    CIDSysInfo {
        registry: Cow::Borrowed("Adobe"),
        ordering: Cow::Borrowed("Identity"),
        supplement: 0,
    }
};

pub(crate) static mut CSI_UNICODE: CIDSysInfo = unsafe {
    CIDSysInfo {
        registry: Cow::Borrowed("Adobe"),
        ordering: Cow::Borrowed("UCS"),
        supplement: 0,
    }
};
static mut CIDFont_stdcc_alias: [C2RustUnnamed_3; 18] = [
    C2RustUnnamed_3 {
        name: "AU",
        index: 0,
    },
    C2RustUnnamed_3 {
        name: "AG1",
        index: 1,
    },
    C2RustUnnamed_3 {
        name: "AC1",
        index: 2,
    },
    C2RustUnnamed_3 {
        name: "AJ1",
        index: 3,
    },
    C2RustUnnamed_3 {
        name: "AK1",
        index: 4,
    },
    C2RustUnnamed_3 {
        name: "AI",
        index: 5,
    },
    C2RustUnnamed_3 {
        name: "UCS",
        index: 0,
    },
    C2RustUnnamed_3 {
        name: "GB1",
        index: 1,
    },
    C2RustUnnamed_3 {
        name: "CNS1",
        index: 2,
    },
    C2RustUnnamed_3 {
        name: "Japan1",
        index: 3,
    },
    C2RustUnnamed_3 {
        name: "Korea1",
        index: 4,
    },
    C2RustUnnamed_3 {
        name: "Identity",
        index: 5,
    },
    C2RustUnnamed_3 {
        name: "U",
        index: 0,
    },
    C2RustUnnamed_3 {
        name: "G",
        index: 1,
    },
    C2RustUnnamed_3 {
        name: "C",
        index: 2,
    },
    C2RustUnnamed_3 {
        name: "J",
        index: 3,
    },
    C2RustUnnamed_3 {
        name: "K",
        index: 4,
    },
    C2RustUnnamed_3 {
        name: "I",
        index: 5,
    },
];
static mut __verbose: i32 = 0;
static mut cidoptflags: i32 = 0;

pub(crate) unsafe fn CIDFont_set_verbose(level: i32) {
    CIDFont_type0_set_verbose(level);
    CIDFont_type2_set_verbose(level);
    __verbose = level;
}

/* It does write PDF objects. */
unsafe fn CIDFont_flush(font: &mut CIDFont) {
    crate::release!(font.indirect);
    font.indirect = ptr::null_mut();
    crate::release2!(font.fontdict);
    font.fontdict = ptr::null_mut();
    crate::release2!(font.descriptor);
    font.descriptor = ptr::null_mut();
}
impl Drop for CIDFont {
    fn drop(&mut self) {
        unsafe {
            if !self.indirect.is_null() {
                panic!("CIDFont: Object not flushed.");
            }
            if !self.fontdict.is_null() {
                panic!("CIDFont: Object not flushed.");
            }
            if !self.descriptor.is_null() {
                panic!("CIDFont: Object not flushed.");
            }
        };
    }
}

pub(crate) unsafe fn CIDFont_get_opt_index(font: &CIDFont) -> i32 {
    font.options.index
}

pub(crate) unsafe fn CIDFont_get_subtype(font: &CIDFont) -> CidFont {
    font.subtype
}

pub(crate) unsafe fn CIDFont_get_embedding(font: &CIDFont) -> i32 {
    font.options.embed
}

pub(crate) unsafe fn CIDFont_get_CIDSysInfo(font: &CIDFont) -> &CIDSysInfo {
    &font.csi
}
/*
 * Returns ID of parent Type0 font
 *  wmode: 0 for horizontal, 1 for vertical
 */

pub(crate) unsafe fn CIDFont_get_parent_id(font: &CIDFont, wmode: i32) -> i32 {
    if wmode < 0 || wmode > 1 {
        panic!("{}: Invalid wmode value.", "CIDFont");
    }
    font.parent[wmode as usize]
}

pub(crate) unsafe fn CIDFont_get_resource(font: &mut CIDFont) -> *mut pdf_obj {
    if font.indirect.is_null() {
        font.indirect = pdf_ref_obj(font.fontdict)
    }
    pdf_link_obj(font.indirect)
}
/*
 * Set parent Type0 font.
 */

pub(crate) unsafe fn CIDFont_attach_parent(font: &mut CIDFont, parent_id: usize, wmode: i32) {
    if wmode < 0 || wmode > 1 {
        panic!("{}: Invalid wmode value.", "CIDFont");
    }
    if font.parent[wmode as usize] >= 0 {
        warn!("{}: CIDFont already have a parent Type1 font.", "CIDFont");
    }
    font.parent[wmode as usize] = parent_id as i32;
}

pub(crate) unsafe fn CIDFont_is_ACCFont(font: &CIDFont) -> bool {
    for i in 1..=4 {
        if font.csi.registry == CIDFont_stdcc_def[i as usize].registry
            && font.csi.ordering == CIDFont_stdcc_def[i as usize].ordering
        {
            return true;
        }
    }
    false
}

pub(crate) unsafe fn CIDFont_is_UCSFont(font: &CIDFont) -> bool {
    (*font.csi).ordering == "UCS" || (*font.csi).ordering == "UCS2"
}
/* FIXME */

pub(crate) unsafe fn CIDFont_get_flag(font: &CIDFont, mask: i32) -> bool {
    (*font).flags & mask != 0
}
unsafe fn CIDFont_dofont(font: &mut CIDFont) {
    if font.indirect.is_null() {
        return;
    }
    if __verbose != 0 {
        info!(":{}", font.ident);
    }
    if __verbose > 1 {
        if !font.fontname.is_empty() {
            info!("[{}]", font.fontname);
        }
    }
    match font.subtype {
        CidFont::Type0 => {
            if __verbose != 0 {
                info!("[CIDFontType0]");
            }
            if CIDFont_get_flag(font, 1 << 8) {
                CIDFont_type0_t1dofont(font);
            } else if CIDFont_get_flag(font, 1 << 9) {
                CIDFont_type0_t1cdofont(font);
            } else {
                CIDFont_type0_dofont(font);
            }
        }
        CidFont::Type2 => {
            if __verbose != 0 {
                info!("[CIDFontType2]");
            }
            CIDFont_type2_dofont(font);
        }
    };
}
/*
 *
 */

pub(crate) unsafe fn CIDFont_is_BaseFont(font: &CIDFont) -> bool {
    font.flags & 1 << 0 != 0
}
static mut cid_basefont: [C2RustUnnamed_2; 20] = [
    C2RustUnnamed_2{
        fontname: "Ryumin-Light",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>",
        descriptor:"<< /CapHeight 709 /Ascent 723 /Descent -241 /StemV 69 /FontBBox [-170 -331 1024 903] /ItalicAngle 0 /Flags 6 /Style << /Panose <010502020300000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "GothicBBB-Medium",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo <<  /Registry (Adobe) /Ordering (Japan1) /Supplement 2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>",
        descriptor: "<< /CapHeight 737 /Ascent 752 /Descent -271 /StemV 99 /FontBBox [-174 -268 1001 944] /ItalicAngle 0 /Flags 4 /Style << /Panose <0801020b0500000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "MHei-Medium-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 0 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-45 -250 1015 887] /ItalicAngle 0 /Flags 4 /XHeight 553 /Style << /Panose <000001000600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "MSung-Light-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 0 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-160 -249 1015 888] /ItalicAngle 0 /Flags 6 /XHeight 553 /Style << /Panose <000000000400000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "STSong-Light-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 2 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-25 -254 1000 880] /ItalicAngle 0 /Flags 6 /XHeight 599 /Style << /Panose <000000000400000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "STHeiti-Regular-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 1 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-34 -250 1000 882] /ItalicAngle 0 /Flags 4 /XHeight 599 /Style << /Panose <000001000600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "HeiseiKakuGo-W5-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement  2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -221 /StemV 114 /FontBBox [-92 -250 1010 922] /ItalicAngle 0 /Flags 4 /XHeight 553 /Style << /Panose <0801020b0600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "HeiseiMin-W3-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 2 >> /DW 1000 /W [  231   632 500  8718 [500 500] ]>>",
        descriptor: "<< /Ascent 723 /CapHeight 709 /Descent -241 /StemV 69 /FontBBox [-123 -257 1001 910] /ItalicAngle 0 /Flags 6 /XHeight 450 /Style << /Panose <010502020400000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "HYGoThic-Medium-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 1 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-6 -145 1003 880] /ItalicAngle 0 /Flags 4 /XHeight 553 /Style << /Panose <000001000600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "HYSMyeongJo-Medium-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 1 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>",
        descriptor: "<< /Ascent 752 /CapHeight 737 /Descent -271 /StemV 58 /FontBBox [-0 -148 1001 880] /ItalicAngle 0 /Flags 6 /XHeight 553 /Style << /Panose <000000000600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "MSungStd-Light-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 4 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>",
        descriptor: "<< /Ascent 880 /CapHeight 662 /Descent -120 /StemV 54 /FontBBox [-160 -249 1015 1071] /ItalicAngle 0 /Flags 6 /Style << /Panose <000000000400000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname:"STSongStd-Light-Acro",
        fontdict:"<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 4 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>",
        descriptor: "<< /Ascent 880 /CapHeight 626 /Descent -120 /StemV 44 /FontBBox [-134 -254 1001 905] /ItalicAngle 0 /Flags 6 /Style << /Panose <000000000400000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "HYSMyeongJoStd-Medium-Acro",
        fontdict: "<< /Subtype /CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 2 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>",
        descriptor: "<< /Ascent 880 /CapHeight 720 /Descent -120 /StemV 60 /FontBBox [-28 -148 1001 880] /ItalicAngle 0 /Flags 6 /Style << /Panose <000000000600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "AdobeMingStd-Light-Acro",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (CNS1) /Supplement 4 >> /DW 1000 /W [13648 13742 500 17603 [500] ]>>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 48 /CapHeight 731 /FontBBox [-38 -121 1002 918] /ItalicAngle 0 /Flags 6 /XHeight 466 /Style << /Panose <000002020300000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "AdobeSongStd-Light-Acro",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 4 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 66 /CapHeight 626 /FontBBox [-134 -254 1001 905] /ItalicAngle 0 /Flags 6 /XHeight 416 /Style << /Panose <000002020300000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "KozMinPro-Regular-Acro",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 4 >> /DW 1000 /W [  231   632 500  8718 [500 500]  9738  9757 250  9758  9778 333 12063 12087 500 ]>>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 86 /CapHeight 740 /FontBBox [-195 -272 1110 1075] /ItalicAngle 0 /Flags 6 /XHeight 502 /Style << /Panose <000002020400000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "KozGoPro-Medium-Acro",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering(Japan1) /Supplement 4 >> /DW 1000 /W [  231   632 500  8718 [500 500]  9738  9757 250  9758  9778 333 12063 12087 500 ]>>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 99 /CapHeight 763 /FontBBox [-149 -374 1254 1008] /ItalicAngle 0 /Flags 4 /XHeight 549 /Style << /Panose <0000020b0700000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "AdobeMyungjoStd-Medium-Acro",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (Korea1) /Supplement 2 >> /DW 1000 /W [   97 [500]  8094  8190 500 ]>>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 99 /CapHeight 719 /FontBBox [-28 -148 1001 880] /ItalicAngle 0 /Flags 6 /XHeight 478 /Style << /Panose <000002020600000000000000> >> >>",
    },
    C2RustUnnamed_2{
        fontname: "KozMinProVI-Regular",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo <<   /Registry (Adobe)   /Ordering (Japan1)   /Supplement 6 >> /DW 1000 /W [  231   632 500   8718 [500 500]   9738  9757 250   9758  9778 333   12063 12087 500 ]        >>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 86 /CapHeight 742 /FontBBox [-437 -340 1144 1317] /ItalicAngle 0 /Flags 6 /XHeight 503 /Style <<   /Panose <000002020400000000000000> >>      >>",
    },
    C2RustUnnamed_2{
        fontname: "AdobeHeitiStd-Regular",
        fontdict: "<< /Subtype/CIDFontType0 /CIDSystemInfo << /Registry (Adobe) /Ordering (GB1) /Supplement 4 >> /DW 1000 /W [  814 939 500  7716 [500] 22355 [500 500] 22357 [500] ]>>",
        descriptor: "<< /Ascent 880 /Descent -120 /StemV 66 /CapHeight 626 /FontBBox [-134 -254 1001 905] /ItalicAngle 0 /Flags 6 /XHeight 416 /Style << /Panose <000002020300000000000000> >> >>",
    }
];
unsafe fn CIDFont_base_open(
    name: &str,
    cmap_csi: &Option<CIDSysInfo>,
    mut opt: Box<cid_opt>,
) -> Result<Box<CIDFont>, Box<cid_opt>> {
    let basefont = cid_basefont.iter().find(|font| {
        font.fontname == name || {
            let len = name.len();
            font.fontname.len() > len
                && &font.fontname[..len] == name
                && &font.fontname[len..] == "-Acro"
        }
    });

    if basefont.is_none() {
        return Err(opt);
    }

    let basefont = basefont.unwrap();

    let fontname = name.to_string()
        + match opt.style {
            1 => ",Bold",
            2 => ",Italic",
            3 => ",BoldItalic",
            _ => "",
        };
    let mut start = basefont.fontdict.as_bytes();
    let mut fontdict = start.parse_pdf_dict(ptr::null_mut()).unwrap();
    let mut start = basefont.descriptor.as_bytes();
    let mut descriptor = start.parse_pdf_dict(ptr::null_mut()).unwrap();
    let csi = if let Object::Dict(tmp) = &fontdict.get("CIDSystemInfo").unwrap().data {
        let registry = std::str::from_utf8(tmp.get("Registry").unwrap().as_string().to_bytes())
            .unwrap()
            .to_string();
        let ordering = std::str::from_utf8(tmp.get("Ordering").unwrap().as_string().to_bytes())
            .unwrap()
            .to_string();
        let supplement = tmp.get("Supplement").unwrap().as_f64() as i32;
        if let Some(cmap_csi) = cmap_csi.as_ref() {
            /* NULL for accept any */
            if registry != cmap_csi.registry || ordering != cmap_csi.ordering {
                panic!(
                    "Inconsistent CMap used for CID-keyed font {}.",
                    basefont.fontname
                );
            } else {
                if supplement < cmap_csi.supplement {
                    warn!(
                        "CMap has higher supplement number than CIDFont: {}",
                        fontname,
                    );
                    warn!("Some chracters may not be displayed or printed.");
                }
            }
        }
        Box::new(CIDSysInfo {
            registry: registry.into(),
            ordering: ordering.into(),
            supplement,
        })
    } else {
        panic!();
    };
    match fontdict.get("Subtype") {
        Some(pdf_obj {
            data: Object::Name(typ),
            ..
        }) => {
            let typ = typ.to_bytes();
            let subtype = if typ == b"CIDFontType0" {
                CidFont::Type0
            } else if typ == b"CIDFontType2" {
                CidFont::Type2
            } else {
                panic!("Unknown CIDFontType \"{}\"", typ.display());
            };
            if cidoptflags & 1 << 1 != 0 {
                if fontdict.has("W") {
                    fontdict.remove("W");
                }
                if fontdict.has("W2") {
                    fontdict.remove("W2");
                }
            }
            fontdict.set("Type", "Font");
            fontdict.set("BaseFont", pdf_name::new(fontname.as_bytes()));
            descriptor.set("Type", "FontDescriptor");
            descriptor.set("FontName", pdf_name::new(fontname.as_bytes()));
            opt.embed = 0;
            Ok(Box::new(CIDFont {
                ident: name.to_string(),
                name: name.to_string(),
                fontname,
                subtype,
                flags: 1 << 0,
                parent: [-1, -1],
                csi,
                options: opt,
                indirect: ptr::null_mut(),
                fontdict: fontdict.into_obj(),
                descriptor: descriptor.into_obj(),
            }))
        }
        _ => panic!(),
    }
}

// Note: The elements are boxed to be able
// to get stable pointers to the cached data.
static mut __cache: Vec<Box<CIDFont>> = Vec::new();

pub(crate) unsafe fn CIDFont_cache_get(font_id: usize) -> &'static mut CIDFont {
    assert!(
        font_id < __cache.len(),
        "{}: Invalid ID {}",
        "CIDFont",
        font_id
    );
    &mut *__cache[font_id]
}
/*
 * cmap_csi is NULL if CMap is Identity.
 */

pub(crate) unsafe fn CIDFont_cache_find(
    map_name: &str,
    cmap_csi: Option<CIDSysInfo>,
    fmap_opt: &mut fontmap_opt,
) -> Option<usize> {
    let mut opt = Box::new(cid_opt {
        style: fmap_opt.style,
        index: fmap_opt.index,
        embed: if fmap_opt.flags & 1 << 1 != 0 { 0 } else { 1 },
        name: String::new(),
        csi: get_cidsysinfo(map_name, fmap_opt),
        stemv: fmap_opt.stemv,
        cff_charsets: None,
    });

    if opt.csi.is_none() {
        if let Some(cmap_csi) = cmap_csi.as_ref() {
            /*
             * No CIDSystemInfo supplied explicitly. Copy from CMap's one if available.
             * It is not neccesary for CID-keyed fonts. But TrueType requires them.
             */
            opt.csi = Some(Box::new(cmap_csi.clone()));
        }
    }
    /*
     * Here, we do not compare font->ident and map_name because of
     * implicit CIDSystemInfo supplied by CMap for TrueType.
     */
    let mut font_id = 0;
    while font_id < __cache.len() {
        let font = &mut *__cache[font_id];
        if font.name == map_name
            && (*font.options).style == opt.style
            && (*font.options).index == opt.index
        {
            if (*font.options).embed == opt.embed {
                /*
                 * Case 1: CSI not available (Identity CMap)
                 *         Font is TrueType --> continue
                 *         Font is CIDFont  --> break
                 * Case 2: CSI matched      --> break
                 */
                if let Some(opt_csi) = opt.csi.as_deref() {
                    if ((*font.csi).registry == opt_csi.registry)
                        && ((*font.csi).ordering == opt_csi.ordering)
                    {
                        if font.subtype == CidFont::Type2 {
                            (*font.csi).supplement = if opt_csi.supplement > (*font.csi).supplement
                            {
                                opt_csi.supplement
                            } else {
                                (*font.csi).supplement
                            }
                        }
                        break;
                    }
                } else {
                    if font.subtype != CidFont::Type2 {
                        break;
                    }
                }
            } else if CIDFont_is_BaseFont(font) {
                opt.embed = 0;
                break;
            }
        }
        font_id += 1
    }
    if font_id < __cache.len() {
        if let Some(cmap_csi) = cmap_csi.as_ref() {
            let font = &mut __cache[font_id];
            if font.csi.registry != cmap_csi.registry || (font.csi.ordering != cmap_csi.ordering) {
                panic!("CIDFont: Incompatible CMap for CIDFont \"{}\"", map_name);
            }
        }
    }
    if font_id == __cache.len() {
        if let Ok(font) = CIDFont_type0_open(map_name, &cmap_csi, opt, 0)
            .or_else(|opt| CIDFont_type2_open(map_name, &cmap_csi, opt))
            .or_else(|opt| CIDFont_type0_open(map_name, &cmap_csi, opt, 1 << 8))
            .or_else(|opt| CIDFont_type0_open(map_name, &cmap_csi, opt, 1 << 9))
            .or_else(|opt| CIDFont_base_open(map_name, &cmap_csi, opt))
        {
            fmap_opt.cff_charsets = font.options.cff_charsets.clone();

            __cache.push(font);
        } else {
            return None;
        }
    }
    Some(font_id)
}
/* FIXME */
/* Converted from Type 1 */
/* FIXME */
/* FIXME */

pub(crate) unsafe fn CIDFont_cache_close() {
    for font_id in 0..__cache.len() as i32 {
        let font = &mut *__cache[font_id as usize];
        if __verbose != 0 {
            info!("(CID");
        }
        CIDFont_dofont(font);
        CIDFont_flush(font);
        if __verbose != 0 {
            info!(")");
        }
    }
    __cache.clear();
}
/* ****************************** OPTIONS *******************************/
/*
 * FORMAT:
 *
 *   (:int:)?!?string(/string)?(,string)?
 */
unsafe fn get_cidsysinfo(map_name: &str, fmap_opt: *mut fontmap_opt) -> Option<Box<CIDSysInfo>> {
    let mut csi = None;
    let mut csi_idx: i32 = -1;
    let pdf_ver = pdf_get_version() as i32;
    /* Use heighest supported value for current output PDF version. */

    if fmap_opt.is_null() || (*fmap_opt).charcoll.is_empty() {
        return None;
    }
    /* First try alias for standard one. */
    for alias in &CIDFont_stdcc_alias {
        let n = alias.name.len();
        if (*fmap_opt).charcoll.starts_with(alias.name) {
            csi_idx = alias.index;
            csi = Some(Box::new(CIDSysInfo {
                registry: Cow::Borrowed(CIDFont_stdcc_def[csi_idx as usize].registry),
                ordering: Cow::Borrowed(CIDFont_stdcc_def[csi_idx as usize].ordering),
                supplement: if (*fmap_opt).charcoll.len() > n as usize {
                    (*fmap_opt).charcoll[n..].parse::<i32>().unwrap_or(0)
                } else {
                    CIDFont_stdcc_def[csi_idx as usize].supplement[pdf_ver as usize]
                },
            }));
            break;
        }
    }
    let csi = csi.unwrap_or_else(|| {
        /* Full REGISTRY-ORDERING-SUPPLEMENT */
        let charcoll_parts = (*fmap_opt).charcoll.split("-").collect::<Vec<_>>();
        assert_eq!(
            charcoll_parts.len(),
            3,
            "CIDFont: String can\'t be converted to REGISTRY-ORDERING-SUPPLEMENT",
        );

        let csi = Box::new(CIDSysInfo {
            registry: charcoll_parts[0].to_string().into(),
            ordering: charcoll_parts[1].to_string().into(),
            supplement: charcoll_parts[2].parse::<i32>().unwrap_or(0),
        });

        /* Check for standart character collections. */
        for (i, def) in CIDFont_stdcc_def.iter().enumerate() {
            if csi.registry == def.registry && csi.ordering == def.ordering {
                csi_idx = i as i32;
            }
        }
        csi
    });
    if csi_idx >= 0 {
        if csi.supplement > CIDFont_stdcc_def[csi_idx as usize].supplement[pdf_ver as usize]
            && (*fmap_opt).flags & 1 << 1 != 0
        {
            warn!(
                "CIDFont: Heighest supplement number supported in PDF-1.{} for {}-{} is {}.",
                pdf_ver,
                csi.registry,
                csi.ordering,
                CIDFont_stdcc_def[csi_idx as usize].supplement[pdf_ver as usize],
            );
            warn!(
                "CIDFont: Some character may not shown without embedded font (--> {}).",
                map_name
            );
        }
    }
    Some(csi)
}
/* CIDFont types */

pub(crate) unsafe fn CIDFont_set_flags(flags: i32) {
    CIDFont_type0_set_flags(flags);
    CIDFont_type2_set_flags(flags);
    cidoptflags |= flags;
}
