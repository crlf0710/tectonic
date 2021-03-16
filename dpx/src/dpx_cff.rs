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
use std::rc::Rc;

use super::dpx_cff_dict::{cff_dict, cff_dict_unpack};
use super::dpx_mem::{new, renew};
use super::dpx_numbers::GetFromFile;
use libc::{free, memmove, memset};

use crate::bridge::InFile;
use std::io::{Read, Seek, SeekFrom};
use std::ptr;

/* CFF Data Types */
/* SID SID number */
/* offset(0) */
/* size offset(0) */
pub(crate) type c_offsize = u8;
/* 1-byte unsigned number specifies the size
of an Offset field or fields, range 1-4 */
pub(crate) type l_offset = u32;
/* 1, 2, 3, or 4-byte offset */
pub(crate) type s_SID = u16;
/* 2-byte string identifier  */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_index {
    pub(crate) count: u16,
    pub(crate) offsize: c_offsize,
    pub(crate) offset: *mut l_offset,
    pub(crate) data: *mut u8,
    /* Object data                       */
}

pub(crate) trait Pack {
    fn pack(&mut self, dest: &mut [u8]) -> usize;
}

/// Rewrittened cff_index
#[derive(Clone)]
pub(crate) struct CffIndex {
    pub(crate) count: u16, // ??
    pub(crate) offsize: c_offsize,
    pub(crate) offset: Vec<l_offset>,
    pub(crate) data: Vec<u8>,
}
impl CffIndex {
    // cff_index_new
    pub(crate) fn new(count: u16) -> Box<Self> {
        let offset = if count > 0 {
            let mut offset = vec![0; count as usize + 1];
            offset[0] = 1;
            offset
        } else {
            vec![]
        };
        Box::new(CffIndex {
            count,
            offsize: 0,
            offset,
            data: vec![],
        })
    }
    // cff_index_size
    pub(crate) fn size(&mut self) -> usize {
        if self.count > 0 {
            let datalen: l_offset = self.offset[self.count as usize] as l_offset - 1;
            self.offsize = if (datalen as u64) < 0xff {
                1
            } else if (datalen as u64) < 0xffff {
                2
            } else if (datalen as u64) < 0xffffff {
                3
            } else {
                4
            };
            (((3 + self.offsize as i32 * (self.count as i32 + 1)) as u32) + datalen) as usize
        } else {
            2
        }
    }
}

// cff_pack_index
impl Pack for CffIndex {
    fn pack(&mut self, mut dest: &mut [u8]) -> usize {
        let destlen = dest.len();
        if self.count < 1 {
            if destlen < 2 {
                panic!("Not enough space available...");
            }
            unsafe {
                memset(dest.as_mut_ptr() as *mut libc::c_void, 0, 2);
            }
            return 2;
        }
        let len = self.size();
        let datalen = (self.offset[self.count as usize] - 1) as usize;
        if destlen < len {
            panic!("Not enough space available...");
        }
        dest[0..2].copy_from_slice(&self.count.to_be_bytes());
        dest = &mut dest[2..];
        if datalen < 0xff {
            self.offsize = 1 as c_offsize;
            dest[0] = 1;
            dest = &mut dest[1..];
            for i in 0..=self.count as usize {
                dest[0] = self.offset[i] as u8;
                dest = &mut dest[1..];
            }
        } else if datalen < 0xffff {
            self.offsize = 2 as c_offsize;
            dest[0] = 2;
            dest = &mut dest[1..];
            for i in 0..=self.count as usize {
                dest[0..2].copy_from_slice(&(self.offset[i] as u16).to_be_bytes());
                dest = &mut dest[2..];
            }
        } else if datalen < 0xffffff {
            self.offsize = 3 as c_offsize;
            dest[0] = 3;
            dest = &mut dest[1..];
            for i in 0..=self.count as usize {
                dest[0..3].copy_from_slice(&self.offset[i].to_be_bytes()[1..4]);
                dest = &mut dest[3..];
            }
        } else {
            self.offsize = 4 as c_offsize;
            dest[0] = 4;
            dest = &mut dest[1..];
            for i in 0..=self.count as usize {
                dest[0..4].copy_from_slice(&self.offset[i].to_be_bytes());
                dest = &mut dest[4..];
            }
        }
        let slen = self.offset[self.count as usize] as usize - 1;
        dest[..slen].copy_from_slice(&self.data[..slen]);
        len
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_header {
    pub(crate) major: u8,
    pub(crate) minor: u8,
    pub(crate) hdr_size: u8,
    pub(crate) offsize: c_offsize,
    /* Absolute offset (0) size             */
}
/* Encoding, Charset and FDSelect */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_range1 {
    pub(crate) first: s_SID,
    pub(crate) n_left: u8,
    /* no. of remaining gids/codes in this range */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_range2 {
    pub(crate) first: s_SID,
    pub(crate) n_left: u16,
    /* u16-version of range1 */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_map {
    pub(crate) code: u8,
    pub(crate) glyph: s_SID,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_encoding {
    pub(crate) format: u8,
    pub(crate) num_entries: u8,
    pub(crate) data: C2RustUnnamed,
    pub(crate) num_supps: u8,
    pub(crate) supp: *mut cff_map,
    /* supplement */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) union C2RustUnnamed {
    pub(crate) codes: *mut u8,
    pub(crate) range1: *mut cff_range1,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_charsets {
    pub(crate) format: u8,
    pub(crate) num_entries: u16,
    pub(crate) data: C2RustUnnamed_0,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) union C2RustUnnamed_0 {
    pub(crate) glyphs: *mut s_SID,
    pub(crate) range1: *mut cff_range1,
    pub(crate) range2: *mut cff_range2,
}
/* CID-Keyed font specific */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_range3 {
    pub(crate) first: u16,
    pub(crate) fd: u8,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_fdselect {
    pub(crate) format: u8,
    pub(crate) num_entries: u16,
    pub(crate) data: C2RustUnnamed_1,
    /* u16 sentinel; */
    /* format 3 only, must be equals to num_glyphs */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) union C2RustUnnamed_1 {
    pub(crate) fds: *mut u8,
    pub(crate) ranges: *mut cff_range3,
}
#[repr(C)]
pub(crate) struct cff_font {
    pub(crate) fontname: String,
    pub(crate) header: cff_header,
    pub(crate) name: Box<CffIndex>,
    pub(crate) topdict: cff_dict,
    pub(crate) string: Option<Box<CffIndex>>,
    pub(crate) gsubr: Option<Box<CffIndex>>,
    pub(crate) encoding: *mut cff_encoding,
    pub(crate) charsets: *mut cff_charsets,
    pub(crate) fdselect: *mut cff_fdselect,
    pub(crate) cstrings: *mut cff_index,
    pub(crate) fdarray: Vec<Option<cff_dict>>,
    pub(crate) private: Vec<Option<cff_dict>>,
    pub(crate) subrs: Vec<Option<Box<CffIndex>>>,
    pub(crate) offset: l_offset,
    pub(crate) gsubr_offset: l_offset,
    pub(crate) num_glyphs: u16,
    pub(crate) num_fds: u8,
    pub(crate) _string: Option<Box<CffIndex>>,
    pub(crate) handle: Option<Rc<InFile>>,
    pub(crate) filter: i32,
    pub(crate) index: i32,
    pub(crate) flag: i32,
    pub(crate) is_notdef_notzero: i32,
}

static mut cff_stdstr: [&str; 391] = [
    ".notdef",
    "space",
    "exclam",
    "quotedbl",
    "numbersign",
    "dollar",
    "percent",
    "ampersand",
    "quoteright",
    "parenleft",
    "parenright",
    "asterisk",
    "plus",
    "comma",
    "hyphen",
    "period",
    "slash",
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "colon",
    "semicolon",
    "less",
    "equal",
    "greater",
    "question",
    "at",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "bracketleft",
    "backslash",
    "bracketright",
    "asciicircum",
    "underscore",
    "quoteleft",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "braceleft",
    "bar",
    "braceright",
    "asciitilde",
    "exclamdown",
    "cent",
    "sterling",
    "fraction",
    "yen",
    "florin",
    "section",
    "currency",
    "quotesingle",
    "quotedblleft",
    "guillemotleft",
    "guilsinglleft",
    "guilsinglright",
    "fi",
    "fl",
    "endash",
    "dagger",
    "daggerdbl",
    "periodcentered",
    "paragraph",
    "bullet",
    "quotesinglbase",
    "quotedblbase",
    "quotedblright",
    "guillemotright",
    "ellipsis",
    "perthousand",
    "questiondown",
    "grave",
    "acute",
    "circumflex",
    "tilde",
    "macron",
    "breve",
    "dotaccent",
    "dieresis",
    "ring",
    "cedilla",
    "hungarumlaut",
    "ogonek",
    "caron",
    "emdash",
    "AE",
    "ordfeminine",
    "Lslash",
    "Oslash",
    "OE",
    "ordmasculine",
    "ae",
    "dotlessi",
    "lslash",
    "oslash",
    "oe",
    "germandbls",
    "onesuperior",
    "logicalnot",
    "mu",
    "trademark",
    "Eth",
    "onehalf",
    "plusminus",
    "Thorn",
    "onequarter",
    "divide",
    "brokenbar",
    "degree",
    "thorn",
    "threequarters",
    "twosuperior",
    "registered",
    "minus",
    "eth",
    "multiply",
    "threesuperior",
    "copyright",
    "Aacute",
    "Acircumflex",
    "Adieresis",
    "Agrave",
    "Aring",
    "Atilde",
    "Ccedilla",
    "Eacute",
    "Ecircumflex",
    "Edieresis",
    "Egrave",
    "Iacute",
    "Icircumflex",
    "Idieresis",
    "Igrave",
    "Ntilde",
    "Oacute",
    "Ocircumflex",
    "Odieresis",
    "Ograve",
    "Otilde",
    "Scaron",
    "Uacute",
    "Ucircumflex",
    "Udieresis",
    "Ugrave",
    "Yacute",
    "Ydieresis",
    "Zcaron",
    "aacute",
    "acircumflex",
    "adieresis",
    "agrave",
    "aring",
    "atilde",
    "ccedilla",
    "eacute",
    "ecircumflex",
    "edieresis",
    "egrave",
    "iacute",
    "icircumflex",
    "idieresis",
    "igrave",
    "ntilde",
    "oacute",
    "ocircumflex",
    "odieresis",
    "ograve",
    "otilde",
    "scaron",
    "uacute",
    "ucircumflex",
    "udieresis",
    "ugrave",
    "yacute",
    "ydieresis",
    "zcaron",
    "exclamsmall",
    "Hungarumlautsmall",
    "dollaroldstyle",
    "dollarsuperior",
    "ampersandsmall",
    "Acutesmall",
    "parenleftsuperior",
    "parenrightsuperior",
    "twodotenleader",
    "onedotenleader",
    "zerooldstyle",
    "oneoldstyle",
    "twooldstyle",
    "threeoldstyle",
    "fouroldstyle",
    "fiveoldstyle",
    "sixoldstyle",
    "sevenoldstyle",
    "eightoldstyle",
    "nineoldstyle",
    "commasuperior",
    "threequartersemdash",
    "periodsuperior",
    "questionsmall",
    "asuperior",
    "bsuperior",
    "centsuperior",
    "dsuperior",
    "esuperior",
    "isuperior",
    "lsuperior",
    "msuperior",
    "nsuperior",
    "osuperior",
    "rsuperior",
    "ssuperior",
    "tsuperior",
    "ff",
    "ffi",
    "ffl",
    "parenleftinferior",
    "parenrightinferior",
    "Circumflexsmall",
    "hyphensuperior",
    "Gravesmall",
    "Asmall",
    "Bsmall",
    "Csmall",
    "Dsmall",
    "Esmall",
    "Fsmall",
    "Gsmall",
    "Hsmall",
    "Ismall",
    "Jsmall",
    "Ksmall",
    "Lsmall",
    "Msmall",
    "Nsmall",
    "Osmall",
    "Psmall",
    "Qsmall",
    "Rsmall",
    "Ssmall",
    "Tsmall",
    "Usmall",
    "Vsmall",
    "Wsmall",
    "Xsmall",
    "Ysmall",
    "Zsmall",
    "colonmonetary",
    "onefitted",
    "rupiah",
    "Tildesmall",
    "exclamdownsmall",
    "centoldstyle",
    "Lslashsmall",
    "Scaronsmall",
    "Zcaronsmall",
    "Dieresissmall",
    "Brevesmall",
    "Caronsmall",
    "Dotaccentsmall",
    "Macronsmall",
    "figuredash",
    "hypheninferior",
    "Ogoneksmall",
    "Ringsmall",
    "Cedillasmall",
    "questiondownsmall",
    "oneeighth",
    "threeeighths",
    "fiveeighths",
    "seveneighths",
    "onethird",
    "twothirds",
    "zerosuperior",
    "foursuperior",
    "fivesuperior",
    "sixsuperior",
    "sevensuperior",
    "eightsuperior",
    "ninesuperior",
    "zeroinferior",
    "oneinferior",
    "twoinferior",
    "threeinferior",
    "fourinferior",
    "fiveinferior",
    "sixinferior",
    "seveninferior",
    "eightinferior",
    "nineinferior",
    "centinferior",
    "dollarinferior",
    "periodinferior",
    "commainferior",
    "Agravesmall",
    "Aacutesmall",
    "Acircumflexsmall",
    "Atildesmall",
    "Adieresissmall",
    "Aringsmall",
    "AEsmall",
    "Ccedillasmall",
    "Egravesmall",
    "Eacutesmall",
    "Ecircumflexsmall",
    "Edieresissmall",
    "Igravesmall",
    "Iacutesmall",
    "Icircumflexsmall",
    "Idieresissmall",
    "Ethsmall",
    "Ntildesmall",
    "Ogravesmall",
    "Oacutesmall",
    "Ocircumflexsmall",
    "Otildesmall",
    "Odieresissmall",
    "OEsmall",
    "Oslashsmall",
    "Ugravesmall",
    "Uacutesmall",
    "Ucircumflexsmall",
    "Udieresissmall",
    "Yacutesmall",
    "Thornsmall",
    "Ydieresissmall",
    "001.000",
    "001.001",
    "001.002",
    "001.003",
    "Black",
    "Bold",
    "Book",
    "Light",
    "Medium",
    "Regular",
    "Roman",
    "Semibold",
];
fn get_unsigned<R: Read>(handle: &mut R, n: i32) -> u32 {
    let mut v: u32 = 0_u32;
    for _ in 0..n {
        v = v * 0x100 + (u8::get(handle) as u32)
    }
    v
}
/*
 * Read Header, Name INDEX, Top DICT INDEX, and String INDEX.
 */

pub(crate) unsafe fn cff_open(
    cff_handle: Rc<InFile>,
    mut offset: i32,
    n: i32,
) -> Option<Box<cff_font>> {
    let handle = &mut cff_handle.as_ref();
    handle.seek(SeekFrom::Start(offset as u64)).unwrap();
    let header = cff_header {
        major: u8::get(handle),
        minor: u8::get(handle),
        hdr_size: u8::get(handle),
        offsize: u8::get(handle),
    };
    if (header.offsize as i32) < 1 || header.offsize as i32 > 4 {
        panic!("invalid offsize data");
    }
    if header.major as i32 > 1 || header.minor as i32 > 0 {
        warn!(
            "{}: CFF version {}.{} not supported.",
            "CFF", header.major as i32, header.minor as i32,
        );
        return None;
    }
    let cff_offset = offset as l_offset;
    handle
        .seek(SeekFrom::Start(cff_offset as u64 + header.hdr_size as u64))
        .unwrap();
    /* Name INDEX */
    let idx = CffIndex::get(handle)?;
    if n > idx.count as i32 - 1 {
        warn!("{}: Invalid CFF fontset index number.", "CFF");
        return None;
    }
    let name = idx;
    let fontname = name.get_name(n);
    /* Top DICT INDEX */
    let idx = CffIndex::get(handle).unwrap();
    if n > idx.count as i32 - 1 {
        panic!("CFF Top DICT not exist...");
    }
    let topdict = cff_dict_unpack(
        &idx.data[idx.offset[n as usize] as usize - 1..idx.offset[(n + 1) as usize] as usize - 1],
    );

    if topdict.contains_key("CharstringType") && topdict.get("CharstringType", 0) != 2. {
        warn!("Only Type 2 Charstrings supported...");
        return None;
    }
    if topdict.contains_key("SyntheticBase") {
        warn!("CFF Synthetic font not supported.");
        return None;
    }
    /* String INDEX */
    let string = CffIndex::get(handle);
    /* offset to GSubr */
    let gsubr_offset = (handle.seek(SeekFrom::Current(0)).unwrap() - offset as u64) as l_offset;
    /* Number of glyphs */
    offset = topdict.get("CharStrings", 0) as i32;
    handle
        .seek(SeekFrom::Start(cff_offset as u64 + offset as u64))
        .unwrap();
    let num_glyphs = u16::get(handle);
    /* Check for font type */
    let mut flag = 0;
    if topdict.contains_key("ROS") {
        flag |= 1 << 0
    } else {
        flag |= 1 << 1
    }
    /* Check for encoding */
    if topdict.contains_key("Encoding") {
        offset = topdict.get("Encoding", 0) as i32;
        if offset == 0 {
            /* predefined */
            flag |= 1 << 3
        } else if offset == 1 {
            flag |= 1 << 4
        }
    } else {
        flag |= 1 << 3
    }
    /* Check for charset */
    if topdict.contains_key("charset") {
        offset = topdict.get("charset", 0) as i32;
        if offset == 0 {
            /* predefined */
            flag |= 1 << 5
        } else if offset == 1 {
            flag |= 1 << 6
        } else if offset == 2 {
            flag |= 1 << 7
        }
    } else {
        flag |= 1 << 5
    } /* seek back to GSubr */
    handle
        .seek(SeekFrom::Start(cff_offset as u64 + gsubr_offset as u64))
        .unwrap(); /* no trailing '\0' */

    Some(Box::new(cff_font {
        fontname,
        index: n,
        handle: Some(cff_handle),
        offset: cff_offset,
        filter: 0,
        flag,
        name,
        topdict,
        gsubr: None,
        encoding: ptr::null_mut(),
        charsets: ptr::null_mut(),
        fdselect: ptr::null_mut(),
        cstrings: ptr::null_mut(),
        fdarray: Vec::new(),
        private: Vec::new(),
        subrs: Vec::new(),
        num_glyphs,
        num_fds: 0,
        string,
        _string: None,
        header,
        gsubr_offset,
        is_notdef_notzero: 0,
    })) /* Additional data in between header and
         * Name INDEX ignored.
         */
}

impl Drop for cff_font {
    fn drop(&mut self) {
        unsafe {
            if !self.encoding.is_null() {
                cff_release_encoding(self.encoding);
            }
            if !self.charsets.is_null() {
                cff_release_charsets(self.charsets);
            }
            if !self.fdselect.is_null() {
                cff_release_fdselect(self.fdselect);
            }
            if !self.cstrings.is_null() {
                cff_release_index(self.cstrings);
            }
        }
    }
}

impl CffIndex {
    pub(crate) unsafe fn get_name(&self, index: i32) -> String {
        std::str::from_utf8(
            &self.data[self.offset[index as usize] as usize - 1
                ..self.offset[(index + 1) as usize] as usize - 1],
        )
        .unwrap()
        .to_string()
    }
}

pub(crate) unsafe fn cff_set_name(cff: &mut cff_font, name: &str) -> i32 {
    if name.len() > 127 {
        panic!("FontName string length too large...");
    }
    cff.name = Box::new(CffIndex {
        count: 1,
        offsize: 1,
        offset: vec![1, (name.len() + 1) as _],
        data: Vec::from(name.as_bytes()),
    });
    (5 + name.len()) as _
}

pub(crate) unsafe fn cff_put_header(cff: &cff_font, dest: &mut [u8]) -> usize {
    /* We will set all offset (0) to four-byte integer. */
    dest[0..4].copy_from_slice(&[cff.header.major, cff.header.minor, 4, 4]);
    4
}
/* Only read header part but not body */

pub(crate) unsafe fn cff_get_index_header(cff: &cff_font) -> *mut cff_index {
    let idx = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_index>() as u64) as u32)
        as *mut cff_index;
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    let count = u16::get(handle);
    (*idx).count = count;
    if count as i32 > 0 {
        (*idx).offsize = u8::get(handle);
        if ((*idx).offsize as i32) < 1 || (*idx).offsize as i32 > 4 {
            panic!("invalid offsize data");
        }
        (*idx).offset = new(((count as i32 + 1) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<l_offset>() as u64)
            as u32) as *mut l_offset;
        for i in 0..count {
            *(*idx).offset.offset(i as isize) = get_unsigned(handle, (*idx).offsize as i32);
        }
        if count as i32 == 0xffff {
            let n = handle.seek(SeekFrom::Current(0)).unwrap();
            handle
                .seek(SeekFrom::Start(n + (*idx).offsize as u64))
                .unwrap();
        } else {
            *(*idx).offset.offset(count as isize) = get_unsigned(handle, (*idx).offsize as i32)
        }
        if *(*idx).offset.offset(0) != 1_u32 {
            panic!("cff_get_index(): invalid index data");
        }
        (*idx).data = ptr::null_mut()
    } else {
        (*idx).offsize = 0 as c_offsize;
        (*idx).offset = ptr::null_mut();
        (*idx).data = ptr::null_mut()
    }
    idx
}

impl CffIndex {
    pub(crate) unsafe fn get<R: Read>(handle: &mut R) -> Option<Box<CffIndex>> {
        let count = u16::get(handle);
        if count > 0 {
            let offsize = u8::get(handle);
            if offsize < 1 || offsize > 4 {
                panic!("invalid offsize data");
            }
            let mut offset = Vec::<l_offset>::with_capacity(count as usize + 1);
            for _ in 0..count + 1 {
                offset.push(get_unsigned(handle, offsize as i32));
            }
            if offset[0] != 1 {
                panic!("Invalid CFF Index offset data");
            }
            let mut length = (offset[count as usize] - offset[0]) as i32;
            let mut data = vec![0_u8; length as usize];
            let mut offset0 = 0;
            while length > 0 {
                let nb_read = handle
                    .read(&mut data[offset0..offset0 + length as usize])
                    .unwrap() as i32;
                offset0 += nb_read as usize;
                length -= nb_read;
            }
            Some(Box::new(CffIndex {
                count,
                offsize,
                offset,
                data,
            }))
        } else {
            None
        }
    }
}

pub(crate) unsafe fn cff_get_index(cff: &cff_font) -> *mut cff_index {
    let idx = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_index>() as u64) as u32)
        as *mut cff_index;
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    let count = u16::get(handle);
    (*idx).count = count;
    if count as i32 > 0 {
        (*idx).offsize = u8::get(handle);
        if ((*idx).offsize as i32) < 1 || (*idx).offsize as i32 > 4 {
            panic!("invalid offsize data");
        }
        (*idx).offset = new(((count as i32 + 1) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<l_offset>() as u64)
            as u32) as *mut l_offset;
        for i in 0..count + 1 {
            *(*idx).offset.offset(i as isize) = get_unsigned(handle, (*idx).offsize as i32);
        }
        if *(*idx).offset.offset(0) != 1_u32 {
            panic!("Invalid CFF Index offset data");
        }
        let mut length =
            (*(*idx).offset.offset(count as isize)).wrapping_sub(*(*idx).offset.offset(0)) as i32;
        (*idx).data =
            new((length as u32 as u64).wrapping_mul(::std::mem::size_of::<u8>() as u64) as u32)
                as *mut u8;
        let mut offset = 0;
        while length > 0 {
            let slice = std::slice::from_raw_parts_mut(
                ((*idx).data).offset(offset as isize),
                length as usize,
            );
            let nb_read = handle.read(slice).unwrap() as i32;
            offset += nb_read;
            length -= nb_read
        }
    } else {
        (*idx).offsize = 0 as c_offsize;
        (*idx).offset = ptr::null_mut();
        (*idx).data = ptr::null_mut()
    }
    idx
}

pub(crate) unsafe fn cff_pack_index(idx: &mut cff_index, mut dest: &mut [u8]) -> usize {
    let destlen = dest.len();
    if (idx.count as i32) < 1 {
        if destlen < 2 {
            panic!("Not enough space available...");
        }
        memset(dest.as_mut_ptr() as *mut libc::c_void, 0, 2);
        return 2;
    }
    let len = cff_index_size(idx);
    let datalen = (*idx.offset.offset(idx.count as isize)).wrapping_sub(1_u32) as usize;
    if destlen < len {
        panic!("Not enough space available...");
    }
    dest[0] = (idx.count as i32 >> 8 & 0xff) as u8;
    dest = &mut dest[1..];
    dest[0] = (idx.count as i32 & 0xff) as u8;
    dest = &mut dest[1..];
    if datalen < 0xff {
        idx.offsize = 1;
        dest[0] = 1 as u8;
        dest = &mut dest[1..];
        for i in 0..=idx.count as i32 {
            dest[0] = (*idx.offset.offset(i as isize) & 0xff_u32) as u8;
            dest = &mut dest[1..];
        }
    } else if datalen < 0xffff {
        idx.offsize = 2;
        dest[0] = 2;
        dest = &mut dest[1..];
        for i in 0..=idx.count as i32 {
            dest[0] = (*idx.offset.offset(i as isize) >> 8 & 0xff_u32) as u8;
            dest = &mut dest[1..];
            dest[0] = (*idx.offset.offset(i as isize) & 0xff_u32) as u8;
            dest = &mut dest[1..];
        }
    } else if datalen < 0xffffff {
        idx.offsize = 3;
        dest[0] = 3;
        dest = &mut dest[1..];
        for i in 0..=idx.count as i32 {
            dest[0] = (*idx.offset.offset(i as isize) >> 16 & 0xff_u32) as u8;
            dest = &mut dest[1..];
            dest[0] = (*idx.offset.offset(i as isize) >> 8 & 0xff_u32) as u8;
            dest = &mut dest[1..];
            dest[0] = (*idx.offset.offset(i as isize) & 0xff_u32) as u8;
            dest = &mut dest[1..];
        }
    } else {
        idx.offsize = 4;
        dest[0] = 4;
        dest = &mut dest[1..];
        for i in 0..=idx.count as i32 {
            dest[0] = (*idx.offset.offset(i as isize) >> 24 & 0xff_u32) as u8;
            dest = &mut dest[1..];
            dest[0] = (*idx.offset.offset(i as isize) >> 16 & 0xff_u32) as u8;
            dest = &mut dest[1..];
            dest[0] = (*idx.offset.offset(i as isize) >> 8 & 0xff_u32) as u8;
            dest = &mut dest[1..];
            dest[0] = (*idx.offset.offset(i as isize) & 0xff_u32) as u8;
            dest = &mut dest[1..];
        }
    }
    memmove(
        dest.as_mut_ptr() as *mut libc::c_void,
        idx.data as *const libc::c_void,
        (*idx.offset.offset(idx.count as isize)).wrapping_sub(1) as _,
    );
    len
}

pub(crate) unsafe fn cff_index_size(mut idx: *mut cff_index) -> usize {
    if (*idx).count as i32 > 0 {
        let datalen = (*(*idx).offset.offset((*idx).count as isize)).wrapping_sub(1_u32);
        if (datalen as u64) < 0xff {
            (*idx).offsize = 1 as c_offsize
        } else if (datalen as u64) < 0xffff {
            (*idx).offsize = 2 as c_offsize
        } else if (datalen as u64) < 0xffffff {
            (*idx).offsize = 3 as c_offsize
        } else {
            (*idx).offsize = 4 as c_offsize
        }
        ((3 + (*idx).offsize as i32 * ((*idx).count as i32 + 1)) as u32).wrapping_add(datalen)
            as usize
    } else {
        2
    }
}

pub(crate) unsafe fn cff_new_index(count: u16) -> *mut cff_index {
    let idx = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_index>() as u64) as u32)
        as *mut cff_index;
    (*idx).count = count;
    (*idx).offsize = 0;
    if count as i32 > 0 {
        (*idx).offset = new(((count as i32 + 1) as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<l_offset>() as u64)
            as u32) as *mut l_offset;
        *(*idx).offset.offset(0) = 1;
    } else {
        (*idx).offset = ptr::null_mut()
    }
    (*idx).data = ptr::null_mut();
    idx
}

pub(crate) unsafe fn cff_release_index(idx: *mut cff_index) {
    if !idx.is_null() {
        if !(*idx).data.is_null() {
            free((*idx).data as *mut libc::c_void);
        }
        if !(*idx).offset.is_null() {
            free((*idx).offset as *mut libc::c_void);
        }
        free(idx as *mut libc::c_void);
    };
}
/* Strings */

pub(crate) unsafe fn cff_get_string(cff: &cff_font, mut id: s_SID) -> String {
    let mut result = String::new();
    if (id as i32) < 391 {
        result = cff_stdstr[id as usize].to_string();
    } else if let Some(strings) = cff.string.as_deref() {
        id = (id as i32 - 391) as s_SID;
        if (id as i32) < strings.count as i32 {
            let offset = strings.offset[id as usize] as usize;
            let size = (strings.offset[id as usize + 1] as usize) - offset;
            let data = &strings.data[offset - 1..offset - 1 + size];
            result = String::from_utf8_lossy(&data).to_string();
        }
    }
    result
}

pub(crate) unsafe fn cff_get_sid(cff: &cff_font, s: &str) -> i32 {
    if s.is_empty() {
        return -1;
    }
    /* I search String INDEX first. */
    if let Some(idx) = cff.string.as_deref() {
        for i in 0..idx.count {
            let offset = idx.offset[i as usize] as usize;
            let size = (idx.offset[i as usize + 1] as usize) - offset;
            if s.as_bytes() == &idx.data[offset - 1..offset - 1 + size] {
                return i as i32 + 391;
            }
        }
    }
    for i in 0..391 {
        if s == cff_stdstr[i] {
            return i as i32;
        }
    }
    -1
}

pub(crate) unsafe fn cff_get_seac_sid(_cff: &cff_font, s: &str) -> i32 {
    if s.is_empty() {
        return -1;
    }
    for i in 0..391 {
        if s == cff_stdstr[i] {
            return i as i32;
        }
    }
    -1
}
unsafe fn cff_match_string(cff: &cff_font, s: &str, sid: s_SID) -> bool {
    if (sid as i32) < 391 {
        s == cff_stdstr[sid as usize]
    } else {
        let i = (sid as i32 - 391) as u16;
        match cff.string.as_deref() {
            Some(string) if (i as i32) < (string.count as i32) => {
                let offset = string.offset[i as usize] as usize;
                let size = (string.offset[i as usize + 1] as usize) - offset;
                s.as_bytes() == &string.data[offset - 1..offset - 1 + size]
            }
            _ => panic!("Invalid SID"),
        }
    }
}

pub(crate) unsafe fn cff_update_string(cff: &mut cff_font) {
    cff.string = (*cff)._string.take();
}
/* String */

pub(crate) unsafe fn cff_add_string(cff: &mut cff_font, s: &str, unique: i32) -> s_SID {
    /* Setting unique == 1 eliminates redundant or predefined strings. */
    let len: usize = s.len() as _;
    if cff._string.is_none() {
        cff._string = Some(CffIndex::new(0));
    }
    let strings = cff._string.as_deref().unwrap();
    if unique != 0 {
        /* TODO: do binary search to speed things up */
        for idx in 0..391 {
            if cff_stdstr[idx] == s {
                return idx as s_SID;
            }
        }
        for idx in 0..strings.count {
            let size = (strings.offset[idx as usize + 1] - strings.offset[idx as usize]) as usize;
            let offset = strings.offset[idx as usize] as usize;
            if &strings.data[offset - 1..offset - 1 + size] == s.as_bytes() {
                return (idx as i32 + 391) as s_SID;
            }
        }
    }
    let strings = cff._string.as_deref_mut().unwrap();
    let offset = if strings.count > 0 {
        strings.offset[strings.count as usize]
    } else {
        1
    };
    strings.offset.resize(strings.count as usize + 2, 0);
    if strings.count == 0 {
        strings.offset[0] = 1 as l_offset;
    }
    let idx = strings.count;
    strings.count += 1;
    strings.offset[strings.count as usize] = (offset as u64).wrapping_add(len as _) as l_offset;
    strings.data.extend(s.as_bytes());
    (idx as i32 + 391) as s_SID
}
/*
 * Encoding and Charset
 *
 *  Encoding and Charset arrays always begin with GID = 1.
 */

pub(crate) unsafe fn cff_read_encoding(cff: &mut cff_font) -> i32 {
    if !cff.topdict.contains_key("Encoding") {
        cff.flag |= 1 << 3;
        cff.encoding = ptr::null_mut();
        return 0;
    }
    let offset = cff.topdict.get("Encoding", 0) as i32;
    if offset == 0 {
        /* predefined */
        cff.flag |= 1 << 3;
        cff.encoding = ptr::null_mut();
        return 0;
    } else {
        if offset == 1 {
            cff.flag |= 1 << 4;
            cff.encoding = ptr::null_mut();
            return 0;
        }
    }
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let encoding = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_encoding>() as u64) as u32)
        as *mut cff_encoding;
    cff.encoding = encoding;
    (*encoding).format = u8::get(handle);
    let mut length = 1;
    match (*encoding).format as i32 & !0x80 {
        0 => {
            (*encoding).num_entries = u8::get(handle);
            (*encoding).data.codes = new(((*encoding).num_entries as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<u8>() as u64)
                as u32) as *mut u8;
            for i in 0..(*encoding).num_entries {
                *(*encoding).data.codes.offset(i as isize) = u8::get(handle);
            }
            length += (*encoding).num_entries as i32 + 1
        }
        1 => {
            (*encoding).num_entries = u8::get(handle);
            let ranges = new(((*encoding).num_entries as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<cff_range1>() as u64)
                as u32) as *mut cff_range1;
            (*encoding).data.range1 = ranges;
            for i in 0..(*encoding).num_entries {
                (*ranges.offset(i as isize)).first = u8::get(handle) as s_SID;
                (*ranges.offset(i as isize)).n_left = u8::get(handle);
            }
            length += (*encoding).num_entries as i32 * 2 + 1
        }
        _ => {
            free(encoding as *mut libc::c_void);
            panic!("Unknown Encoding format");
        }
    }
    /* Supplementary data */
    if (*encoding).format as i32 & 0x80 != 0 {
        (*encoding).num_supps = u8::get(handle);
        let map = new(((*encoding).num_supps as u32 as u64)
            .wrapping_mul(::std::mem::size_of::<cff_map>() as u64) as u32)
            as *mut cff_map;
        (*encoding).supp = map;
        for i in 0..(*encoding).num_supps {
            (*map.offset(i as isize)).code = u8::get(handle);
            (*map.offset(i as isize)).glyph = u16::get(handle);
            /* SID */
        }
        length += (*encoding).num_supps as i32 * 3 + 1
    } else {
        (*encoding).num_supps = 0 as u8;
        (*encoding).supp = ptr::null_mut()
    }
    length
}

pub(crate) unsafe fn cff_pack_encoding(cff: &cff_font, dest: &mut [u8]) -> usize {
    let mut len = 0_usize;
    if cff.flag & (1 << 3 | 1 << 4) != 0 || cff.encoding.is_null() {
        return 0;
    }
    let encoding = &*cff.encoding;
    dest[len] = encoding.format;
    len += 1;
    dest[len] = encoding.num_entries;
    len += 1;
    match encoding.format as i32 & !0x80 {
        0 => {
            for i in 0..encoding.num_entries as isize {
                dest[len] = *encoding.data.codes.offset(i);
                len += 1;
            }
        }
        1 => {
            for i in 0..encoding.num_entries as isize {
                dest[len] = ((*encoding.data.range1.offset(i)).first as i32 & 0xff) as u8;
                len += 1;
                dest[len] = (*encoding.data.range1.offset(i)).n_left;
                len += 1;
            }
        }
        _ => {
            panic!("Unknown Encoding format");
        }
    }
    if encoding.format as i32 & 0x80 != 0 {
        dest[len] = encoding.num_supps;
        len += 1;
        for i in 0..encoding.num_supps as isize {
            dest[len] = (*encoding.supp.offset(i)).code;
            len += 1;
            dest[len..len + 2].copy_from_slice(&(*encoding.supp.offset(i)).glyph.to_be_bytes());
            len += 2;
        }
    }
    len
}
/* input: code, output: glyph index */

pub(crate) unsafe fn cff_encoding_lookup(cff: &cff_font, code: u8) -> u16 {
    if cff.flag & (1 << 3 | 1 << 4) != 0 {
        panic!("Predefined CFF encoding not supported yet");
    } else {
        if cff.encoding.is_null() {
            panic!("Encoding data not available");
        }
    }
    let encoding = cff.encoding;
    let mut gid = 0;
    match (*encoding).format as i32 & !0x80 {
        0 => {
            for i in 0..(*encoding).num_entries {
                if code as i32 == *(*encoding).data.codes.offset(i as isize) as i32 {
                    gid = (i as i32 + 1) as u16;
                    break;
                }
            }
        }
        1 => {
            let mut i = 0;
            while (i as i32) < (*encoding).num_entries as i32 {
                if code as i32 >= (*(*encoding).data.range1.offset(i as isize)).first as i32
                    && code as i32
                        <= (*(*encoding).data.range1.offset(i as isize)).first as i32
                            + (*(*encoding).data.range1.offset(i as isize)).n_left as i32
                {
                    gid = (gid as i32
                        + (code as i32
                            - (*(*encoding).data.range1.offset(i as isize)).first as i32
                            + 1)) as u16;
                    break;
                } else {
                    gid = (gid as i32
                        + ((*(*encoding).data.range1.offset(i as isize)).n_left as i32 + 1))
                        as u16;
                    i += 1;
                }
            }
            if i as i32 == (*encoding).num_entries as i32 {
                gid = 0 as u16
            }
        }
        _ => {
            panic!("Unknown Encoding format.");
        }
    }
    /* Supplementary data */
    if gid as i32 == 0 && (*encoding).format as i32 & 0x80 != 0 {
        if (*encoding).supp.is_null() {
            panic!("No CFF supplementary encoding data read.");
        }
        let map = (*encoding).supp;
        for i in 0..(*encoding).num_supps {
            if code as i32 == (*map.offset(i as isize)).code as i32 {
                gid = cff_charsets_lookup(cff, (*map.offset(i as isize)).glyph);
                break;
            }
        }
    }
    gid
}

pub(crate) unsafe fn cff_release_encoding(encoding: *mut cff_encoding) {
    if !encoding.is_null() {
        match (*encoding).format as i32 & !0x80 {
            0 => {
                free((*encoding).data.codes as *mut libc::c_void);
            }
            1 => {
                free((*encoding).data.range1 as *mut libc::c_void);
            }
            _ => {
                panic!("Unknown Encoding format.");
            }
        }
        if (*encoding).format as i32 & 0x80 != 0 {
            free((*encoding).supp as *mut libc::c_void);
        }
        free(encoding as *mut libc::c_void);
    };
}

pub(crate) unsafe fn cff_read_charsets(cff: &mut cff_font) -> i32 {
    if !cff.topdict.contains_key("charset") {
        cff.flag |= 1 << 5;
        cff.charsets = ptr::null_mut();
        return 0;
    }
    let offset = cff.topdict.get("charset", 0) as i32;
    if offset == 0 {
        /* predefined */
        cff.flag |= 1 << 5;
        cff.charsets = ptr::null_mut();
        return 0;
    } else {
        if offset == 1 {
            cff.flag |= 1 << 6;
            cff.charsets = ptr::null_mut();
            return 0;
        } else {
            if offset == 2 {
                cff.flag |= 1 << 7;
                cff.charsets = ptr::null_mut();
                return 0;
            }
        }
    }
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let charset = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_charsets>() as u64) as u32)
        as *mut cff_charsets;
    cff.charsets = charset;
    (*charset).format = u8::get(handle);
    (*charset).num_entries = 0 as u16;
    let mut count = (cff.num_glyphs as i32 - 1) as u16;
    let mut length = 1;
    /* Not sure. Not well documented. */
    match (*charset).format as i32 {
        0 => {
            (*charset).num_entries = (cff.num_glyphs as i32 - 1) as u16; /* no .notdef */
            (*charset).data.glyphs = new(((*charset).num_entries as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<s_SID>() as u64)
                as u32) as *mut s_SID; /* no-overrap */
            length += (*charset).num_entries as i32 * 2; /* non-overrapping */
            for i in 0..(*charset).num_entries {
                *(*charset).data.glyphs.offset(i as isize) = u16::get(handle);
            }
            count = 0 as u16
        }
        1 => {
            let mut ranges: *mut cff_range1 = ptr::null_mut();
            while count as i32 > 0 && ((*charset).num_entries as i32) < cff.num_glyphs as i32 {
                ranges = renew(
                    ranges as *mut libc::c_void,
                    (((*charset).num_entries as i32 + 1) as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<cff_range1>() as u64)
                        as u32,
                ) as *mut cff_range1;
                (*ranges.offset((*charset).num_entries as isize)).first = u16::get(handle);
                (*ranges.offset((*charset).num_entries as isize)).n_left = u8::get(handle);
                count = (count as i32
                    - ((*ranges.offset((*charset).num_entries as isize)).n_left as i32 + 1))
                    as u16;
                (*charset).num_entries = ((*charset).num_entries as i32 + 1) as u16;
                (*charset).data.range1 = ranges
            }
            length += (*charset).num_entries as i32 * 3
        }
        2 => {
            let mut ranges_0: *mut cff_range2 = ptr::null_mut();
            while count as i32 > 0 && ((*charset).num_entries as i32) < cff.num_glyphs as i32 {
                ranges_0 = renew(
                    ranges_0 as *mut libc::c_void,
                    (((*charset).num_entries as i32 + 1) as u32 as u64)
                        .wrapping_mul(::std::mem::size_of::<cff_range2>() as u64)
                        as u32,
                ) as *mut cff_range2;
                (*ranges_0.offset((*charset).num_entries as isize)).first = u16::get(handle);
                (*ranges_0.offset((*charset).num_entries as isize)).n_left = u16::get(handle);
                count = (count as i32
                    - ((*ranges_0.offset((*charset).num_entries as isize)).n_left as i32 + 1))
                    as u16;
                (*charset).num_entries = ((*charset).num_entries as i32 + 1) as u16
            }
            (*charset).data.range2 = ranges_0;
            length += (*charset).num_entries as i32 * 4
        }
        _ => {
            free(charset as *mut libc::c_void);
            panic!("Unknown Charset format");
        }
    }
    if count as i32 > 0 {
        panic!("Charset data possibly broken");
    }
    length
}

pub(crate) unsafe fn cff_pack_charsets(cff: &cff_font, dest: &mut [u8]) -> usize {
    let destlen = dest.len();
    let mut len = 0;
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 || cff.charsets.is_null() {
        return 0;
    }
    if destlen < 1 {
        panic!("in cff_pack_charsets(): Buffer overflow");
    }
    let charset = cff.charsets;
    dest[len] = (*charset).format;
    len += 1;
    match (*charset).format as i32 {
        0 => {
            if destlen < len + (*charset).num_entries as usize * 2 {
                panic!("in cff_pack_charsets(): Buffer overflow");
            }
            for i in 0..((*charset).num_entries as isize) {
                let sid: s_SID = *(*charset).data.glyphs.offset(i);
                dest[len..len + 2].copy_from_slice(&sid.to_be_bytes());
                len += 2;
            }
        }
        1 => {
            if destlen < len + (*charset).num_entries as usize * 3 {
                panic!("in cff_pack_charsets(): Buffer overflow");
            }
            for i in 0..((*charset).num_entries as isize) {
                let range = *(*charset).data.range1.offset(i);
                dest[len..len + 2].copy_from_slice(&range.first.to_be_bytes());
                len += 2;
                dest[len] = range.n_left;
                len += 1;
            }
        }
        2 => {
            if destlen < len + (*charset).num_entries as usize * 4 {
                panic!("in cff_pack_charsets(): Buffer overflow");
            }
            for i in 0..((*charset).num_entries as isize) {
                let range = *(*charset).data.range2.offset(i);
                dest[len..len + 2].copy_from_slice(&range.first.to_be_bytes());
                len += 2;
                dest[len..len + 2].copy_from_slice(&range.n_left.to_be_bytes());
                len += 2;
            }
        }
        _ => {
            panic!("Unknown Charset format");
        }
    }
    len
}

pub(crate) unsafe fn cff_get_glyphname(cff: &cff_font, gid: u16) -> String {
    let sid = cff_charsets_lookup_inverse(cff, gid);
    cff_get_string(cff, sid)
}

pub(crate) unsafe fn cff_glyph_lookup_str(cff: &cff_font, s: &str) -> u16 {
    cff_glyph_lookup(cff, s)
}

pub(crate) unsafe fn cff_glyph_lookup(cff: &cff_font, glyph: &str) -> u16 {
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 {
        panic!("Predefined CFF charsets not supported yet");
    } else {
        if cff.charsets.is_null() {
            panic!("Charsets data not available");
        }
    }
    /* .notdef always have glyph index 0 */
    if glyph.is_empty() || glyph == ".notdef" {
        return 0 as u16;
    }
    let charset = cff.charsets;
    let mut gid = 0u16;
    match (*charset).format as i32 {
        0 => {
            for i in 0..(*charset).num_entries as i32 {
                gid = gid.wrapping_add(1);
                if cff_match_string(cff, glyph, *(*charset).data.glyphs.offset(i as isize)) {
                    return gid;
                }
            }
        }
        1 => {
            for i in 0..(*charset).num_entries as i32 {
                for n in 0..=(*(*charset).data.range1.offset(i as isize)).n_left as i32 {
                    gid = gid.wrapping_add(1);
                    if cff_match_string(
                        cff,
                        glyph,
                        ((*(*charset).data.range1.offset(i as isize)).first as i32 + n as i32)
                            as s_SID,
                    ) {
                        return gid;
                    }
                }
            }
        }
        2 => {
            for i in 0..(*charset).num_entries as i32 {
                for n in 0..=(*(*charset).data.range2.offset(i as isize)).n_left as i32 {
                    gid = gid.wrapping_add(1);
                    if cff_match_string(
                        cff,
                        glyph,
                        ((*(*charset).data.range2.offset(i as isize)).first as i32 + n as i32)
                            as s_SID,
                    ) {
                        return gid;
                    }
                }
            }
        }
        _ => {
            panic!("Unknown Charset format");
        }
    }
    return 0 as u16;
    /* not found, returns .notdef */
}
/* Input : SID or CID (16-bit unsigned int)
 * Output: glyph index
 */

pub(crate) unsafe fn cff_charsets_lookup(cff: &cff_font, cid: u16) -> u16 {
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 {
        panic!("Predefined CFF charsets not supported yet");
    } else {
        if cff.charsets.is_null() {
            panic!("Charsets data not available");
        }
    }
    cff_charsets_lookup_gid(cff.charsets, cid)
}

pub(crate) unsafe fn cff_charsets_lookup_gid(charset: *mut cff_charsets, cid: u16) -> u16 {
    let mut gid: u16 = 0 as u16;
    if cid as i32 == 0 {
        return 0 as u16;
        /* GID 0 (.notdef) */
    }
    match (*charset).format as i32 {
        0 => {
            for i in 0..(*charset).num_entries as i32 {
                if cid as i32 == *(*charset).data.glyphs.offset(i as isize) as i32 {
                    gid = (i as i32 + 1) as u16;
                    return gid;
                }
            }
        }
        1 => {
            for i in 0..(*charset).num_entries as i32 {
                if cid as i32 >= (*(*charset).data.range1.offset(i as isize)).first as i32
                    && cid as i32
                        <= (*(*charset).data.range1.offset(i as isize)).first as i32
                            + (*(*charset).data.range1.offset(i as isize)).n_left as i32
                {
                    gid = (gid as i32
                        + (cid as i32 - (*(*charset).data.range1.offset(i as isize)).first as i32
                            + 1)) as u16;
                    return gid;
                }
                gid = (gid as i32
                    + ((*(*charset).data.range1.offset(i as isize)).n_left as i32 + 1))
                    as u16;
            }
        }
        2 => {
            for i in 0..(*charset).num_entries as i32 {
                if cid as i32 >= (*(*charset).data.range2.offset(i as isize)).first as i32
                    && cid as i32
                        <= (*(*charset).data.range2.offset(i as isize)).first as i32
                            + (*(*charset).data.range2.offset(i as isize)).n_left as i32
                {
                    gid = (gid as i32
                        + (cid as i32 - (*(*charset).data.range2.offset(i as isize)).first as i32
                            + 1)) as u16;
                    return gid;
                }
                gid = (gid as i32
                    + ((*(*charset).data.range2.offset(i as isize)).n_left as i32 + 1))
                    as u16;
            }
        }
        _ => {
            panic!("Unknown Charset format");
        }
    }
    return 0 as u16;
    /* not found */
}
/* Input : GID
 * Output: SID/CID (u16)
 */

pub(crate) unsafe fn cff_charsets_lookup_inverse(cff: &cff_font, gid: u16) -> u16 {
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 {
        panic!("Predefined CFF charsets not supported yet");
    } else if cff.charsets.is_null() {
        panic!("Charsets data not available");
    }
    if gid == 0 {
        return 0;
        /* .notdef */
    }
    cff_charsets_lookup_cid(&*cff.charsets, gid)
}

pub(crate) unsafe fn cff_charsets_lookup_cid(charset: &cff_charsets, mut gid: u16) -> u16 {
    let mut sid: u16 = 0;
    match charset.format as i32 {
        0 => {
            if gid as i32 - 1 >= charset.num_entries as i32 {
                panic!("Invalid GID.");
            }
            sid = *charset.data.glyphs.offset((gid as i32 - 1) as isize)
        }
        1 => {
            let mut i = 0;
            while (i as i32) < charset.num_entries as i32 {
                if gid as i32 <= (*charset.data.range1.offset(i as isize)).n_left as i32 + 1 {
                    sid = (gid as i32 + (*charset.data.range1.offset(i as isize)).first as i32 - 1)
                        as u16;
                    break;
                } else {
                    gid = (gid as i32
                        - ((*charset.data.range1.offset(i as isize)).n_left as i32 + 1))
                        as u16;
                    i += 1;
                }
            }
            if i as i32 == charset.num_entries as i32 {
                panic!("Invalid GID");
            }
        }
        2 => {
            let mut i = 0;
            while (i as i32) < charset.num_entries as i32 {
                if gid as i32 <= (*charset.data.range2.offset(i as isize)).n_left as i32 + 1 {
                    sid = (gid as i32 + (*charset.data.range2.offset(i as isize)).first as i32 - 1)
                        as u16;
                    break;
                } else {
                    gid = (gid as i32
                        - ((*charset.data.range2.offset(i as isize)).n_left as i32 + 1))
                        as u16;
                    i += 1;
                }
            }
            if i as i32 == charset.num_entries as i32 {
                panic!("Invalid GID");
            }
        }
        _ => {
            panic!("Unknown Charset format");
        }
    }
    sid
}

pub(crate) unsafe fn cff_release_charsets(charset: *mut cff_charsets) {
    if !charset.is_null() {
        match (*charset).format as i32 {
            0 => {
                free((*charset).data.glyphs as *mut libc::c_void);
            }
            1 => {
                free((*charset).data.range1 as *mut libc::c_void);
            }
            2 => {
                free((*charset).data.range2 as *mut libc::c_void);
            }
            _ => {}
        }
        free(charset as *mut libc::c_void);
    };
}
/* CID-Keyed font specific */

pub(crate) unsafe fn cff_read_fdselect(cff: &mut cff_font) -> i32 {
    if cff.flag & 1 << 0 == 0 {
        return 0;
    }
    let offset = cff.topdict.get("FDSelect", 0) as i32;
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let fdsel = new((1_u64).wrapping_mul(::std::mem::size_of::<cff_fdselect>() as u64) as u32)
        as *mut cff_fdselect;
    cff.fdselect = fdsel;
    (*fdsel).format = u8::get(handle);
    let mut length = 1;
    match (*fdsel).format as i32 {
        0 => {
            (*fdsel).num_entries = cff.num_glyphs;
            (*fdsel).data.fds = new(((*fdsel).num_entries as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<u8>() as u64)
                as u32) as *mut u8;
            for i in 0..(*fdsel).num_entries as i32 {
                *(*fdsel).data.fds.offset(i as isize) = u8::get(handle);
            }
            length += (*fdsel).num_entries as i32
        }
        3 => {
            (*fdsel).num_entries = u16::get(handle);
            let ranges = new(((*fdsel).num_entries as u32 as u64)
                .wrapping_mul(::std::mem::size_of::<cff_range3>() as u64)
                as u32) as *mut cff_range3;
            (*fdsel).data.ranges = ranges;
            for i in 0..(*fdsel).num_entries as i32 {
                (*ranges.offset(i as isize)).first = u16::get(handle);
                (*ranges.offset(i as isize)).fd = u8::get(handle);
            }
            if (*ranges.offset(0)).first as i32 != 0 {
                panic!("Range not starting with 0.");
            }
            if cff.num_glyphs as i32 != u16::get(handle) as i32 {
                panic!("Sentinel value mismatched with number of glyphs.");
            }
            length += (*fdsel).num_entries as i32 * 3 + 4
        }
        _ => {
            free(fdsel as *mut libc::c_void);
            panic!("Unknown FDSelect format.");
        }
    }
    length
}

pub(crate) unsafe fn cff_pack_fdselect(cff: &cff_font, dest: &mut [u8]) -> usize {
    let mut len = 0;
    if cff.fdselect.is_null() {
        return 0;
    }
    let fdsel = &*cff.fdselect;
    dest[len] = fdsel.format;
    len += 1;
    match fdsel.format as i32 {
        0 => {
            if fdsel.num_entries != cff.num_glyphs {
                panic!("in cff_pack_fdselect(): Invalid data");
            }
            for i in 0..fdsel.num_entries as isize {
                dest[len] = *fdsel.data.fds.offset(i);
                len += 1;
            }
        }
        3 => {
            len += 2;
            for i in 0..fdsel.num_entries as isize {
                dest[len..len + 2]
                    .copy_from_slice(&(*fdsel.data.ranges.offset(i)).first.to_be_bytes());
                len += 2;
                dest[len] = (*fdsel.data.ranges.offset(i)).fd;
                len += 1;
            }
            dest[len..len + 2].copy_from_slice(&cff.num_glyphs.to_be_bytes());
            len += 2;
            dest[1] = (len / 3 - 1 >> 8 & 0xff) as u8;
            dest[2] = (len / 3 - 1 & 0xff) as u8
        }
        _ => {
            panic!("Unknown FDSelect format.");
        }
    }
    len
}

pub(crate) unsafe fn cff_release_fdselect(fdselect: *mut cff_fdselect) {
    if !fdselect.is_null() {
        if (*fdselect).format as i32 == 0 {
            free((*fdselect).data.fds as *mut libc::c_void);
        } else if (*fdselect).format as i32 == 3 {
            free((*fdselect).data.ranges as *mut libc::c_void);
        }
        free(fdselect as *mut libc::c_void);
    };
}

pub(crate) unsafe fn cff_fdselect_lookup(cff: &cff_font, gid: u16) -> u8 {
    if cff.fdselect.is_null() {
        panic!("in cff_fdselect_lookup(): FDSelect not available");
    }
    let fdsel = cff.fdselect;
    if gid as i32 >= cff.num_glyphs as i32 {
        panic!("in cff_fdselect_lookup(): Invalid glyph index");
    }
    let fd = match (*fdsel).format as i32 {
        0 => *(*fdsel).data.fds.offset(gid as isize),
        3 => {
            if gid as i32 == 0 {
                (*(*fdsel).data.ranges.offset(0)).fd
            } else {
                let mut i = 1;
                while i < (*fdsel).num_entries {
                    if (gid as i32) < (*(*fdsel).data.ranges.offset(i as isize)).first as i32 {
                        break;
                    }
                    i += 1;
                }
                (*(*fdsel).data.ranges.offset((i as i32 - 1) as isize)).fd
            }
        }
        _ => {
            panic!("in cff_fdselect_lookup(): Invalid FDSelect format");
        }
    };
    if fd as i32 >= cff.num_fds as i32 {
        panic!("in cff_fdselect_lookup(): Invalid Font DICT index");
    }
    fd
}

pub(crate) unsafe fn cff_read_subrs(cff: &mut cff_font) -> i32 {
    let mut len: i32 = 0;
    if cff.flag & 1 << 0 != 0 && cff.fdarray.is_empty() {
        cff_read_fdarray(cff);
    }
    if cff.private.is_empty() {
        cff_read_private(cff);
    }
    if cff.gsubr.is_none() {
        cff.handle
            .as_ref()
            .unwrap()
            .as_ref()
            .seek(SeekFrom::Start(cff.offset as u64 + cff.gsubr_offset as u64))
            .unwrap();
        cff.gsubr = CffIndex::get(&mut cff.handle.as_ref().unwrap().as_ref());
    }
    cff.subrs = Vec::with_capacity(cff.num_fds as usize);
    if cff.flag & 1 << 0 != 0 {
        for i in 0..cff.num_fds as usize {
            match cff.private[i].as_ref() {
                Some(private) if private.contains_key("Subrs") => {
                    let offset = cff.fdarray[i].as_ref().unwrap().get("Private", 1) as i32;
                    let offset = (offset as f64 + private.get("Subrs", 0)) as i32;
                    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
                    handle
                        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
                        .unwrap();
                    cff.subrs.push(CffIndex::get(handle));
                    len += cff.subrs[i].as_mut().unwrap().size() as i32;
                }
                _ => cff.subrs.push(None),
            }
        }
    } else {
        match cff.private[0].as_ref() {
            Some(private) if private.contains_key("Subrs") => {
                let offset = cff.topdict.get("Private", 1) as i32;
                let offset = (offset as f64 + private.get("Subrs", 0)) as i32;
                let handle = &mut cff.handle.as_ref().unwrap().as_ref();
                handle
                    .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
                    .unwrap();
                cff.subrs.push(CffIndex::get(handle));
                len += cff.subrs[0].as_mut().unwrap().size() as i32;
            }
            _ => cff.subrs.push(None),
        }
    }
    len
}

pub(crate) unsafe fn cff_read_fdarray(cff: &mut cff_font) -> i32 {
    if cff.flag & 1 << 0 == 0 {
        return 0;
    }
    /* must exist */
    let offset = cff.topdict.get("FDArray", 0) as i32;
    cff.handle
        .as_ref()
        .unwrap()
        .as_ref()
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let idx = cff_get_index(cff);
    cff.num_fds = (*idx).count as u8;
    cff.fdarray = Vec::with_capacity((*idx).count as usize);
    for i in 0..(*idx).count as i32 {
        let data = (*idx)
            .data
            .offset(*(*idx).offset.offset(i as isize) as isize)
            .offset(-1);
        let size = (*(*idx).offset.offset((i as i32 + 1) as isize))
            .wrapping_sub(*(*idx).offset.offset(i as isize)) as i32;
        if size > 0 {
            let data = std::slice::from_raw_parts(data, size as usize);
            cff.fdarray.push(Some(cff_dict_unpack(data)));
        } else {
            cff.fdarray.push(None);
        }
    }
    let len = cff_index_size(idx) as i32;
    cff_release_index(idx);
    len
}
/* Flag */
/* FontName */
/* - CFF structure - */
/* CFF Header */
/* Name INDEX */
/* Top DICT (single) */
/* String INDEX */
/* Global Subr INDEX */
/* Encodings */
/* Charsets  */
/* FDSelect, CIDFont only */
/* CharStrings */
/* CIDFont only */
/* per-Font DICT */
/* Local Subr INDEX, per-Private DICT */
/* -- extra data -- */
/* non-zero for OpenType or PostScript wrapped */
/* number of glyphs (CharString INDEX count) */
/* number of Font DICT */
/* Updated String INDEX.
 * Please fix this. We should separate input and output.
 */
/* not used, ASCII Hex filter if needed */
/* CFF fontset index */
/* Flag: see above */
/* 1 if .notdef is not the 1st glyph */
/* CFF Header */
/* CFF INDEX */
/* Name INDEX */
/* Global and Local Subrs INDEX */
/* Encoding */
/* Charsets */
/* Returns GID of PS name "glyph" */
/* Return PS name of "gid" */
/* Returns GID of glyph with SID/CID "cid" */
/* Returns SID or CID */
/* FDSelect */
/* Font DICT(s) */
/* Private DICT(s) */

pub(crate) unsafe fn cff_read_private(cff: &mut cff_font) -> i32 {
    let mut len: i32 = 0;
    let mut size: i32 = 0;
    if cff.flag & 1 << 0 != 0 {
        if cff.fdarray.is_empty() {
            cff_read_fdarray(cff);
        }
        cff.private = Vec::with_capacity(cff.num_fds as usize);
        for i in 0..cff.num_fds as i32 {
            match cff.fdarray[i as usize].as_ref() {
                Some(fdarray)
                    if fdarray.contains_key("Private") && {
                        size = fdarray.get("Private", 0) as i32;
                        size > 0
                    } =>
                {
                    let offset = fdarray.get("Private", 1) as i32;
                    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
                    handle
                        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
                        .unwrap();
                    let mut data = vec![0; size as usize];
                    handle
                        .read_exact(data.as_mut_slice())
                        .expect("reading file failed");
                    cff.private.push(Some(cff_dict_unpack(data.as_slice())));
                    len += size;
                }
                _ => cff.private.push(None),
            }
        }
    } else {
        cff.num_fds = 1 as u8;
        cff.private = Vec::with_capacity(1);
        new((1_u64).wrapping_mul(::std::mem::size_of::<*mut cff_dict>() as u64) as u32)
            as *mut *mut cff_dict;
        if cff.topdict.contains_key("Private") && {
            size = cff.topdict.get("Private", 0) as i32;
            size > 0
        } {
            let handle = &mut cff.handle.as_ref().unwrap().as_ref();
            let offset = cff.topdict.get("Private", 1) as i32;
            handle
                .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
                .unwrap();
            let mut data = vec![0; size as usize];
            handle
                .read_exact(data.as_mut_slice())
                .expect("reading file failed");
            cff.private.push(Some(cff_dict_unpack(data.as_slice())));
            len += size
        } else {
            cff.private.push(None);
            len = 0;
        }
    }
    len
}
