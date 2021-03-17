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
use super::dpx_mem::new;
use super::dpx_numbers::GetFromFile;
use libc::memset;

use crate::bridge::InFile;
use std::io::{Read, Seek, SeekFrom};

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

pub(crate) trait Pack {
    fn pack(&mut self, dest: &mut [u8]) -> usize;
}

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
/*#[derive(Clone)]
#[repr(C)]
pub(crate) struct cff_encoding {
    pub(crate) format: u8,
    pub(crate) num_entries: u8,
    pub(crate) data: C2RustUnnamed,
    pub(crate) supp: Vec<cff_map>,
    /* supplement */
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) union C2RustUnnamed {
    pub(crate) codes: *mut u8,
    pub(crate) range1: *mut cff_range1,
}*/
#[derive(Clone)]
pub(crate) enum Encoding {
    Codes(Box<[u8]>),
    CodesSupp(Box<[u8]>, Box<[cff_map]>),
    Range1(Box<[cff_range1]>),
    Range1Supp(Box<[cff_range1]>, Box<[cff_map]>),
}

impl Encoding {
    pub(crate) fn format(&self) -> u8 {
        match self {
            Self::Codes(_) => 0,
            Self::CodesSupp(_, _) => 0 | 0x80,
            Self::Range1(_) => 1,
            Self::Range1Supp(_, _) => 1 | 0x80,
        }
    }
    pub(crate) fn num_entries(&self) -> usize {
        match self {
            Self::Codes(codes) | Self::CodesSupp(codes, _) => codes.len(),
            Self::Range1(ranges) | Self::Range1Supp(ranges, _) => ranges.len(),
        }
    }
    pub(crate) fn num_supps(&self) -> usize {
        match self {
            Self::CodesSupp(_, supp) | Self::Range1Supp(_, supp) => supp.len(),
            _ => 0,
        }
    }
}

#[derive(Clone)]
pub(crate) enum Charsets {
    Glyphs(Box<[s_SID]>),
    Range1(Box<[cff_range1]>),
    Range2(Box<[cff_range2]>),
}
impl Charsets {
    pub(crate) fn format(&self) -> u8 {
        match self {
            Self::Glyphs(_) => 0,
            Self::Range1(_) => 1,
            Self::Range2(_) => 2,
        }
    }
    pub(crate) fn num_entries(&self) -> usize {
        match self {
            Self::Glyphs(glyphs) => glyphs.len(),
            Self::Range1(ranges) => ranges.len(),
            Self::Range2(ranges) => ranges.len(),
        }
    }
}
/* CID-Keyed font specific */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct cff_range3 {
    pub(crate) first: u16,
    pub(crate) fd: u8,
}

#[derive(Clone)]
pub(crate) enum FdSelect {
    Fds(Box<[u8]>),
    Ranges(Box<[cff_range3]>),
}
impl FdSelect {
    pub(crate) fn format(&self) -> u8 {
        match self {
            Self::Fds(_) => 0,
            Self::Ranges(_) => 3,
        }
    }
    pub(crate) fn num_entries(&self) -> usize {
        match self {
            Self::Fds(fds) => fds.len(),
            Self::Ranges(ranges) => ranges.len(),
        }
    }
}
#[repr(C)]
pub(crate) struct cff_font {
    pub(crate) fontname: String,
    pub(crate) header: cff_header,
    pub(crate) name: Box<CffIndex>,
    pub(crate) topdict: cff_dict,
    pub(crate) string: Option<Box<CffIndex>>,
    pub(crate) gsubr: Option<Box<CffIndex>>,
    pub(crate) encoding: Option<Box<Encoding>>,
    pub(crate) charsets: Option<Rc<Charsets>>,
    pub(crate) fdselect: Option<Box<FdSelect>>,
    pub(crate) cstrings: Option<Box<CffIndex>>,
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
        encoding: None,
        charsets: None,
        fdselect: None,
        cstrings: None,
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

pub(crate) unsafe fn cff_get_index_header<R: Read + Seek>(handle: &mut R) -> Option<Box<CffIndex>> {
    let count = u16::get(handle);
    if count as i32 > 0 {
        let offsize = u8::get(handle);
        if (offsize as i32) < 1 || offsize as i32 > 4 {
            panic!("invalid offsize data");
        }
        let mut offset = Vec::with_capacity((count + 1) as _);
        for _ in 0..count {
            offset.push(get_unsigned(handle, offsize as i32));
        }
        if count as i32 == 0xffff {
            let n = handle.seek(SeekFrom::Current(0)).unwrap();
            handle.seek(SeekFrom::Start(n + offsize as u64)).unwrap();
            offset.push(0); //
        } else {
            offset.push(get_unsigned(handle, offsize as i32));
        }
        if offset[0] != 1 {
            panic!("cff_get_index(): invalid index data");
        }
        Some(Box::new(CffIndex {
            count,
            offsize,
            offset,
            data: Vec::new(),
        }))
    } else {
        None
    }
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
        cff.encoding = None;
        return 0;
    }
    let offset = cff.topdict.get("Encoding", 0) as i32;
    if offset == 0 {
        /* predefined */
        cff.flag |= 1 << 3;
        cff.encoding = None;
        return 0;
    } else {
        if offset == 1 {
            cff.flag |= 1 << 4;
            cff.encoding = None;
            return 0;
        }
    }
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let format = u8::get(handle);
    let mut length = 1;
    let mut encoding = match format as i32 & !0x80 {
        0 => {
            let num_entries = u8::get(handle) as usize;
            let mut codes = Vec::with_capacity(num_entries);
            for _ in 0..num_entries {
                codes.push(u8::get(handle));
            }
            length += num_entries as i32 + 1;
            Encoding::Codes(codes.into_boxed_slice())
        }
        1 => {
            let num_entries = u8::get(handle) as usize;
            let mut ranges = Vec::with_capacity(num_entries);
            for _ in 0..num_entries {
                ranges.push(cff_range1 {
                    first: u8::get(handle) as s_SID,
                    n_left: u8::get(handle),
                });
            }
            length += num_entries as i32 * 2 + 1;
            Encoding::Range1(ranges.into_boxed_slice())
        }
        _ => {
            panic!("Unknown Encoding format");
        }
    };
    fn get_supps<R: Read>(handle: &mut R) -> Vec<cff_map> {
        let num_supps = u8::get(handle);
        let mut map = Vec::with_capacity(num_supps as _);
        for _ in 0..num_supps {
            map.push(cff_map {
                code: u8::get(handle),
                glyph: u16::get(handle),
            });
            /* SID */
        }
        map
    }
    /* Supplementary data */
    if format as i32 & 0x80 != 0 {
        let supp = get_supps(handle).into_boxed_slice();
        length += supp.len() as i32 * 3 + 1;
        encoding = match encoding {
            Encoding::Codes(codes) => Encoding::CodesSupp(codes, supp),
            Encoding::Range1(ranges) => Encoding::Range1Supp(ranges, supp),
            _ => unreachable!(),
        };
    }
    cff.encoding = Some(Box::new(encoding));
    length
}

pub(crate) unsafe fn cff_pack_encoding(cff: &cff_font, dest: &mut [u8]) -> usize {
    let mut len = 0_usize;
    if cff.flag & (1 << 3 | 1 << 4) != 0 || cff.encoding.is_none() {
        return 0;
    }
    let encoding = cff.encoding.as_deref().unwrap();
    dest[len] = encoding.format();
    len += 1;
    dest[len] = encoding.num_entries() as _;
    len += 1;
    match encoding {
        Encoding::Codes(codes) | Encoding::CodesSupp(codes, _) => {
            for &c in codes.iter() {
                dest[len] = c;
                len += 1;
            }
        }
        Encoding::Range1(ranges) | Encoding::Range1Supp(ranges, _) => {
            for range in ranges.iter() {
                dest[len] = (range.first as i32 & 0xff) as u8;
                len += 1;
                dest[len] = range.n_left;
                len += 1;
            }
        }
    }
    match encoding {
        Encoding::CodesSupp(_, supp) | Encoding::Range1Supp(_, supp) => {
            dest[len] = supp.len() as _;
            len += 1;
            for s in supp.iter() {
                dest[len] = s.code;
                len += 1;
                dest[len..len + 2].copy_from_slice(&s.glyph.to_be_bytes());
                len += 2;
            }
        }
        _ => {}
    }
    len
}
/* input: code, output: glyph index */

pub(crate) unsafe fn cff_encoding_lookup(cff: &cff_font, code: u8) -> u16 {
    if cff.flag & (1 << 3 | 1 << 4) != 0 {
        panic!("Predefined CFF encoding not supported yet");
    } else {
        if cff.encoding.is_none() {
            panic!("Encoding data not available");
        }
    }
    let encoding = cff.encoding.as_deref().unwrap();
    let mut gid = 0;
    match encoding {
        Encoding::Codes(codes) | Encoding::CodesSupp(codes, _) => {
            for (i, &c) in codes.iter().enumerate() {
                if code == c {
                    gid = (i as i32 + 1) as u16;
                    break;
                }
            }
        }
        Encoding::Range1(ranges) | Encoding::Range1Supp(ranges, _) => {
            let mut i = 0;
            while i < ranges.len() {
                if code as i32 >= ranges[i].first as i32
                    && code as i32 <= ranges[i].first as i32 + ranges[i].n_left as i32
                {
                    gid = (gid as i32 + (code as i32 - ranges[i].first as i32 + 1)) as u16;
                    break;
                } else {
                    gid = (gid as i32 + (ranges[i].n_left as i32 + 1)) as u16;
                    i += 1;
                }
            }
            if i == ranges.len() {
                gid = 0 as u16
            }
        }
    }
    /* Supplementary data */
    if gid as i32 == 0 {
        match encoding {
            Encoding::CodesSupp(_, supp) | Encoding::Range1Supp(_, supp) => {
                if supp.is_empty() {
                    panic!("No CFF supplementary encoding data read.");
                }
                for s in supp.iter() {
                    if code == s.code {
                        gid = cff_charsets_lookup(cff, s.glyph);
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    gid
}

pub(crate) unsafe fn cff_read_charsets(cff: &mut cff_font) -> i32 {
    if !cff.topdict.contains_key("charset") {
        cff.flag |= 1 << 5;
        cff.charsets = None;
        return 0;
    }
    let offset = cff.topdict.get("charset", 0) as i32;
    if offset == 0 {
        /* predefined */
        cff.flag |= 1 << 5;
        cff.charsets = None;
        return 0;
    } else {
        if offset == 1 {
            cff.flag |= 1 << 6;
            cff.charsets = None;
            return 0;
        } else {
            if offset == 2 {
                cff.flag |= 1 << 7;
                cff.charsets = None;
                return 0;
            }
        }
    }
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let format = u8::get(handle);
    let mut count = (cff.num_glyphs as i32 - 1) as u16;
    let mut length = 1;
    /* Not sure. Not well documented. */
    let charset = match format as i32 {
        0 => {
            let num_entries = (cff.num_glyphs as i32 - 1) as usize; /* no .notdef */
            let mut glyphs = Vec::with_capacity(num_entries); /* no-overrap */
            length += num_entries as i32 * 2; /* non-overrapping */
            for _ in 0..num_entries {
                glyphs.push(u16::get(handle));
            }
            count = 0 as u16;
            Charsets::Glyphs(glyphs.into_boxed_slice())
        }
        1 => {
            let mut num_entries = 0;
            let mut ranges = Vec::new();
            while count as i32 > 0 && (num_entries as i32) < cff.num_glyphs as i32 {
                let range1 = cff_range1 {
                    first: u16::get(handle),
                    n_left: u8::get(handle),
                };
                count = (count as i32 - (range1.n_left as i32 + 1)) as u16;
                num_entries = (num_entries as i32 + 1) as u16;
                ranges.push(range1);
            }
            length += ranges.len() as i32 * 3;
            Charsets::Range1(ranges.into_boxed_slice())
        }
        2 => {
            let mut num_entries = 0;
            let mut ranges = Vec::new();
            while count as i32 > 0 && (num_entries as i32) < cff.num_glyphs as i32 {
                let range2 = cff_range2 {
                    first: u16::get(handle),
                    n_left: u16::get(handle),
                };
                count = (count as i32 - (range2.n_left as i32 + 1)) as u16;
                num_entries = (num_entries as i32 + 1) as u16;
                ranges.push(range2);
            }
            length += ranges.len() as i32 * 4;
            Charsets::Range2(ranges.into_boxed_slice())
        }
        _ => {
            panic!("Unknown Charset format");
        }
    };
    if count as i32 > 0 {
        panic!("Charset data possibly broken");
    }
    cff.charsets = Some(Rc::new(charset));
    length
}

pub(crate) unsafe fn cff_pack_charsets(cff: &cff_font, dest: &mut [u8]) -> usize {
    let destlen = dest.len();
    let mut len = 0;
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 || cff.charsets.is_none() {
        return 0;
    }
    if destlen < 1 {
        panic!("in cff_pack_charsets(): Buffer overflow");
    }
    let charset = cff.charsets.as_deref().unwrap();
    dest[len] = charset.format();
    len += 1;
    match charset {
        Charsets::Glyphs(glyphs) => {
            if destlen < len + glyphs.len() * 2 {
                panic!("in cff_pack_charsets(): Buffer overflow");
            }
            for sid in glyphs.iter() {
                dest[len..len + 2].copy_from_slice(&sid.to_be_bytes());
                len += 2;
            }
        }
        Charsets::Range1(ranges) => {
            if destlen < len + ranges.len() * 3 {
                panic!("in cff_pack_charsets(): Buffer overflow");
            }
            for range in ranges.iter() {
                dest[len..len + 2].copy_from_slice(&range.first.to_be_bytes());
                len += 2;
                dest[len] = range.n_left;
                len += 1;
            }
        }
        Charsets::Range2(ranges) => {
            if destlen < len + ranges.len() * 4 {
                panic!("in cff_pack_charsets(): Buffer overflow");
            }
            for range in ranges.iter() {
                dest[len..len + 2].copy_from_slice(&range.first.to_be_bytes());
                len += 2;
                dest[len..len + 2].copy_from_slice(&range.n_left.to_be_bytes());
                len += 2;
            }
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
        if cff.charsets.is_none() {
            panic!("Charsets data not available");
        }
    }
    /* .notdef always have glyph index 0 */
    if glyph.is_empty() || glyph == ".notdef" {
        return 0 as u16;
    }
    let charset = cff.charsets.as_deref().unwrap();
    let mut gid = 0;
    match charset {
        Charsets::Glyphs(glyphs) => {
            for &sid in glyphs.iter() {
                gid += 1;
                if cff_match_string(cff, glyph, sid) {
                    return gid;
                }
            }
        }
        Charsets::Range1(ranges) => {
            for range in ranges.iter() {
                for n in 0..=range.n_left as i32 {
                    gid += 1;
                    if cff_match_string(cff, glyph, (range.first as i32 + n as i32) as s_SID) {
                        return gid;
                    }
                }
            }
        }
        Charsets::Range2(ranges) => {
            for range in ranges.iter() {
                for n in 0..=range.n_left as i32 {
                    gid += 1;
                    if cff_match_string(cff, glyph, (range.first as i32 + n as i32) as s_SID) {
                        return gid;
                    }
                }
            }
        }
    }
    0
    /* not found, returns .notdef */
}
/* Input : SID or CID (16-bit unsigned int)
 * Output: glyph index
 */

pub(crate) unsafe fn cff_charsets_lookup(cff: &cff_font, cid: u16) -> u16 {
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 {
        panic!("Predefined CFF charsets not supported yet");
    } else {
        if let Some(charsets) = cff.charsets.as_ref() {
            cff_charsets_lookup_gid(charsets, cid)
        } else {
            panic!("Charsets data not available");
        }
    }
}

pub(crate) unsafe fn cff_charsets_lookup_gid(charset: &Charsets, cid: u16) -> u16 {
    let mut gid: u16 = 0;
    if cid as i32 == 0 {
        return 0 as u16;
        /* GID 0 (.notdef) */
    }
    match charset {
        Charsets::Glyphs(glyphs) => {
            for (i, &sid) in glyphs.iter().enumerate() {
                if cid == sid {
                    gid = (i as i32 + 1) as u16;
                    return gid;
                }
            }
        }
        Charsets::Range1(ranges) => {
            for range in ranges.iter() {
                if cid as i32 >= range.first as i32
                    && cid as i32 <= range.first as i32 + range.n_left as i32
                {
                    gid = (gid as i32 + (cid as i32 - range.first as i32 + 1)) as u16;
                    return gid;
                }
                gid = (gid as i32 + (range.n_left as i32 + 1)) as u16;
            }
        }
        Charsets::Range2(ranges) => {
            for range in ranges.iter() {
                if cid as i32 >= range.first as i32
                    && cid as i32 <= range.first as i32 + range.n_left as i32
                {
                    gid = (gid as i32 + (cid as i32 - range.first as i32 + 1)) as u16;
                    return gid;
                }
                gid = (gid as i32 + (range.n_left as i32 + 1)) as u16;
            }
        }
    }
    return 0;
    /* not found */
}
/* Input : GID
 * Output: SID/CID (u16)
 */

pub(crate) unsafe fn cff_charsets_lookup_inverse(cff: &cff_font, gid: u16) -> u16 {
    if cff.flag & (1 << 5 | 1 << 6 | 1 << 7) != 0 {
        panic!("Predefined CFF charsets not supported yet");
    } else {
        if let Some(charsets) = cff.charsets.as_ref() {
            if gid == 0 {
                return 0;
                /* .notdef */
            }
            cff_charsets_lookup_cid(charsets, gid)
        } else {
            panic!("Charsets data not available");
        }
    }
}

pub(crate) unsafe fn cff_charsets_lookup_cid(charset: &Charsets, mut gid: u16) -> u16 {
    match charset {
        Charsets::Glyphs(glyphs) => {
            if gid as i32 - 1 >= glyphs.len() as i32 {
                panic!("Invalid GID.");
            }
            return glyphs[(gid as i32 - 1) as usize];
        }
        Charsets::Range1(ranges) => {
            for range in ranges.iter() {
                if gid as i32 <= range.n_left as i32 + 1 {
                    return (gid as i32 + range.first as i32 - 1) as u16;
                } else {
                    gid = (gid as i32 - (range.n_left as i32 + 1)) as u16;
                }
            }
            panic!("Invalid GID");
        }
        Charsets::Range2(ranges) => {
            for range in ranges.iter() {
                if gid as i32 <= range.n_left as i32 + 1 {
                    return (gid as i32 + range.first as i32 - 1) as u16;
                } else {
                    gid = (gid as i32 - (range.n_left as i32 + 1)) as u16;
                }
            }
            panic!("Invalid GID");
        }
    }
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
    let format = u8::get(handle);
    let mut length = 1;
    match format {
        0 => {
            let mut fds = Vec::with_capacity(cff.num_glyphs as _);
            for _ in 0..cff.num_glyphs {
                fds.push(u8::get(handle));
            }
            length += fds.len() as i32;
            cff.fdselect = Some(Box::new(FdSelect::Fds(fds.into_boxed_slice())));
        }
        3 => {
            let num_entries = u16::get(handle);
            let mut ranges = Vec::with_capacity(num_entries as _);
            for _ in 0..num_entries {
                ranges.push(cff_range3 {
                    first: u16::get(handle),
                    fd: u8::get(handle),
                });
            }
            if ranges[0].first != 0 {
                panic!("Range not starting with 0.");
            }
            if cff.num_glyphs as i32 != u16::get(handle) as i32 {
                panic!("Sentinel value mismatched with number of glyphs.");
            }
            length += num_entries as i32 * 3 + 4;
            cff.fdselect = Some(Box::new(FdSelect::Ranges(ranges.into_boxed_slice())));
        }
        _ => {
            panic!("Unknown FDSelect format.");
        }
    }
    length
}

pub(crate) unsafe fn cff_pack_fdselect(cff: &cff_font, dest: &mut [u8]) -> usize {
    let mut len = 0;
    if let Some(fdsel) = cff.fdselect.as_deref() {
        dest[len] = fdsel.format();
        len += 1;
        match fdsel {
            FdSelect::Fds(fds) => {
                if fds.len() != cff.num_glyphs as usize {
                    panic!("in cff_pack_fdselect(): Invalid data");
                }
                for &fd in fds.iter() {
                    dest[len] = fd;
                    len += 1;
                }
            }
            FdSelect::Ranges(ranges) => {
                len += 2;
                for range in ranges.iter() {
                    dest[len..len + 2].copy_from_slice(&range.first.to_be_bytes());
                    len += 2;
                    dest[len] = range.fd;
                    len += 1;
                }
                dest[len..len + 2].copy_from_slice(&cff.num_glyphs.to_be_bytes());
                len += 2;
                dest[1] = (len / 3 - 1 >> 8 & 0xff) as u8;
                dest[2] = (len / 3 - 1 & 0xff) as u8
            }
        }
        len
    } else {
        0
    }
}

pub(crate) unsafe fn cff_fdselect_lookup(cff: &cff_font, gid: u16) -> u8 {
    if cff.fdselect.is_none() {
        panic!("in cff_fdselect_lookup(): FDSelect not available");
    }
    if gid as i32 >= cff.num_glyphs as i32 {
        panic!("in cff_fdselect_lookup(): Invalid glyph index");
    }
    let fd = match cff.fdselect.as_deref().unwrap() {
        FdSelect::Fds(fds) => fds[gid as usize],
        FdSelect::Ranges(ranges) => {
            if gid as i32 == 0 {
                ranges[0].fd
            } else {
                let mut i = 1;
                while i < ranges.len() {
                    if (gid as i32) < ranges[i].first as i32 {
                        break;
                    }
                    i += 1;
                }
                ranges[i - 1].fd
            }
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
    let handle = &mut cff.handle.as_ref().unwrap().as_ref();
    handle
        .seek(SeekFrom::Start(cff.offset as u64 + offset as u64))
        .unwrap();
    let mut idx = CffIndex::get(handle).unwrap();
    cff.num_fds = idx.count as u8;
    cff.fdarray = Vec::with_capacity(idx.count as usize);
    for i in 0..idx.count as i32 {
        let data = &idx.data[idx.offset[i as usize] as usize - 1..];
        let size = (idx.offset[(i as usize + 1) as usize] - idx.offset[i as usize]) as i32;
        if size > 0 {
            cff.fdarray
                .push(Some(cff_dict_unpack(&data[..size as usize])));
        } else {
            cff.fdarray.push(None);
        }
    }
    let len = idx.size() as i32;
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
