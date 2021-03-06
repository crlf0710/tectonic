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
use crate::{info, warn};
use std::ptr;

use super::dpx_cid::CSI_IDENTITY;

use bridge::{InFile, TTInputFormat};

use super::dpx_cid::CIDSysInfo;

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct rangeDef {
    pub(crate) dim: usize,
    pub(crate) codeLo: *mut u8,
    pub(crate) codeHi: *mut u8,
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct mapDef {
    pub(crate) flag: i32,
    pub(crate) len: usize,
    pub(crate) code: *mut u8,
    pub(crate) next: Option<Box<[mapDef]>>,
}
impl Default for mapDef {
    fn default() -> Self {
        Self {
            flag: 0 | 0,
            len: 0,
            code: ptr::null_mut(),
            next: None,
        }
    }
}

#[derive(Clone)]
#[repr(C)]
pub(crate) struct mapData {
    pub(crate) data: Box<[u8]>,
    pub(crate) prev: Option<Box<mapData>>,
    pub(crate) pos: usize,
}

impl Default for mapData {
    fn default() -> Self {
        Self {
            prev: None,
            pos: 0,
            data: vec![0_u8; 4096].into_boxed_slice(),
        }
    }
}

/* quasi-hack to get the primary input */
/* CID, Code... MEM_ALLOC_SIZE bytes  */
/* Previous mapData data segment      */
/* Position of next free data segment */
pub(crate) struct CMap {
    pub(crate) name: String,
    pub(crate) type_0: i32,
    pub(crate) wmode: i32,
    pub(crate) CSI: Option<Box<CIDSysInfo>>,
    pub(crate) useCMap: Option<&'static mut CMap>,
    pub(crate) codespace: Vec<rangeDef>,
    pub(crate) mapTbl: Option<Box<[mapDef]>>,
    pub(crate) mapData: Box<mapData>,
    pub(crate) profile: C2RustUnnamed,
    pub(crate) reverseMap: Box<[i32]>,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct C2RustUnnamed {
    pub(crate) minBytesIn: usize,
    pub(crate) maxBytesIn: usize,
    pub(crate) minBytesOut: usize,
    pub(crate) maxBytesOut: usize,
}
pub(crate) type CID = u16;

/*
 * References:
 *
 *  PostScript Language Reference Manual, 3rd. ed. (Adobe Systems Inc.)
 *    5.11.4 CMap Dictionaries
 *    5.11.5 FMapType 9 Composite Fonts
 *  Building CMap Files for CID-Keyed Fonts, Adobe Technical Note #5099
 *  CID-Keyed Font Technology Overview, Adobe Technical Note #5092
 *  Adobe CMap and CIDFont Files Specification, Adobe Technical Specification #5014
 *
 *  Undefined Character Handling:
 *    PLRM 3rd. ed., sec. 5.11.5., "Handling Undefined Characters"
 *
 * TODO:
 *   Only cid(range|char) allowed for CODE_TO_CID and bf(range|char) for CID_TO_CODE ?
 */
static mut __verbose: i32 = 0;
static mut __silent: i32 = 0;

pub(crate) unsafe fn CMap_set_verbose(level: i32) {
    __verbose = level;
}

pub(crate) unsafe fn CMap_set_silent(value: i32) {
    __silent = if value != 0 { 1 } else { 0 };
}

impl CMap {
    pub(crate) unsafe fn new() -> Self {
        let profile = C2RustUnnamed {
            minBytesIn: 2,
            maxBytesIn: 2,
            minBytesOut: 2,
            maxBytesOut: 2,
        };

        Self {
            profile,
            name: String::new(),
            type_0: 1,
            wmode: 0,
            useCMap: None,
            CSI: None,
            codespace: Vec::with_capacity(10),
            mapTbl: None,
            mapData: Default::default(),
            reverseMap: vec![0; 65536].into_boxed_slice(),
        }
    }
}

impl CMap {
    pub(crate) unsafe fn is_identity(&self) -> bool {
        self.name == "Identity-H" || self.name == "Identity-V"
    }

    pub(crate) unsafe fn is_valid(&self) -> bool {
        /* Quick check */
        if self.name.is_empty()
            || self.type_0 < 0
            || self.type_0 > 3
            || self.codespace.len() < 1
            || self.type_0 != 0 && self.mapTbl.is_none()
        {
            return false;
        }
        if let Some(use_cmap) = self.useCMap.as_ref() {
            let csi1 = self.get_CIDSysInfo().unwrap();
            let csi2 = use_cmap.get_CIDSysInfo().unwrap();
            if csi1.registry != csi2.registry || csi1.ordering != csi2.ordering {
                warn!(
                    "CIDSystemInfo mismatched {} <--> {}",
                    self.get_name(),
                    use_cmap.get_name(),
                );
                return false;
            }
        }
        true
    }
}

pub(crate) unsafe fn CMap_get_profile(cmap: &CMap, type_0: i32) -> i32 {
    match type_0 {
        0 => cmap.profile.minBytesIn as i32,
        1 => cmap.profile.maxBytesIn as i32,
        2 => cmap.profile.maxBytesOut as i32,
        3 => cmap.profile.maxBytesOut as i32,
        _ => {
            panic!("{}: Unrecognized profile type {}.", "CMap", type_0);
        }
    }
}
/*
 * Put notdef chars for codes not declared in notdef(range|char)
 */
unsafe fn handle_undefined<'a>(
    cmap: &CMap,
    inbuf: &mut &[u8],
    outbuf: &'a mut [u8],
) -> &'a mut [u8] {
    if outbuf.len() < 2 {
        panic!("{}: Buffer overflow.", "CMap");
    }
    match cmap.type_0 {
        1 => {
            outbuf[..2].copy_from_slice(b"\x00\x00");
        }
        2 => {
            outbuf[..2].copy_from_slice(b"\xff\xfd");
        }
        _ => {
            warn!(
                "Cannot handle undefined mapping for this type of CMap mapping: {}",
                cmap.type_0
            );
            warn!("<0000> is used for .notdef char.");
            outbuf[..2].fill(0);
        }
    }
    let len = bytes_consumed(cmap, *inbuf);
    *inbuf = &inbuf[len as usize..];
    &mut outbuf[2..]
}

pub(crate) unsafe fn CMap_decode_char<'a>(
    cmap: &CMap,
    inbuf: &mut &[u8],
    mut outbuf: &'a mut [u8],
) -> &'a mut [u8] {
    let mut c = 0;
    let mut count: usize = 0;
    let mut save = *inbuf;
    let mut p = save;
    /*
     * First handle some special cases:
     */
    if cmap.type_0 == 0 {
        if inbuf.len() % 2 != 0 {
            panic!("{}: Invalid/truncated input string.", "CMap");
        }
        if outbuf.len() < 2 {
            panic!("{}: Buffer overflow.", "CMap");
        }
        outbuf[..2].copy_from_slice(&inbuf[..2]);
        *inbuf = &inbuf[2..];
        return &mut outbuf[2..];
    } else {
        if cmap.mapTbl.is_none() {
            if let Some(useCMap) = cmap.useCMap.as_ref() {
                return CMap_decode_char(useCMap, inbuf, outbuf);
            } else {
                /* no mapping available in this CMap */
                warn!("No mapping available for this character.");
                return handle_undefined(cmap, inbuf, outbuf);
            }
        }
    }
    let mut t = cmap.mapTbl.as_deref().unwrap();
    while count < inbuf.len() {
        c = p[0] as usize;
        p = &p[1..];
        count += 1;
        if t[c].flag & 1 << 4 == 0 {
            break;
        }
        t = t[c].next.as_deref().unwrap();
    }
    if t[c].flag & 1 << 4 != 0 {
        /* need more bytes */
        panic!("{}: Premature end of input string.", "CMap");
    } else {
        if if t[c].flag & 0xf != 0 { 1 } else { 0 } == 0 {
            if let Some(use_cmap) = cmap.useCMap.as_ref() {
                return CMap_decode_char(use_cmap, inbuf, outbuf);
            } else {
                /* no mapping available in this CMap */
                warn!("No character mapping available.");
                info!(" CMap name: {}\n", cmap.get_name());
                info!(" input str: ");
                info!("<");
                while save.len() > p.len() {
                    info!("{:02x}", save[0] as i32);
                    save = &save[1..];
                }
                info!(">\n");
                /*
                 * We know partial match found up to `count' bytes,
                 * but we will not use this information for the sake of simplicity.
                 */
                return handle_undefined(cmap, inbuf, outbuf);
            }
        } else {
            match t[c].flag & 0xf {
                8 => {
                    warn!("Character mapped to .notdef found.");
                }
                1 | 4 => {}
                2 => {
                    panic!("{}: CharName mapping not supported.", "CMap");
                }
                _ => {
                    panic!("{}: Unknown mapping type.", "CMap");
                }
            }
            /* continue */
            let len = t[c].len as usize;
            if outbuf.len() >= len {
                outbuf[..len].copy_from_slice(std::slice::from_raw_parts(t[c].code, len));
            } else {
                panic!("{}: Buffer overflow.", "CMap");
            }
            outbuf = &mut outbuf[len..];
            *inbuf = p;
        }
    }
    outbuf
}
/*
 * For convenience, it does not do decoding to CIDs.
 */

pub(crate) unsafe fn CMap_decode<'a>(
    cmap: &CMap,
    inbuf: &mut &[u8],
    mut outbuf: &'a mut [u8],
) -> (usize, &'a mut [u8]) {
    let mut count = 0;
    while !inbuf.is_empty() && !outbuf.is_empty() {
        outbuf = CMap_decode_char(cmap, inbuf, outbuf);
        count += 1;
    }
    (count, outbuf)
}

pub(crate) unsafe fn CMap_reverse_decode(cmap: &CMap, cid: CID) -> i32 {
    let ch = cmap.reverseMap[cid as usize];
    if ch == 0 {
        if let Some(useCMap) = cmap.useCMap.as_ref() {
            return CMap_reverse_decode(useCMap, cid);
        }
    }
    ch
}

impl CMap {
    pub(crate) fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub(crate) fn get_type(&self) -> i32 {
        self.type_0
    }

    pub(crate) fn get_wmode(&self) -> i32 {
        self.wmode
    }

    pub(crate) unsafe fn get_CIDSysInfo(&self) -> Option<&CIDSysInfo> {
        self.CSI.as_deref()
    }

    pub(crate) fn set_name(&mut self, name: &str) {
        self.name = name.to_string();
    }

    pub(crate) fn set_type(&mut self, type_0: i32) {
        self.type_0 = type_0;
    }

    pub(crate) fn set_wmode(&mut self, wmode: i32) {
        self.wmode = wmode;
    }

    pub(crate) unsafe fn set_CIDSysInfo(&mut self, csi: &CIDSysInfo) {
        if !csi.registry.is_empty() && !csi.ordering.is_empty() {
            self.CSI = Some(Box::new(csi.clone()));
        } else {
            warn!("Invalid CIDSystemInfo.");
            self.CSI = None;
        };
    }
}
/*
 * Can have muliple entry ?
 */

pub(crate) unsafe fn CMap_set_usecmap(cmap: &mut CMap, ucmap: &'static mut CMap) {
    /* Maybe if (!ucmap) panic! is better for this. */
    if (cmap as *mut CMap) == (ucmap as *mut CMap) {
        panic!(
            "{}: Identical CMap object cannot be used for usecmap CMap: 0x{:p}=0x{:p}",
            "CMap", cmap, ucmap,
        );
    }
    /* Check if ucmap have neccesary information. */
    if !ucmap.is_valid() {
        panic!("{}: Invalid CMap.", "CMap");
    }
    /*
     *  CMapName of cmap can be undefined when usecmap is executed in CMap parsing.
     *  And it is also possible CSI is not defined at that time.
     */
    if cmap.name == ucmap.name {
        panic!(
            "{}: CMap refering itself not allowed: CMap {} --> {}",
            "CMap", cmap.name, ucmap.name,
        );
    }
    if let Some(cmap_csi) = cmap.CSI.as_ref() {
        let ucmap_csi = ucmap.CSI.as_ref().unwrap();
        if cmap_csi.registry != ucmap_csi.registry || cmap_csi.ordering != ucmap_csi.ordering {
            panic!(
                "CMap: CMap {} required by {} have different CSI.",
                cmap.get_name(),
                ucmap.get_name(),
            );
        }
    }
    /* We must copy codespaceranges. */
    for csr in &ucmap.codespace {
        use std::slice::from_raw_parts;
        cmap.add_codespacerange(
            from_raw_parts(csr.codeLo, csr.dim),
            from_raw_parts(csr.codeHi, csr.dim),
        );
    }
    cmap.useCMap = Some(ucmap);
}
/* Test the validity of character c. */
unsafe fn CMap_match_codespace(cmap: *mut CMap, c: &[u8]) -> i32 {
    assert!(!cmap.is_null());
    let dim = c.len();
    for csr in &(*cmap).codespace {
        if csr.dim == dim {
            let mut pos = 0;
            while pos < dim {
                if c[pos] as i32 > *csr.codeHi.offset(pos as isize) as i32
                    || (c[pos] as i32) < *csr.codeLo.offset(pos as isize) as i32
                {
                    break;
                }
                pos += 1;
            }
            if pos == dim {
                return 0;
            }
        }
        /* Valid */
    }
    return -1;
    /* Invalid */
}
/*
 * No overlapping codespace ranges are allowed, otherwise mapping is ambiguous.
 */
impl CMap {
    pub(crate) unsafe fn add_codespacerange(&mut self, codelo: &[u8], codehi: &[u8]) -> i32 {
        let dim = codelo.len();
        assert!(dim > 0);
        for csr in &self.codespace {
            let mut overlap: bool = true;
            let mut j = 0;
            while j < (if csr.dim < dim { csr.dim } else { dim }) && overlap as i32 != 0 {
                if codelo[j] as i32 >= *csr.codeLo.offset(j as isize) as i32
                    && codelo[j] as i32 <= *csr.codeHi.offset(j as isize) as i32
                    || codehi[j] as i32 >= *csr.codeLo.offset(j as isize) as i32
                        && codehi[j] as i32 <= *csr.codeHi.offset(j as isize) as i32
                {
                    overlap = true
                } else {
                    overlap = false
                }
                j += 1;
            }
            if overlap {
                warn!("Overlapping codespace found. (ingored)");
                return -1;
            }
        }
        if dim < self.profile.minBytesIn {
            self.profile.minBytesIn = dim
        }
        if dim > self.profile.maxBytesIn {
            self.profile.maxBytesIn = dim
        }

        let codeHi = get_mem(&mut self.mapData, dim);
        codeHi.copy_from_slice(&codehi);
        let codeHi = codeHi.as_mut_ptr();
        let codeLo = get_mem(&mut self.mapData, dim);
        codeLo.copy_from_slice(&codelo);
        let codeLo = codeLo.as_mut_ptr();
        self.codespace.push(rangeDef {
            dim,
            codeHi,
            codeLo,
        });
        0
    }

    pub(crate) unsafe fn add_notdefchar(&mut self, src: &[u8], dst: CID) -> i32 {
        self.add_notdefrange(src, src, dst)
    }

    pub(crate) unsafe fn add_notdefrange(
        &mut self,
        srclo: &[u8],
        srchi: &[u8],
        mut dst: CID,
    ) -> i32 {
        /* dst not used here */
        /* FIXME */
        if check_range(self, srclo, srchi, &mut dst as *mut CID as *const u8, 2) < 0 {
            return -1;
        }
        if self.mapTbl.is_none() {
            self.mapTbl = Some(mapDef_new());
        }
        let mut cur = self.mapTbl.as_deref_mut().unwrap();
        let cur = if let Some(tbl) = locate_tbl(&mut cur, srclo) {
            tbl
        } else {
            return -1;
        };
        let srcdim = srclo.len();
        for c in srclo[srcdim - 1]..=srchi[srcdim - 1] {
            let c = c as usize;
            if if cur[c].flag & 0xf != 0 { 1 } else { 0 } != 0 {
                if __silent == 0 {
                    warn!("Trying to redefine already defined code mapping. (ignored)");
                }
            } else {
                let code = get_mem(&mut self.mapData, 2);
                code[0] = (dst as i32 >> 8) as u8;
                code[1] = (dst as i32 & 0xff) as u8;
                cur[c].flag = 0 | 1 << 3;
                cur[c].code = code.as_mut_ptr();
                cur[c].len = code.len();
            }
            /* Do not do dst++ for notdefrange  */
        }
        0
    }
}

impl CMap {
    pub(crate) unsafe fn add_bfchar(&mut self, src: &[u8], dst: &[u8]) -> i32 {
        self.add_bfrange(src, src, dst)
    }

    pub(crate) unsafe fn add_bfrange(&mut self, srclo: &[u8], srchi: &[u8], base: &[u8]) -> i32 {
        let dstdim = base.len();
        if check_range(self, srclo, srchi, base.as_ptr(), dstdim) < 0 {
            return -1;
        }
        if self.mapTbl.is_none() {
            self.mapTbl = Some(mapDef_new());
        }
        let mut cur = self.mapTbl.as_deref_mut().unwrap();
        let cur = if let Some(tbl) = locate_tbl(&mut cur, srclo) {
            tbl
        } else {
            return -1;
        };
        let srcdim = srclo.len();
        for c in srclo[srcdim - 1]..=srchi[srcdim - 1] {
            /* According to 5014.CIDFont_Spec.pdf (p.52),
             * Code mappings (unlike codespace ranges) may overlap,
             * but succeeding maps superceded preceding maps.
             * (reported and patched by Luo Jie on 2007/12/2)
             */
            /*
             * We assume restriction to code ranges also applied here.
             * Addition <00FF> + 1 is undefined.
             *
             * Changed on 2004-03-20:
             *
             *  Should be treated as <0100> in Acrobat's "ToUnicode" CMap.
             */
            let c = c as usize;
            if (if cur[c].flag & 0xf != 0 { 1 } else { 0 }) == 0 || cur[c].len < dstdim {
                cur[c].flag = 0 | 1 << 2;
                cur[c].code = get_mem(&mut self.mapData, dstdim).as_mut_ptr();
            }
            cur[c].len = dstdim;
            std::slice::from_raw_parts_mut(cur[c].code, dstdim).copy_from_slice(base);
            let mut last_byte = (c as i32) - srclo[srcdim - 1] as i32 + base[dstdim - 1] as i32;
            *cur[c].code.offset(dstdim.wrapping_sub(1) as isize) = (last_byte & 0xff) as u8;
            let mut i = dstdim.wrapping_sub(2) as i32;
            while i >= 0 && last_byte > 255 {
                last_byte = *cur[c].code.offset(i as isize) as i32 + 1;
                *cur[c].code.offset(i as isize) = (last_byte & 0xff) as u8;
                i -= 1
            }
        }
        0
    }

    pub(crate) unsafe fn add_cidchar(&mut self, src: &[u8], dst: CID) -> i32 {
        self.add_cidrange(src, src, dst)
    }

    pub(crate) unsafe fn add_cidrange(&mut self, srclo: &[u8], srchi: &[u8], mut base: CID) -> i32 {
        /* base not used here */
        if check_range(self, srclo, srchi, &mut base as *mut CID as *const u8, 2) < 0 {
            /* FIXME */
            return -1;
        }
        if self.mapTbl.is_none() {
            self.mapTbl = Some(mapDef_new());
        }
        let mut cur = self.mapTbl.as_deref_mut().unwrap();
        let cur = if let Some(tbl) = locate_tbl(&mut cur, srclo) {
            tbl
        } else {
            return -1;
        };
        let mut v = 0_usize;
        let srcdim = srclo.len();
        for i in 0..srcdim - 1 {
            v = (v << 8).wrapping_add(srclo[i] as usize);
        }
        self.reverseMap[base as usize] = v as i32;
        for c in srclo[srcdim - 1]..=srchi[srcdim - 1] {
            let c = c as usize;
            if cur[c].flag != 0 {
                if __silent == 0 {
                    warn!("Trying to redefine already defined CID mapping. (ignored)");
                }
            } else {
                let code = get_mem(&mut self.mapData, 2);
                code[0] = (base as i32 >> 8) as u8;
                code[1] = (base as i32 & 0xff) as u8;
                cur[c].flag = 0 | 1 << 0;
                cur[c].len = code.len();
                cur[c].code = code.as_mut_ptr();
                self.reverseMap[base as usize] = (v << 8).wrapping_add(c as _) as i32
            }
            if base as i32 >= 65535 {
                warn!("CID number too large.");
            }
            base += 1;
        }
        0
    }
}

unsafe fn mapDef_new() -> Box<[mapDef]> {
    let mut t = Vec::<mapDef>::with_capacity(256);
    t.resize_with(256, Default::default);
    t.into_boxed_slice()
}
unsafe fn get_mem(map: &mut Box<mapData>, size: usize) -> &mut [u8] {
    if map.pos + size >= 4096 {
        let prev = std::mem::take(map);
        map.prev = Some(prev);
    }
    let pos = map.pos;
    map.pos += size;
    &mut map.data[pos..pos + size]
}
unsafe fn locate_tbl<'a>(mut cur: &'a mut [mapDef], code: &[u8]) -> Option<&'a mut [mapDef]> {
    for i in 0..(code.len() - 1) {
        let c = code[i] as usize;
        if if cur[c].flag & 0xf != 0 { 1 } else { 0 } != 0 {
            warn!("Ambiguous CMap entry.");
            return None;
        }
        if cur[c].next.is_none() {
            /* create new node */
            cur[c].next = Some(mapDef_new());
        }
        cur[c].flag |= 1 << 4;
        cur = cur[c].next.as_deref_mut().unwrap();
    }
    Some(cur)
}
/* Private funcs. */
/*
 * Guess how many bytes consumed as a `single' character:
 * Substring of length bytesconsumed bytes of input string is interpreted as
 * a `single' character by CMap_decode().
 */
unsafe fn bytes_consumed(cmap: &CMap, instr: &[u8]) -> usize {
    let inbytes = instr.len();
    let mut i = 0;
    let mut longest = 0_usize;
    let mut bytesconsumed;
    while i < cmap.codespace.len() {
        let csr = &cmap.codespace[i];
        let mut pos = 0;
        while pos < (if csr.dim < inbytes { csr.dim } else { inbytes }) {
            if instr[pos as usize] as i32 > *csr.codeHi.offset(pos as isize) as i32
                || (instr[pos as usize] as i32) < *csr.codeLo.offset(pos as isize) as i32
            {
                break;
            }
            pos = pos.wrapping_add(1)
        }
        if pos == csr.dim {
            /* part of instr is totally valid in this codespace. */
            return csr.dim;
        }
        if pos > longest {
            longest = pos
        }
        i += 1;
    }
    if i == cmap.codespace.len() {
        /* No matching at all */
        bytesconsumed = cmap.profile.minBytesIn
    } else {
        bytesconsumed = cmap.profile.maxBytesIn;
        for csr_0 in &cmap.codespace {
            if csr_0.dim > longest && csr_0.dim < bytesconsumed {
                bytesconsumed = csr_0.dim
            }
        }
    }
    bytesconsumed
}
unsafe fn check_range(
    cmap: &mut CMap,
    srclo: &[u8],
    srchi: &[u8],
    dst: *const u8,
    dstdim: usize,
) -> i32 {
    let srcdim = srclo.len();
    if srcdim < 1
        || dstdim < 1
        || dst.is_null()
        || &srclo[..srcdim - 1] != &srchi[..srcdim - 1]
        || srclo[srcdim - 1] as i32 > srchi[srcdim - 1] as i32
    {
        warn!("Invalid CMap mapping entry. (ignored)");
        return -1;
    }
    if CMap_match_codespace(cmap, &srclo) < 0 || CMap_match_codespace(cmap, &srchi) < 0 {
        warn!("Invalid CMap mapping entry. (ignored)");
        return -1;
    }
    cmap.profile.minBytesIn = cmap.profile.minBytesIn.min(srcdim);
    cmap.profile.maxBytesIn = cmap.profile.maxBytesIn.max(srcdim);
    cmap.profile.minBytesOut = cmap.profile.minBytesOut.min(dstdim);
    cmap.profile.maxBytesOut = cmap.profile.maxBytesOut.max(dstdim);
    0
}

// Note: The elements are boxed to be able
// to get stable pointers to the cached data.
// (CMap_cache_get returns *mut CMap)
static mut __cache: Vec<Box<CMap>> = Vec::new();

pub(crate) unsafe fn CMap_cache_init() {
    static mut range_min: [u8; 2] = [0; 2];
    static mut range_max: [u8; 2] = [0xff_u8, 0xff_u8];
    __cache.clear();
    /* Create Identity mapping */
    __cache.push(Box::new(CMap::new()));
    __cache.push(Box::new(CMap::new()));
    let cmap = &mut *__cache[0];
    cmap.set_name("Identity-H");
    cmap.set_type(0);
    cmap.set_wmode(0);
    cmap.set_CIDSysInfo(&CSI_IDENTITY);
    cmap.add_codespacerange(&range_min[..2], &range_max[..2]);

    __cache.push(Box::new(CMap::new()));
    let cmap = &mut *__cache[1];
    cmap.set_name("Identity-V");
    cmap.set_type(0);
    cmap.set_wmode(1);
    cmap.set_CIDSysInfo(&CSI_IDENTITY);
    cmap.add_codespacerange(&range_min[..2], &range_max[..2]);
}

pub(crate) unsafe fn CMap_cache_get(id: Option<usize>) -> *mut CMap {
    match id {
        Some(id) if id < __cache.len() => &mut *__cache[id],
        _ => panic!("Invalid CMap ID {:?}", id),
    }
}

pub(crate) unsafe fn CMap_cache_find(cmap_name: &str) -> Option<usize> {
    for (id, cmap) in __cache.iter_mut().enumerate() {
        /* CMapName may be undefined when processing usecmap. */
        let name = (**cmap).get_name();
        if !name.is_empty() && cmap_name == name {
            return Some(id);
        }
    }
    if let Some(handle) = InFile::open(cmap_name, TTInputFormat::CMAP, 0) {
        if CMap::parse_check_sig(&mut &handle).is_err() {
            return None;
        }
        if __verbose != 0 {
            info!("(CMap:{}", cmap_name);
        }

        let id = (*__cache).len();

        let cmap =
            CMap::parse(handle).unwrap_or_else(|| panic!("{}: Parsing CMap file failed.", "CMap"));
        __cache.push(Box::new(cmap));

        if __verbose != 0 {
            info!(")");
        }
        Some(id)
    } else {
        None
    }
}

pub(crate) unsafe fn CMap_cache_add(cmap: Box<CMap>) -> usize {
    if !cmap.is_valid() {
        panic!("{}: Invalid CMap.", "CMap");
    }
    for other in &mut __cache {
        let cmap_name0 = (*cmap).get_name();
        let cmap_name1 = (**other).get_name();
        if cmap_name0 == cmap_name1 {
            panic!("{}: CMap \"{}\" already defined.", "CMap", cmap_name0);
        }
    }

    __cache.push(cmap);
    __cache.len()
}
/* Limits */
/*
 * TYPE_IDENTITY and TYPE_CID_TO_CODE is not defined in the CMap spec.
 */
/* ************************* CMAP_MAIN **************************/
/* charName not supported */

pub(crate) unsafe fn CMap_cache_close() {
    __cache.clear();
}
