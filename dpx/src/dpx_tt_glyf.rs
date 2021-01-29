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
#![allow(mutable_transmutes, non_camel_case_types)]

use crate::warn;
use crate::FromBEByteSlice;

use super::dpx_numbers::GetFromFile;
use super::dpx_sfnt::{sfnt_find_table_pos, sfnt_locate_table, sfnt_set_table};
use super::dpx_tt_table::{
    tt_pack_head_table, tt_pack_hhea_table, tt_pack_maxp_table, tt_read_head_table,
    tt_read_hhea_table, tt_read_longMetrics, tt_read_maxp_table, tt_read_os2__table,
    tt_read_vhea_table,
};
use crate::dpx_truetype::sfnt_table_info;

use std::io::{Read, Seek, SeekFrom};

use super::dpx_sfnt::{sfnt, PutBE};

#[derive(Clone)]
pub(crate) struct tt_glyph_desc {
    pub(crate) gid: u16,
    pub(crate) ogid: u16,
    pub(crate) advw: u16,
    pub(crate) advh: u16,
    pub(crate) lsb: i16,
    pub(crate) tsb: i16,
    pub(crate) llx: i16,
    pub(crate) lly: i16,
    pub(crate) urx: i16,
    pub(crate) ury: i16,
    pub(crate) data: Vec<u8>,
}
#[derive(Clone)]
pub(crate) struct tt_glyphs {
    pub(crate) last_gid: u16,
    pub(crate) emsize: u16,
    pub(crate) dw: u16,
    pub(crate) default_advh: u16,
    pub(crate) default_tsb: i16,
    pub(crate) gd: Vec<tt_glyph_desc>,
    pub(crate) used_slot: Vec<u8>,
}

fn find_empty_slot(g: &tt_glyphs) -> u16 {
    let mut gid = 0_u16;
    while (gid as i32) < 65534 {
        if g.used_slot[(gid as i32 / 8) as usize] as i32 & 1 << 7 - gid as i32 % 8 == 0 {
            break;
        }
        gid = gid.wrapping_add(1)
    }
    if gid as i32 == 65534 {
        panic!("No empty glyph slot available.");
    }
    gid
}

pub(crate) fn tt_find_glyph(g: &tt_glyphs, gid: u16) -> u16 {
    let mut new_gid = 0_u16;
    for i in &g.gd {
        if gid == i.ogid {
            new_gid = i.gid;
            break;
        }
    }
    new_gid
}

pub(crate) fn tt_get_index(g: &tt_glyphs, gid: u16) -> u16 {
    let mut idx = 0;
    while idx < g.gd.len() {
        if gid == g.gd[idx].gid {
            break;
        }
        idx += 1;
    }
    if idx == g.gd.len() {
        idx = 0;
    }
    idx as u16
}

pub(crate) fn tt_add_glyph(g: &mut tt_glyphs, gid: u16, new_gid: u16) -> u16 {
    if g.used_slot[(new_gid as i32 / 8) as usize] as i32 & 1 << 7 - new_gid as i32 % 8 != 0 {
        warn!("Slot {} already used.", new_gid);
    } else {
        if g.gd.len() + 1 >= 65534 {
            panic!("Too many glyphs.");
        }
        g.gd.push(tt_glyph_desc {
            gid: new_gid,
            ogid: gid,
            advw: 0,
            advh: 0,
            lsb: 0,
            tsb: 0,
            llx: 0,
            lly: 0,
            urx: 0,
            ury: 0,
            data: Vec::new(),
        });

        g.used_slot[(new_gid as i32 / 8) as usize] |= (1 << 7 - new_gid as i32 % 8) as u8;
    }
    if new_gid as i32 > g.last_gid as i32 {
        g.last_gid = new_gid
    }
    new_gid
}
/*
 * Initialization
 */

impl tt_glyphs {
    pub(crate) fn init() -> Self {
        let mut g = Self {
            last_gid: 0,
            emsize: 1,
            dw: 0,
            default_advh: 0,
            default_tsb: 0,
            gd: Vec::new(),
            used_slot: vec![0; 8192],
        };
        tt_add_glyph(&mut g, 0_u16, 0_u16);
        g
    }
}

pub(crate) fn tt_build_tables(sfont: &mut sfnt, g: &mut tt_glyphs) -> i32 {
    /* some information available from other TrueType table */
    /* temp */
    if sfont.type_0 != 1 << 0 && sfont.type_0 != 1 << 4 && sfont.type_0 != 1 << 8 {
        panic!("Invalid font type");
    }
    if g.gd.len() > 65534 {
        panic!("Too many glyphs.");
    }
    /*
     * Read head, hhea, maxp, loca:
     *
     *   unitsPerEm       --> head
     *   numHMetrics      --> hhea
     *   indexToLocFormat --> head
     *   numGlyphs        --> maxp
     */
    let mut head = tt_read_head_table(sfont);
    let mut hhea = tt_read_hhea_table(sfont);
    let mut maxp = tt_read_maxp_table(sfont);
    if hhea.metricDataFormat != 0 {
        panic!("Unknown metricDataFormat.");
    }
    g.emsize = head.unitsPerEm;
    sfnt_locate_table(sfont, sfnt_table_info::HMTX);
    let hmtx = tt_read_longMetrics(
        &mut &*sfont.handle,
        maxp.numGlyphs,
        hhea.numOfLongHorMetrics,
        hhea.numOfExSideBearings,
    );
    let os2 = tt_read_os2__table(sfont);
    g.default_advh = (os2.sTypoAscender as i32 - os2.sTypoDescender as i32) as u16;
    g.default_tsb = (g.default_advh as i32 - os2.sTypoAscender as i32) as i16;

    let vmtx = if sfnt_find_table_pos(sfont, b"vmtx") > 0_u32 {
        let vhea = tt_read_vhea_table(sfont);
        sfnt_locate_table(sfont, b"vmtx");
        Some(tt_read_longMetrics(
            &mut &*sfont.handle,
            maxp.numGlyphs,
            vhea.numOfLongVerMetrics,
            vhea.numOfExSideBearings,
        ))
    } else {
        None
    };
    sfnt_locate_table(sfont, sfnt_table_info::LOCA);
    let mut location = Vec::<u32>::with_capacity(maxp.numGlyphs as usize + 1); /* Estimate most frequently appeared width */
    let handle = &mut &*sfont.handle;
    if head.indexToLocFormat as i32 == 0 {
        for _ in 0..=maxp.numGlyphs as i32 {
            location.push(2 * (u16::get(handle) as u32));
        }
    } else if head.indexToLocFormat as i32 == 1 {
        for _ in 0..=maxp.numGlyphs as i32 {
            location.push(u32::get(handle));
        }
    } else {
        panic!("Unknown IndexToLocFormat.");
    }
    let mut w_stat = vec![0_u16; g.emsize as usize + 2];
    /*
     * Read glyf table.
     */
    let offset = sfnt_locate_table(sfont, sfnt_table_info::GLYF);
    /*
     * The num_glyphs may grow when composite glyph is found.
     * A component of glyph refered by a composite glyph is appended
     * to used_glyphs if it is not already registered in used_glyphs.
     * Glyph programs of composite glyphs are modified so that it
     * correctly refer to new gid of their components.
     */
    /* old gid */
    for i in 0..65534 {
        if i >= g.gd.len() {
            break;
        }
        let gid = g.gd[i].ogid;
        if gid as i32 >= maxp.numGlyphs as i32 {
            panic!("Invalid glyph index (gid {})", gid);
        }
        let loc = location[gid as usize];
        let len = location[gid as usize + 1] - loc;
        g.gd[i].advw = hmtx[gid as usize].advance;
        g.gd[i].lsb = hmtx[gid as usize].sideBearing;
        if let Some(vmtx) = vmtx.as_ref() {
            g.gd[i].advh = vmtx[gid as usize].advance;
            g.gd[i].tsb = vmtx[gid as usize].sideBearing
        } else {
            g.gd[i].advh = g.default_advh;
            g.gd[i].tsb = g.default_tsb
        }
        g.gd[i].data = Vec::new();
        if g.gd[i].advw as i32 <= g.emsize as i32 {
            w_stat[g.gd[i].advw as usize] += 1;
        } else {
            w_stat[g.emsize as usize + 1] += 1;
            /* larger than em */
        }
        if !(len == 0) {
            if len < 10 {
                panic!("Invalid TrueType glyph data (gid {}).", gid);
            }
            g.gd[i].data = Vec::with_capacity(len as usize);
            let handle = &mut &*sfont.handle;
            handle
                .seek(SeekFrom::Start(offset as u64 + loc as u64))
                .unwrap();
            let number_of_contours = i16::get(handle);
            g.gd[i].data.put_be(number_of_contours);
            /* BoundingBox: i16 x 4 */
            let llx = i16::get(handle);
            let lly = i16::get(handle);
            let urx = i16::get(handle);
            let ury = i16::get(handle);
            g.gd[i].llx = llx;
            g.gd[i].lly = lly;
            g.gd[i].urx = urx;
            g.gd[i].ury = ury;
            /* _FIXME_ */
            if vmtx.is_none() {
                /* vertOriginY == sTypeAscender */
                g.gd[i].tsb = (g.default_advh as i32 - g.default_tsb as i32 - ury as i32) as i16
            }
            let p = &mut g.gd[i].data;
            p.put_be(llx);
            p.put_be(lly);
            p.put_be(urx);
            p.put_be(ury);
            /* Read everything else. */

            p.resize(len as usize, 0);
            handle.read_exact(&mut p[10..]).unwrap();
            /*
             * Fix GIDs of composite glyphs.
             */
            if number_of_contours < 0 {
                let mut n = 10;
                loop {
                    if n >= len as usize {
                        panic!("Invalid TrueType glyph data (gid {}): {} bytes", gid, len);
                    }
                    /*
                     * Flags and gid of component glyph are both u16.
                     */
                    let flags = u16::from_be_byte_slice(&g.gd[i].data[n..n + 2]); /* flag, gid of a component */
                    n += 2;
                    let cgid = u16::from_be_byte_slice(&g.gd[i].data[n..n + 2]);
                    if cgid as i32 >= maxp.numGlyphs as i32 {
                        panic!(
                            "Invalid gid ({} > {}) in composite glyph {}.",
                            cgid, maxp.numGlyphs, gid,
                        );
                    }
                    let mut new_gid = tt_find_glyph(g, cgid);
                    if new_gid == 0 {
                        new_gid = tt_add_glyph(g, cgid, find_empty_slot(g))
                    }
                    g.gd[i].data[n..n + 2].copy_from_slice(&new_gid.to_be_bytes()[..]);
                    /*
                     * Just skip remaining part.
                     */
                    n += if flags as i32 & 1 << 0 != 0 { 4 } else { 2 };
                    if flags as i32 & 1 << 3 != 0 {
                        /* F2Dot14 */
                        n += 2;
                    } else if flags as i32 & 1 << 6 != 0 {
                        /* F2Dot14 x 2 */
                        n += 4;
                    } else if flags as i32 & 1 << 7 != 0 {
                        /* F2Dot14 x 4 */
                        n += 8;
                    }
                    if !(flags as i32 & 1 << 5 != 0) {
                        break;
                    }
                }
            }
        }
        /* Does not contains any data. */
    }
    let mut max_count: i32 = -1;
    g.dw = g.gd[0].advw;
    for i in 0..g.emsize as i32 + 1 {
        if w_stat[i as usize] as i32 > max_count {
            max_count = w_stat[i as usize] as i32;
            g.dw = i as u16
        }
    }
    g.gd.sort_unstable_by_key(|sv| sv.gid);
    let mut glyf_table_size = 0u64 as u32;
    let mut num_hm_known = 0;
    let last_advw = g.gd[g.gd.len() - 1].advw;
    for i in g.gd.iter().rev() {
        let padlen = (if i.data.len() % 4 != 0 {
            4u32 - (i.data.len() as u32 % 4)
        } else {
            0
        }) as i32;
        glyf_table_size += (i.data.len() as u32) + (padlen as u32);
        if num_hm_known == 0 && last_advw as i32 != i.advw as i32 {
            hhea.numOfLongHorMetrics = (i.gid as i32 + 2) as u16;
            num_hm_known = 1
        }
    }
    /* All advance widths are same. */
    if num_hm_known == 0 {
        hhea.numOfLongHorMetrics = 1_u16
    }
    let hmtx_table_size =
        (hhea.numOfLongHorMetrics as i32 * 2 + (g.last_gid as i32 + 1) * 2) as u32;
    /*
     * Choosing short format does not always give good result
     * when compressed. Sometimes increases size.
     */
    let loca_table_size = if (glyf_table_size as u64) < 0x20000 {
        head.indexToLocFormat = 0_i16;
        ((g.last_gid as i32 + 2) * 2) as u32
    } else {
        head.indexToLocFormat = 1_i16;
        ((g.last_gid as i32 + 2) * 4) as u32
    };
    let mut hmtx_table_data = Vec::<u8>::with_capacity(hmtx_table_size as usize);
    let mut loca_table_data = Vec::<u8>::with_capacity(loca_table_size as usize);
    let mut glyf_table_data = Vec::<u8>::with_capacity(glyf_table_size as usize);
    let mut offset = 0u64 as u32;
    let mut prev = 0_u16;
    for i in &mut g.gd {
        let gap = i.gid as i32 - prev as i32 - 1;
        for j in 1..=gap {
            if prev as i32 + j == hhea.numOfLongHorMetrics as i32 - 1 {
                hmtx_table_data.put_be(last_advw as u16);
            } else if prev as i32 + j < hhea.numOfLongHorMetrics as i32 {
                hmtx_table_data.put_be(0_u16);
            }
            hmtx_table_data.put_be(0_u16);
            if head.indexToLocFormat as i32 == 0 {
                loca_table_data.put_be((offset / 2) as u16);
            } else {
                loca_table_data.put_be(offset as u32);
            }
        }
        let padlen = (if i.data.len() % 4 != 0 {
            4u32 - (i.data.len() as u32 % 4)
        } else {
            0
        }) as i32;
        if (i.gid as i32) < hhea.numOfLongHorMetrics as i32 {
            hmtx_table_data.put_be(i.advw as u16);
        }
        hmtx_table_data.put_be(i.lsb as i16);
        if head.indexToLocFormat as i32 == 0 {
            loca_table_data.put_be((offset / 2) as u16);
        } else {
            loca_table_data.put_be(offset as u32);
        }
        glyf_table_data.extend(&i.data);
        glyf_table_data.resize((offset as usize) + i.data.len() + (padlen as usize), 0);
        offset += (i.data.len() as u32) + (padlen as u32);
        prev = i.gid;
        /* free data here since it consume much memory */
        i.data = Vec::new();
    }
    if head.indexToLocFormat as i32 == 0 {
        loca_table_data.put_be((offset / 2) as u16);
    } else {
        loca_table_data.put_be(offset as u32);
    }
    sfnt_set_table(sfont, sfnt_table_info::HMTX, hmtx_table_data);
    sfnt_set_table(sfont, sfnt_table_info::LOCA, loca_table_data);
    sfnt_set_table(sfont, sfnt_table_info::GLYF, glyf_table_data);
    head.checkSumAdjustment = 0_u32;
    maxp.numGlyphs = (g.last_gid as i32 + 1) as u16;
    /* TODO */
    sfnt_set_table(sfont, sfnt_table_info::MAXP, tt_pack_maxp_table(&maxp));
    sfnt_set_table(sfont, sfnt_table_info::HHEA, tt_pack_hhea_table(&hhea));
    sfnt_set_table(sfont, sfnt_table_info::HEAD, tt_pack_head_table(&head));
    0
}
/* GID in original font */
/* optimal value for DW */
/* default value */
/* default value */

pub(crate) fn tt_get_metrics(sfont: &sfnt, g: &mut tt_glyphs) -> i32 {
    /* temp */
    if sfont.type_0 != 1 << 0 && sfont.type_0 != 1 << 4 && sfont.type_0 != 1 << 8 {
        panic!("Invalid font type");
    }
    /*
     * Read head, hhea, maxp, loca:
     *
     *   unitsPerEm       --> head
     *   numHMetrics      --> hhea
     *   indexToLocFormat --> head
     *   numGlyphs        --> maxp
     */
    let head = tt_read_head_table(sfont);
    let hhea = tt_read_hhea_table(sfont);
    let maxp = tt_read_maxp_table(sfont);
    if hhea.metricDataFormat as i32 != 0 {
        panic!("Unknown metricDataFormat.");
    }
    g.emsize = head.unitsPerEm;
    sfnt_locate_table(sfont, sfnt_table_info::HMTX);
    let hmtx = tt_read_longMetrics(
        &mut &*sfont.handle,
        maxp.numGlyphs,
        hhea.numOfLongHorMetrics,
        hhea.numOfExSideBearings,
    );
    let os2 = tt_read_os2__table(sfont);
    g.default_advh = (os2.sTypoAscender as i32 - os2.sTypoDescender as i32) as u16;
    g.default_tsb = (g.default_advh as i32 - os2.sTypoAscender as i32) as i16;
    let vmtx = if sfnt_find_table_pos(sfont, b"vmtx") > 0_u32 {
        let vhea = tt_read_vhea_table(sfont);
        sfnt_locate_table(sfont, b"vmtx");
        Some(tt_read_longMetrics(
            &mut &*sfont.handle,
            maxp.numGlyphs,
            vhea.numOfLongVerMetrics,
            vhea.numOfExSideBearings,
        ))
    } else {
        None
    };
    sfnt_locate_table(sfont, sfnt_table_info::LOCA);
    let mut location = Vec::<u32>::with_capacity(maxp.numGlyphs as usize + 1);
    let handle = &mut &*sfont.handle;
    if head.indexToLocFormat as i32 == 0 {
        for _ in 0..=maxp.numGlyphs as u32 {
            location.push(2 * (u16::get(handle) as u32));
        }
    } else if head.indexToLocFormat as i32 == 1 {
        for _ in 0..=maxp.numGlyphs as u32 {
            location.push(u32::get(handle));
        }
    } else {
        panic!("Unknown IndexToLocFormat.");
    }
    let mut w_stat = vec![0_u16; g.emsize as usize + 2];
    /*
     * Read glyf table.
     */
    let offset = sfnt_locate_table(sfont, sfnt_table_info::GLYF); /* old gid */
    for i in &mut g.gd {
        let gid = i.ogid;
        if gid as i32 >= maxp.numGlyphs as i32 {
            panic!("Invalid glyph index (gid {})", gid);
        }
        let loc = location[gid as usize];
        let len = location[gid as usize + 1] - loc;
        i.advw = hmtx[gid as usize].advance;
        i.lsb = hmtx[gid as usize].sideBearing;
        if let Some(vmtx) = vmtx.as_ref() {
            i.advh = vmtx[gid as usize].advance;
            i.tsb = vmtx[gid as usize].sideBearing
        } else {
            i.advh = g.default_advh;
            i.tsb = g.default_tsb
        }
        i.data = Vec::new();
        if i.advw as i32 <= g.emsize as i32 {
            w_stat[i.advw as usize] += 1;
        } else {
            w_stat[g.emsize as usize + 1] += 1;
            /* larger than em */
        }
        if !(len == 0_u32) {
            if len < 10_u32 {
                panic!("Invalid TrueType glyph data (gid {}).", gid);
            }
            let handle = &mut &*sfont.handle;
            handle
                .seek(SeekFrom::Start(offset as u64 + loc as u64))
                .unwrap();
            i16::get(handle);
            /* BoundingBox: i16 x 4 */
            i.llx = i16::get(handle);
            i.lly = i16::get(handle);
            i.urx = i16::get(handle);
            i.ury = i16::get(handle);
            /* _FIXME_ */
            if vmtx.is_none() {
                /* vertOriginY == sTypeAscender */
                i.tsb = (g.default_advh as i32 - g.default_tsb as i32 - i.ury as i32) as i16
            }
        }
        /* Does not contains any data. */
    }
    let mut max_count: i32 = -1;
    g.dw = g.gd[0].advw;
    for i in 0..(g.emsize as i32 + 1) as u32 {
        if w_stat[i as usize] as i32 > max_count {
            max_count = w_stat[i as usize] as i32;
            g.dw = i as u16
        }
    }
    0
}
