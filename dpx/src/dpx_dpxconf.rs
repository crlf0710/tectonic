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

pub(crate) type __off_t = i64;
pub(crate) type __off64_t = i64;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct paper<'a> {
    pub(crate) name: &'a [u8],
    pub(crate) pswidth: f64,
    pub(crate) psheight: f64,
}

pub(crate) static mut paperspecs: [paper; 21] = [
    paper {
        name: b"letter",
        pswidth: 612.,
        psheight: 792.,
    },
    paper {
        name: b"legal",
        pswidth: 612.,
        psheight: 1008.,
    },
    paper {
        name: b"ledger",
        pswidth: 1224.,
        psheight: 792.,
    },
    paper {
        name: b"tabloid",
        pswidth: 792.,
        psheight: 1224.,
    },
    paper {
        name: b"a6",
        pswidth: 297.638,
        psheight: 419.528,
    },
    paper {
        name: b"a5",
        pswidth: 419.528,
        psheight: 595.276,
    },
    paper {
        name: b"a4",
        pswidth: 595.276,
        psheight: 841.89,
    },
    paper {
        name: b"a3",
        pswidth: 841.89,
        psheight: 1190.55,
    },
    paper {
        name: b"b6",
        pswidth: 364.25,
        psheight: 515.91,
    },
    paper {
        name: b"b5",
        pswidth: 515.91,
        psheight: 728.5,
    },
    paper {
        name: b"b4",
        pswidth: 728.5,
        psheight: 1031.81,
    },
    paper {
        name: b"b3",
        pswidth: 1031.81,
        psheight: 1457.,
    },
    paper {
        name: b"b5var",
        pswidth: 515.91,
        psheight: 651.97,
    },
    paper {
        name: b"jisb6",
        pswidth: 364.25,
        psheight: 515.91,
    },
    paper {
        name: b"jisb5",
        pswidth: 515.91,
        psheight: 728.5,
    },
    paper {
        name: b"jisb4",
        pswidth: 728.5,
        psheight: 1031.81,
    },
    paper {
        name: b"jisb3",
        pswidth: 1031.81,
        psheight: 1457.,
    },
    paper {
        name: b"isob6",
        pswidth: 354.331,
        psheight: 498.898,
    },
    paper {
        name: b"isob5",
        pswidth: 498.898,
        psheight: 708.661,
    },
    paper {
        name: b"isob4",
        pswidth: 708.661,
        psheight: 1000.63,
    },
    paper {
        name: b"isob3",
        pswidth: 1000.63,
        psheight: 1417.32,
    },
];

pub(crate) unsafe fn paperinfo(ppformat: &[u8]) -> Option<*const paper> {
    if ppformat.is_empty() {
        return None;
    }
    for ppinfo in &paperspecs {
        if ppformat == ppinfo.name {
            return Some(ppinfo as *const paper);
        }
    }
    None
}
/* HAVE_LIBPAPER */
/* HAVE_LIBPAPER */
/*
pub(crate) unsafe fn dumppaperinfo() {
    for ppinfo in &paperspecs {
        let wd = ppinfo.pswidth;
        let ht = ppinfo.psheight;
        println!(
            "{}: {:.2} {:.2} ({:.2}mm {:.2}mm)",
            ppinfo.display(),
            wd,
            ht,
            25.4 * wd / 72.,
            25.4 * ht / 72.,
        );
    }
}*/
