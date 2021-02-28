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

use euclid::point2;

use super::dpx_dvi::{
    dvi_close, dvi_comment, dvi_do_page, dvi_init, dvi_npages, dvi_reset_global_state,
    dvi_scan_specials, dvi_set_verbose, ReadLength,
};
use super::dpx_pdfdev::{
    pdf_close_device, pdf_dev_reset_global_state, pdf_dev_set_verbose, pdf_init_device, Point, Rect,
};
use super::dpx_pdfdoc::pdf_doc_mut;
use super::dpx_pdfdoc::{pdf_doc_set_creator, pdf_doc_set_verbose};
use super::dpx_pdffont::{
    pdf_font_reset_unique_tag_state, pdf_font_set_deterministic_unique_tags, pdf_font_set_dpi,
};
use super::dpx_tt_aux::tt_aux_set_verbose;
use crate::bridge::DisplayExt;
use crate::dpx_pdfparse::parse_unsigned;
use crate::info;
use std::ffi::CStr;
use std::ptr;

use super::dpx_cid::CIDFont_set_flags;
use super::dpx_dpxconf::paperinfo;
use super::dpx_dpxfile::dpx_delete_old_cache;
use super::dpx_error::shut_up;
use super::dpx_fontmap::{
    pdf_close_fontmaps, pdf_fontmap_set_verbose, pdf_init_fontmaps, pdf_load_fontmap_file,
};
use super::dpx_pdfencrypt::{pdf_enc_compute_id_string, pdf_enc_set_passwd, pdf_enc_set_verbose};
use super::dpx_pdfobj::{
    pdf_files_close, pdf_files_init, pdf_get_version, pdf_obj_reset_global_state,
    pdf_obj_set_verbose, pdf_set_compression, pdf_set_use_predictor, pdf_set_version,
};
use super::dpx_tfm::tfm_reset_global_state;
use super::dpx_vf::vf_reset_global_state;
use crate::specials::{
    spc_exec_at_begin_document, spc_exec_at_end_document, tpic::tpic_set_fill_mode,
};
use libc::{atoi, free, strlen};

use std::borrow::Cow;

pub struct XdvipdfmxConfig {
    pub paperspec: Cow<'static, str>,
}

pub(crate) type PageRange = page_range;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct page_range {
    pub(crate) first: i32,
    pub(crate) last: i32,
}

pub(crate) static mut is_xdv: i32 = 0;

pub(crate) static mut translate_origin: i32 = 0;
static mut ignore_colors: i8 = 0_i8;
static mut annot_grow: f64 = 0.0f64;
static mut bookmark_open: i32 = 0;
static mut mag: f64 = 1.0f64;
static mut font_dpi: i32 = 600;
/*
 * Precision is essentially limited to 0.01pt.
 * See, dev_set_string() in pdfdev.c.
 */
static mut pdfdecimaldigits: i32 = 3;
/* Image cache life in hours */
/*  0 means erase all old images and leave new images */
/* -1 means erase all old images and also erase new images */
/* -2 means ignore image cache (default) */
static mut image_cache_life: i32 = -2;
/* Encryption */
static mut do_encryption: i32 = 0;
static mut key_bits: i32 = 40;
static mut permission: i32 = 0x3c;
/* Page device */

pub(crate) static mut paper_width: f64 = 595.0f64;

pub(crate) static mut paper_height: f64 = 842.0f64;
static mut x_offset: f64 = 72.0f64;
static mut y_offset: f64 = 72.0f64;

pub(crate) static mut landscape_mode: i32 = 0;

pub(crate) static mut always_embed: i32 = 0;
unsafe fn select_paper(paperspec_str: &str) {
    let paperspec = paperspec_str.as_bytes();
    let mut error: i32 = 0;
    paper_width = 0.;
    paper_height = 0.;
    if let Some(pi) = paperinfo(paperspec) {
        paper_width = (*pi).pswidth;
        paper_height = (*pi).psheight;
    } else {
        let comma = paperspec
            .iter()
            .position(|&x| x == b',')
            .unwrap_or_else(|| panic!("Unrecognized paper format: {}", paperspec_str));
        if let (Ok(width), Ok(height)) = (
            (&paperspec[..comma]).read_length_no_mag(),
            (&paperspec[comma + 1..]).read_length_no_mag(),
        ) {
            paper_width = width;
            paper_height = height;
        } else {
            error = -1;
        }
    }
    if error != 0 || paper_width <= 0. || paper_height <= 0. {
        panic!(
            "Invalid paper size: {} ({:.2}x{:.2}",
            paperspec_str, paper_width, paper_height,
        );
    };
}
unsafe fn select_pages(pagespec: *const i8, page_ranges: &mut Vec<PageRange>) {
    let mut p: *const i8 = pagespec;
    while *p as i32 != '\u{0}' as i32 {
        let mut page_range = PageRange { first: 0, last: 0 };

        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        let q = parse_unsigned(&mut p, p.offset(strlen(p) as isize));
        if !q.is_null() {
            /* '-' is allowed here */
            page_range.first = atoi(q) - 1; /* Root node */
            page_range.last = page_range.first;
            free(q as *mut libc::c_void);
        }
        while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
            p = p.offset(1)
        }
        if *p as i32 == '-' as i32 {
            p = p.offset(1);
            while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
                p = p.offset(1)
            }
            page_range.last = -1;
            if *p != 0 {
                let q = parse_unsigned(&mut p, p.offset(strlen(p) as isize));
                if !q.is_null() {
                    page_range.last = atoi(q) - 1;
                    free(q as *mut libc::c_void);
                }
                while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
                    p = p.offset(1)
                }
            }
        } else {
            page_range.last = page_range.first;
        }
        page_ranges.push(page_range);
        if *p as i32 == ',' as i32 {
            p = p.offset(1)
        } else {
            while *p as i32 != 0 && libc::isspace(*p as _) != 0 {
                p = p.offset(1)
            }
            if *p != 0 {
                panic!(
                    "Bad page range specification: {}",
                    CStr::from_ptr(p).display()
                );
            }
        }
    }
}

unsafe fn do_dvi_pages(mut page_ranges: Vec<PageRange>) {
    spc_exec_at_begin_document().ok();
    let mut page_width = paper_width;
    let init_paper_width = page_width;
    let mut page_height = paper_height;
    let init_paper_height = page_height;
    let mut page_count = 0;
    let mut mediabox = Rect::new(Point::zero(), point2(paper_width, paper_height));
    pdf_doc_mut().set_mediabox(0, &mediabox);
    let mut i = 0;
    while i < page_ranges.len() && dvi_npages() != 0 {
        if page_ranges[i].last < 0 {
            page_ranges[i].last += dvi_npages() as i32;
        }
        let step = if page_ranges[i].first <= page_ranges[i].last {
            1
        } else {
            -1
        };
        let mut page_no = page_ranges[i].first;
        while dvi_npages() != 0 {
            if (page_no as u32) < dvi_npages() {
                info!("[{}", page_no + 1);
                /* Users want to change page size even after page is started! */
                page_width = paper_width;
                page_height = paper_height;
                let mut w = page_width;
                let mut h = page_height;
                let mut lm = landscape_mode;
                let mut xo = x_offset;
                let mut yo = y_offset;
                dvi_scan_specials(
                    page_no,
                    &mut w,
                    &mut h,
                    &mut xo,
                    &mut yo,
                    &mut lm,
                    ptr::null_mut(),
                    ptr::null_mut(),
                    ptr::null_mut(),
                    ptr::null_mut(),
                    ptr::null_mut(),
                    ptr::null_mut(),
                    ptr::null_mut(),
                );
                if lm != landscape_mode {
                    let mut _tmp: f64 = w;
                    w = h;
                    h = _tmp;
                    landscape_mode = lm
                }
                if page_width != w || page_height != h {
                    page_width = w;
                    page_height = h
                }
                if x_offset != xo || y_offset != yo {
                    x_offset = xo;
                    y_offset = yo
                }
                if page_width != init_paper_width || page_height != init_paper_height {
                    mediabox = Rect::new(point2(0., 0.), point2(page_width, page_height));
                    pdf_doc_mut().set_mediabox(page_count + 1, &mediabox);
                }
                dvi_do_page(page_height, x_offset, y_offset);
                page_count = page_count + 1;
                info!("]");
            }
            if step > 0 && page_no >= page_ranges[i].last {
                break;
            }
            if step < 0 && page_no <= page_ranges[i].last {
                break;
            }
            page_no += step
        }
        i = i.wrapping_add(1)
    }
    if page_count < 1 {
        panic!("No pages fall in range!");
    }
    spc_exec_at_end_document().ok();
}

pub unsafe fn dvipdfmx_main(
    dpx_config: &XdvipdfmxConfig,
    pdf_filename: &str,
    dvi_filename: &str,
    pagespec: *const i8,
    opt_flags: i32,
    translate: bool,
    compress: bool,
    deterministic_tags: bool,
    quiet: bool,
    verbose: u32,
) -> i32 {
    let mut enable_object_stream: bool = true; /* This must come before parsing options... */
    let mut page_ranges = Vec::new();
    assert!(!pdf_filename.is_empty());
    assert!(!dvi_filename.is_empty());
    translate_origin = translate as i32;
    dvi_reset_global_state();
    tfm_reset_global_state();
    vf_reset_global_state();
    pdf_dev_reset_global_state();
    pdf_obj_reset_global_state();
    pdf_font_reset_unique_tag_state();
    if quiet {
        shut_up(2);
    } else {
        dvi_set_verbose(verbose as i32);
        pdf_dev_set_verbose(verbose as i32);
        pdf_doc_set_verbose(verbose as i32);
        pdf_enc_set_verbose(verbose as i32);
        pdf_obj_set_verbose(verbose as i32);
        pdf_fontmap_set_verbose(verbose as i32);
        tt_aux_set_verbose(verbose as i32);
    }
    pdf_set_compression(if compress as i32 != 0 { 9 } else { 0 });
    pdf_font_set_deterministic_unique_tags(if deterministic_tags as i32 != 0 { 1 } else { 0 });
    pdf_init_fontmaps();
    /* We used to read the config file here. It synthesized command-line
     * arguments, so we emulate the default TeXLive config file by copying those
     * code bits. */
    pdf_set_version(5_u32); /* last page */
    select_paper(&dpx_config.paperspec);
    annot_grow = 0 as f64;
    bookmark_open = 0;
    key_bits = 40;
    permission = 0x3c;
    font_dpi = 600;
    pdfdecimaldigits = 5;
    image_cache_life = -2;
    pdf_load_fontmap_file("pdftex.map", '+' as i32).ok();
    pdf_load_fontmap_file("kanjix.map", '+' as i32).ok();
    pdf_load_fontmap_file("ckx.map", '+' as i32).ok();
    if !pagespec.is_null() {
        select_pages(pagespec, &mut page_ranges);
    }
    if page_ranges.is_empty() {
        page_ranges.push(PageRange { first: 0, last: -1 });
    }
    /*kpse_init_prog("", font_dpi, NULL, NULL);
    kpse_set_program_enabled(kpse_pk_format, true, kpse_src_texmf_cnf);*/
    pdf_font_set_dpi(font_dpi);
    dpx_delete_old_cache(image_cache_life);
    pdf_enc_compute_id_string(
        if dvi_filename.is_empty() {
            None
        } else {
            Some(dvi_filename.as_bytes())
        },
        if pdf_filename.is_empty() {
            None
        } else {
            Some(pdf_filename.as_bytes())
        },
    );
    let mut ver_major: i32 = 0;
    let mut ver_minor: i32 = 0;
    let mut owner_pw: [i8; 127] = [0; 127];
    let mut user_pw: [i8; 127] = [0; 127];
    /* Dependency between DVI and PDF side is rather complicated... */
    let dvi2pts = dvi_init(dvi_filename, mag);
    if dvi2pts == 0.0f64 {
        panic!("dvi_init() failed!");
    }
    pdf_doc_set_creator(dvi_comment());
    dvi_scan_specials(
        0,
        &mut paper_width,
        &mut paper_height,
        &mut x_offset,
        &mut y_offset,
        &mut landscape_mode,
        &mut ver_major,
        &mut ver_minor,
        &mut do_encryption,
        &mut key_bits,
        &mut permission,
        owner_pw.as_mut_ptr(),
        user_pw.as_mut_ptr(),
    );
    if ver_minor >= 3 && ver_minor <= 7 {
        pdf_set_version(ver_minor as u32);
    }
    if do_encryption != 0 {
        if !(key_bits >= 40 && key_bits <= 128 && key_bits % 8 == 0) && key_bits != 256 {
            panic!("Invalid encryption key length specified: {}", key_bits);
        } else {
            if key_bits > 40 && pdf_get_version() < 4_u32 {
                panic!("Chosen key length requires at least PDF 1.4. Use \"-V 4\" to change.");
            }
        }
        do_encryption = 1;
        pdf_enc_set_passwd(
            key_bits as u32,
            permission as u32,
            owner_pw.as_mut_ptr(),
            user_pw.as_mut_ptr(),
        );
    }
    if landscape_mode != 0 {
        let mut _tmp: f64 = paper_width;
        paper_width = paper_height;
        paper_height = _tmp
    }
    pdf_files_init();
    if opt_flags & 1 << 6 != 0 {
        enable_object_stream = false
    }
    /* Set default paper size here so that all page's can inherite it.
     * annot_grow:    Margin of annotation.
     * bookmark_open: Miximal depth of open bookmarks.
     */
    pdf_doc_mut().open_document(
        pdf_filename,
        do_encryption != 0,
        enable_object_stream,
        paper_width,
        paper_height,
        annot_grow,
        bookmark_open,
        (opt_flags & 1 << 4 == 0) as i32,
    );
    /* Ignore_colors placed here since
     * they are considered as device's capacity.
     */
    pdf_init_device(dvi2pts, pdfdecimaldigits, ignore_colors as i32);
    if opt_flags & 1 << 2 != 0 {
        CIDFont_set_flags(1 << 1);
    }
    /* Please move this to spc_init_specials(). */
    if opt_flags & 1 << 1 != 0 {
        tpic_set_fill_mode(1); /* No prediction */
    }
    if opt_flags & 1 << 5 != 0 {
        pdf_set_use_predictor(0);
    }
    do_dvi_pages(page_ranges);
    pdf_files_close();
    /* Order of close... */
    pdf_close_device();
    /* pdf_close_document flushes XObject (image) and other resources. */
    pdf_doc_mut().close_document(); /* pdf_font may depend on fontmap. */
    pdf_close_fontmaps();
    dvi_close();
    info!("\n");
    0
}
