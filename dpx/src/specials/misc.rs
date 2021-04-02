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

use crate::dpx_mfileio::tt_mfreadln;
use crate::dpx_mpost::mps_scan_bbox;
use crate::dpx_pdfdev::{pdf_dev_put_image, transform_info, transform_info_clear};
use crate::dpx_pdfdoc::PdfPageBoundary;
use crate::dpx_pdfparse::SkipWhite;
use crate::dpx_pdfximage::pdf_ximage_findresource;
use crate::spc_warn;
use bridge::{InFile, TTInputFormat};
use std::ptr;

use super::{Handler, SpcArg, SpcEnv};
use super::{Result, ERR, ERROR};

use crate::dpx_pdfximage::load_options;

fn parse_postscriptbox_special(buf: &str) -> std::result::Result<(f64, f64, String), ()> {
    // TODO: port this to nom?
    let mut parts = Vec::new();
    for elem in buf.split("}") {
        if elem == "" {
            // "{a}{b}".split("}") is ["{a", "{b", ""]
            continue;
        }
        if let Some(el) = elem.strip_prefix("{") {
            parts.push(el);
        } else {
            return Err(());
        }
    }

    if parts.len() != 3 {
        return Err(());
    }

    let width = parts[0].parse::<f64>().map_err(|_| ())?;
    let height = parts[1].parse::<f64>().map_err(|_| ())?;
    let filename = parts[2];
    Ok((width, height, filename.to_string()))
}

/* quasi-hack to get the primary input */
unsafe fn spc_handler_postscriptbox(spe: &mut SpcEnv, ap: &mut SpcArg) -> Result<()> {
    let mut ti = transform_info::new();
    let mut buf: [u8; 512] = [0; 512];
    if ap.cur.is_empty() {
        spc_warn!(
            spe,
            "No width/height/filename given for postscriptbox special."
        );
        return ERR;
    }
    /* input is not NULL terminated */
    let len = ap.cur.len();
    let len = if 511 < len { 511 } else { len };
    buf[..len].copy_from_slice(&ap.cur[..len]);
    buf[len] = 0;
    transform_info_clear(&mut ti);

    let command = std::str::from_utf8(&buf).expect("non-utf8 postscriptbox special");

    spc_warn!(spe, "{}", command);

    let filename = if let Ok((width, height, filename)) = parse_postscriptbox_special(command) {
        ti.width = width;
        ti.height = height;
        filename
    } else {
        spc_warn!(spe, "Syntax error in postscriptbox special?");
        return ERR;
    };

    ap.cur = &[];
    ti.width *= 72.0f64 / 72.27f64;
    ti.height *= 72.0f64 / 72.27f64;
    if let Some(mut handle) = InFile::open(&filename, TTInputFormat::PICT, 0) {
        ti.flags |= 1 << 1 | 1 << 2;
        loop {
            if let Ok(buf) = tt_mfreadln(512, &mut handle) {
                let mut p = buf.as_slice();
                if !(mps_scan_bbox(&mut p, &mut ti.bbox) >= 0) {
                    continue;
                }
                ti.flags |= 1 << 0;
                break;
            } else {
                break;
            }
        }
        let options: load_options = load_options {
            page_no: 1,
            bbox_type: PdfPageBoundary::Auto,
            dict: ptr::null_mut(),
        };
        let form_id = pdf_ximage_findresource(&filename, options);
        if form_id < 0 {
            spc_warn!(spe, "Failed to load image file: {}", filename);
            return ERR;
        }
        pdf_dev_put_image(form_id, &mut ti, spe.x_user, spe.y_user);
        Ok(())
    } else {
        spc_warn!(spe, "Could not open image file: {}", filename);
        return ERR;
    }
}
unsafe fn spc_handler_null(_spe: &mut SpcEnv, args: &mut SpcArg) -> Result<()> {
    args.cur = &[];
    Ok(())
}

static MISC_HANDLERS: phf::Map<&'static str, Handler> = phf::phf_map! {
    "postscriptbox" => spc_handler_postscriptbox,
    "landscape" => spc_handler_null,
    "papersize" => spc_handler_null,
    "src:" => spc_handler_null,
    "pos:" => spc_handler_null,
    "om:" => spc_handler_null,
};

pub(crate) fn spc_misc_check_special(mut buf: &[u8]) -> bool {
    buf.skip_white();
    for key in MISC_HANDLERS.keys() {
        if buf.starts_with(key.as_bytes()) {
            return true;
        }
    }
    false
}

pub(crate) unsafe fn spc_misc_setup_handler(
    _spe: &mut SpcEnv,
    args: &mut SpcArg,
) -> Result<Handler> {
    args.cur.skip_white();
    let key = args.cur;
    let mut keylen = 0;
    for &c in args.cur {
        if !(c as u8).is_ascii_alphabetic() {
            break;
        }
        keylen += 1;
    }
    args.cur = &args.cur[keylen..];
    if !args.cur.is_empty() && args.cur[0] == b':' {
        args.cur = &args.cur[1..];
        keylen += 1;
    }
    if keylen < 1 {
        return ERROR();
    }
    for (hkey, &exec) in MISC_HANDLERS.entries() {
        if &key[..keylen] == hkey.as_bytes() {
            args.cur.skip_white();
            args.command = Some(hkey);
            return Ok(exec);
        }
    }
    ERROR()
}
