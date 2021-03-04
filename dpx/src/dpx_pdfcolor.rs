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

#![allow(non_snake_case)]

use super::dpx_pdfdev::{pdf_dev_get_param, pdf_dev_reset_color};
use crate::dpx_pdfobj::{
    pdf_get_version, pdf_link_obj, pdf_obj, pdf_ref_obj, pdf_release_obj, pdf_stream, IntoObj,
    IntoRef, PushObj, STREAM_COMPRESS,
};
use crate::shims::sprintf;
use crate::{info, warn, FromBEByteSlice};
use md5::{Digest, Md5};
use std::error::Error;
use std::fmt;
use std::ptr;

#[derive(Debug)]
pub(crate) enum PdfColorError {
    InvalidValue { name: &'static str, value: f64 },
    EmptyName,
}

impl PdfColorError {
    pub(crate) fn warn(&self) {
        warn!("{}", self);
    }
}

impl fmt::Display for PdfColorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PdfColorError::InvalidValue { name, value } => {
                write!(f, "Invalid color value specified: {}={}", name, value)
            }
            PdfColorError::EmptyName => write!(f, "Invalid spot color: empty name"),
        }
    }
}

impl Error for PdfColorError {}

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum PdfColor {
    Gray(f64),
    Spot(String, f64),
    Rgb(f64, f64, f64),
    Cmyk(f64, f64, f64, f64),
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) struct RgbPdfColor(f64, f64, f64);

impl RgbPdfColor {
    pub(crate) fn r(&self) -> f64 {
        self.0
    }

    pub(crate) fn g(&self) -> f64 {
        self.1
    }

    pub(crate) fn b(&self) -> f64 {
        self.2
    }

    pub(crate) fn from_hsv(h: f64, s: f64, v: f64) -> RgbPdfColor {
        let mut b = v;
        let mut g = b;
        let mut r = g;
        if s != 0.0 {
            let h6 = h * 6.0;
            let i = h6 as i32;
            let f = h6 - i as f64;
            let v1 = v * (1.0 - s);
            let v2 = v * (1.0 - s * f);
            let v3 = v * (1.0 - s * (1.0 - f));
            match i {
                0 => {
                    r = v;
                    g = v3;
                    b = v1
                }
                1 => {
                    r = v2;
                    g = v;
                    b = v1
                }
                2 => {
                    r = v1;
                    g = v;
                    b = v3
                }
                3 => {
                    r = v1;
                    g = v2;
                    b = v
                }
                4 => {
                    r = v3;
                    g = v1;
                    b = v
                }
                5 => {
                    r = v;
                    g = v1;
                    b = v2
                }
                6 => {
                    r = v;
                    g = v1;
                    b = v2
                }
                _ => {}
            }
        }
        RgbPdfColor(r, g, b)
    }
}

impl From<RgbPdfColor> for PdfColor {
    fn from(RgbPdfColor(r, g, b): RgbPdfColor) -> Self {
        PdfColor::Rgb(r, g, b)
    }
}

pub(crate) const WHITE: PdfColor = PdfColor::Gray(1.0);
pub(crate) const BLACK: PdfColor = PdfColor::Gray(0.0);

impl PdfColor {
    pub(crate) fn from_gray(value: f64) -> Result<Self, PdfColorError> {
        if value >= 0.0 && value <= 1.0 {
            Ok(Self::Gray(value))
        } else {
            Err(PdfColorError::InvalidValue {
                name: "gray",
                value,
            })
        }
    }

    pub(crate) fn from_rgb(r: f64, g: f64, b: f64) -> Result<Self, PdfColorError> {
        if r < 0.0 || r > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "red",
                value: r,
            })
        } else if g < 0.0 || g > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "green",
                value: g,
            })
        } else if b < 0.0 || b > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "blue",
                value: b,
            })
        } else {
            Ok(PdfColor::Rgb(r, g, b))
        }
    }

    pub(crate) fn from_cmyk(c: f64, m: f64, y: f64, k: f64) -> Result<Self, PdfColorError> {
        if c < 0.0 || c > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "cyan",
                value: c,
            })
        } else if m < 0.0 || m > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "magenta",
                value: m,
            })
        } else if y < 0.0f64 || y > 1.0f64 {
            Err(PdfColorError::InvalidValue {
                name: "yellow",
                value: y,
            })
        } else if k < 0.0 || k > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "black",
                value: k,
            })
        } else {
            Ok(PdfColor::Cmyk(c, m, y, k))
        }
    }

    pub(crate) fn from_spot(name: &str, c: f64) -> Result<Self, PdfColorError> {
        if c < 0.0 || c > 1.0 {
            Err(PdfColorError::InvalidValue {
                name: "grade",
                value: c,
            })
        } else if name.is_empty() {
            Err(PdfColorError::EmptyName)
        } else {
            Ok(PdfColor::Spot(name.to_string(), c))
        }
    }

    /// Brighten up a color. f == 0 means no change, f == 1 means white.
    pub(crate) fn brightened(self, f: f64) -> Self {
        if f == 1.0 {
            WHITE
        } else {
            let f1 = if let PdfColor::Cmyk(..) = self {
                1.0
            } else {
                f
            };
            let f0 = 1.0 - f;
            match self {
                PdfColor::Gray(g) => PdfColor::Gray(f0 * g + f1),
                PdfColor::Spot(name, c) => PdfColor::Spot(name, f0 * c + f1),
                PdfColor::Rgb(r, g, b) => PdfColor::Rgb(f0 * r + f1, f0 * g + f1, f0 * b + f1),
                PdfColor::Cmyk(c, m, y, k) => {
                    PdfColor::Cmyk(f0 * c + f1, f0 * m + f1, f0 * y + f1, f0 * k + f1)
                }
            }
        }
    }

    pub(crate) fn is_white(&self) -> bool {
        match self {
            PdfColor::Spot(..) => false,
            PdfColor::Rgb(r, g, b) => [r, g, b].iter().all(|value| **value == 1.0),
            PdfColor::Cmyk(c, m, y, k) => [c, m, y, k].iter().all(|value| **value == 0.0),
            PdfColor::Gray(value) => *value == 1.0,
        }
    }

    pub(crate) unsafe fn to_string(&self, mask: u8) -> String {
        let format_float_with_printf_g = |value: f64| {
            // TODO: refactor this ugly hack while preserving sematics of printf %g
            let mut buf = String::from_utf8_lossy(&[0x41; 256]).into_owned();
            let len = sprintf(
                buf.as_mut_ptr() as *mut i8,
                b"%g\0" as *const u8 as *const i8,
                value,
            ) as usize;
            buf.truncate(len);
            buf
        };

        let values_to_string = |values: &[f64]| {
            let mut res = String::new();
            for value in values {
                let value = (value / 0.001 + 0.5).floor() * 0.001;
                res += " ";
                res += &format_float_with_printf_g(value);
            }
            res
        };

        match self {
            PdfColor::Spot(name, c) => format!(
                " /{} {} {} {} {}{}",
                name,
                ('C' as u8 | mask) as char,
                ('S' as u8 | mask) as char,
                format_float_with_printf_g((c / 0.001 + 0.5).floor() * 0.001),
                ('S' as u8 | mask) as char,
                ('C' as u8 | mask) as char,
            ),
            PdfColor::Cmyk(c, m, y, k) => values_to_string(&[*c, *m, *y, *k]),
            PdfColor::Rgb(r, g, b) => values_to_string(&[*r, *g, *b]),
            PdfColor::Gray(g) => values_to_string(&[*g]),
        }
    }

    pub(crate) fn named(name: &str) -> Option<PdfColor> {
        COLORDEFS
            .as_ref()
            .iter()
            .find(|&colordef| colordef.key == name)
            .map(|colordef| colordef.color.clone())
    }
}

/* Color names */
struct Colordef {
    key: &'static str,
    color: PdfColor,
}

impl Colordef {
    const fn new(key: &'static str, color: PdfColor) -> Self {
        Colordef { key, color }
    }
}

const COLORDEFS: [Colordef; 68] = [
    Colordef::new("GreenYellow", PdfColor::Cmyk(0.15, 0.0, 0.69, 0.0)),
    Colordef::new("Yellow", PdfColor::Cmyk(0.0, 0.0, 1.0, 0.0)),
    Colordef::new("Goldenrod", PdfColor::Cmyk(0.0, 0.1, 0.84, 0.0)),
    Colordef::new("Dandelion", PdfColor::Cmyk(0.0, 0.29, 0.84, 0.0)),
    Colordef::new("Apricot", PdfColor::Cmyk(0.0, 0.32, 0.52, 0.0)),
    Colordef::new("Peach", PdfColor::Cmyk(0.0, 0.5, 0.7, 0.0)),
    Colordef::new("Melon", PdfColor::Cmyk(0.0, 0.46, 0.5, 0.0)),
    Colordef::new("YellowOrange", PdfColor::Cmyk(0.0, 0.42, 1.0, 0.0)),
    Colordef::new("Orange", PdfColor::Cmyk(0.0, 0.61, 0.87, 0.0)),
    Colordef::new("BurntOrange", PdfColor::Cmyk(0.0, 0.51, 1.0, 0.0)),
    Colordef::new("Bittersweet", PdfColor::Cmyk(0.0, 0.75, 1.0, 0.24)),
    Colordef::new("RedOrange", PdfColor::Cmyk(0.0, 0.77, 0.87, 0.0)),
    Colordef::new("Mahogany", PdfColor::Cmyk(0.0, 0.85, 0.87, 0.35)),
    Colordef::new("Maroon", PdfColor::Cmyk(0.0, 0.87, 0.68, 0.32)),
    Colordef::new("BrickRed", PdfColor::Cmyk(0.0, 0.89, 0.94, 0.28)),
    Colordef::new("Red", PdfColor::Cmyk(0.0, 1.0, 1.0, 0.0)),
    Colordef::new("OrangeRed", PdfColor::Cmyk(0.0, 1.0, 0.5, 0.0)),
    Colordef::new("RubineRed", PdfColor::Cmyk(0.0, 1.0, 0.13, 0.0)),
    Colordef::new("WildStrawberry", PdfColor::Cmyk(0.0, 0.96, 0.39, 0.0)),
    Colordef::new("Salmon", PdfColor::Cmyk(0.0, 0.53, 0.38, 0.0)),
    Colordef::new("CarnationPink", PdfColor::Cmyk(0.0, 0.63, 0.0, 0.0)),
    Colordef::new("Magenta", PdfColor::Cmyk(0.0, 1.0, 0.0, 0.0)),
    Colordef::new("VioletRed", PdfColor::Cmyk(0.0, 0.81, 0.0, 0.0)),
    Colordef::new("Rhodamine", PdfColor::Cmyk(0.0, 0.82, 0.0, 0.0)),
    Colordef::new("Mulberry", PdfColor::Cmyk(0.34, 0.90, 0.0, 0.02)),
    Colordef::new("RedViolet", PdfColor::Cmyk(0.07, 0.9, 0.0, 0.34)),
    Colordef::new("Fuchsia", PdfColor::Cmyk(0.47, 0.91, 0.0, 0.08)),
    Colordef::new("Lavender", PdfColor::Cmyk(0.0, 0.48, 0.0, 0.0)),
    Colordef::new("Thistle", PdfColor::Cmyk(0.12, 0.59, 0.0, 0.0)),
    Colordef::new("Orchid", PdfColor::Cmyk(0.32, 0.64, 0.0, 0.0)),
    Colordef::new("DarkOrchid", PdfColor::Cmyk(0.4, 0.8, 0.2, 0.0)),
    Colordef::new("Purple", PdfColor::Cmyk(0.45, 0.86, 0.0, 0.0)),
    Colordef::new("Plum", PdfColor::Cmyk(0.50, 1.0, 0.0, 0.0)),
    Colordef::new("Violet", PdfColor::Cmyk(0.79, 0.88, 0.0, 0.0)),
    Colordef::new("RoyalPurple", PdfColor::Cmyk(0.75, 0.9, 0.0, 0.0)),
    Colordef::new("BlueViolet", PdfColor::Cmyk(0.86, 0.91, 0.0, 0.04)),
    Colordef::new("Periwinkle", PdfColor::Cmyk(0.57, 0.55, 0.0, 0.0)),
    Colordef::new("CadetBlue", PdfColor::Cmyk(0.62, 0.57, 0.23, 0.0)),
    Colordef::new("CornflowerBlue", PdfColor::Cmyk(0.65, 0.13, 0.0, 0.0)),
    Colordef::new("MidnightBlue", PdfColor::Cmyk(0.98, 0.13, 0.0, 0.43)),
    Colordef::new("NavyBlue", PdfColor::Cmyk(0.94, 0.54, 0.0, 0.0)),
    Colordef::new("RoyalBlue", PdfColor::Cmyk(1.0, 0.5, 0.0, 0.0)),
    Colordef::new("Blue", PdfColor::Cmyk(1.0, 1.0, 0.0, 0.0)),
    Colordef::new("Cerulean", PdfColor::Cmyk(0.94, 0.11, 0.0, 0.0)),
    Colordef::new("Cyan", PdfColor::Cmyk(1.0, 0.0, 0.0, 0.0)),
    Colordef::new("ProcessBlue", PdfColor::Cmyk(0.96, 0.0, 0.0, 0.0)),
    Colordef::new("SkyBlue", PdfColor::Cmyk(0.62, 0.0, 0.12, 0.0)),
    Colordef::new("Turquoise", PdfColor::Cmyk(0.85, 0.0, 0.20, 0.0)),
    Colordef::new("TealBlue", PdfColor::Cmyk(0.86, 0.0, 0.34, 0.02)),
    Colordef::new("Aquamarine", PdfColor::Cmyk(0.82, 0.0, 0.3, 0.0)),
    Colordef::new("BlueGreen", PdfColor::Cmyk(0.85, 0.0, 0.33, 0.0)),
    Colordef::new("Emerald", PdfColor::Cmyk(1.0, 0.0, 0.5, 0.0)),
    Colordef::new("JungleGreen", PdfColor::Cmyk(0.99, 0.0, 0.52, 0.0)),
    Colordef::new("SeaGreen", PdfColor::Cmyk(0.69, 0.0, 0.5, 0.0)),
    Colordef::new("Green", PdfColor::Cmyk(1.0, 0.0, 1.0, 0.00f64)),
    Colordef::new("ForestGreen", PdfColor::Cmyk(0.91, 0.0, 0.88, 0.12)),
    Colordef::new("PineGreen", PdfColor::Cmyk(0.92, 0.0, 0.59, 0.25)),
    Colordef::new("LimeGreen", PdfColor::Cmyk(0.5, 0.0, 1.0, 0.0)),
    Colordef::new("YellowGreen", PdfColor::Cmyk(0.44, 0.0, 0.74, 0.0)),
    Colordef::new("SpringGreen", PdfColor::Cmyk(0.26, 0.0, 0.76, 0.0)),
    Colordef::new("OliveGreen", PdfColor::Cmyk(0.64, 0.0, 0.95, 0.40)),
    Colordef::new("RawSienna", PdfColor::Cmyk(0.0, 0.72, 1.0, 0.45)),
    Colordef::new("Sepia", PdfColor::Cmyk(0.0, 0.83, 1.0, 0.7)),
    Colordef::new("Brown", PdfColor::Cmyk(0.0, 0.81, 1.0, 0.6)),
    Colordef::new("Tan", PdfColor::Cmyk(0.14, 0.42, 0.56, 0.0)),
    Colordef::new("Gray", PdfColor::Gray(0.5)),
    Colordef::new("Black", PdfColor::Gray(0.0)),
    Colordef::new("White", PdfColor::Gray(1.0)),
];

#[derive(Clone)]
#[repr(C)]
pub(crate) struct pdf_colorspace {
    pub(crate) ident: String,
    pub(crate) subtype: i32,
    pub(crate) resource: *mut pdf_obj,
    pub(crate) reference: *mut pdf_obj,
    pub(crate) cdata: Option<Box<iccbased_cdata>>,
}
#[allow(non_camel_case_types)]
pub(crate) type iccSig = u32;
/*
 * In ICC profile stream dicrionary, there is /Range whose values must
 * "match the information in the profile". But where is those values in?
 *
 * How should I treat rendering intent?
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct iccbased_cdata {
    pub(crate) sig: i32,
    pub(crate) checksum: [u8; 16],
    pub(crate) colorspace: i32,
    pub(crate) alternate: i32,
    /* alternate colorspace (id), unused */
}
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub(crate) struct iccHeader {
    pub(crate) size: i32,
    pub(crate) CMMType: iccSig,
    pub(crate) version: i32,
    pub(crate) devClass: iccSig,
    pub(crate) colorSpace: iccSig,
    pub(crate) PCS: iccSig,
    pub(crate) creationDate: [u8; 12],
    pub(crate) acsp: iccSig,
    pub(crate) platform: iccSig,
    pub(crate) flags: [u8; 4],
    pub(crate) devMnfct: iccSig,
    pub(crate) devModel: iccSig,
    pub(crate) devAttr: [u8; 8],
    pub(crate) intent: i32,
    pub(crate) illuminant: iccXYZNumber,
    pub(crate) creator: iccSig,
    pub(crate) ID: [u8; 16],
}
#[derive(Copy, Clone, Default)]
#[repr(C)]
pub(crate) struct iccXYZNumber {
    pub(crate) X: i32,
    pub(crate) Y: i32,
    pub(crate) Z: i32,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct IccVersion {
    pub(crate) major: i32,
    pub(crate) minor: i32,
}
impl IccVersion {
    const fn new(major: i32, minor: i32) -> Self {
        Self { major, minor }
    }
}
#[derive(Clone)]
#[repr(C)]
pub(crate) struct ColorStack {
    pub(crate) current: i32,
    pub(crate) stroke: [PdfColor; 128],
    pub(crate) fill: [PdfColor; 128],
}
/* No page independence here...
 */
static mut VERBOSE: i32 = 0;

pub(crate) unsafe fn pdf_color_set_verbose(level: i32) {
    VERBOSE = level;
}

static mut COLOR_STACK: ColorStack = ColorStack {
    current: 0,
    stroke: [BLACK; 128],
    fill: [BLACK; 128],
};

pub(crate) unsafe fn pdf_color_clear_stack() {
    if COLOR_STACK.current > 0 {
        warn!("You\'ve mistakenly made a global color change within nested colors.");
    }
    loop {
        let fresh4 = COLOR_STACK.current;
        COLOR_STACK.current = COLOR_STACK.current - 1;
        if fresh4 == 0 {
            break;
        }
    }
    COLOR_STACK.current = 0;
    COLOR_STACK.stroke[0] = BLACK;
    COLOR_STACK.fill[0] = BLACK;
}

pub(crate) unsafe fn pdf_color_set(sc: &PdfColor, fc: &PdfColor) {
    COLOR_STACK.stroke[COLOR_STACK.current as usize] = sc.clone();
    COLOR_STACK.fill[COLOR_STACK.current as usize] = fc.clone();
    pdf_dev_reset_color(0);
}

pub(crate) unsafe fn pdf_color_push(sc: &mut PdfColor, fc: &PdfColor) {
    if COLOR_STACK.current >= 128 - 1 {
        warn!("Color stack overflow. Just ignore.");
    } else {
        COLOR_STACK.current += 1;
        pdf_color_set(sc, fc);
    };
}

pub(crate) unsafe fn pdf_color_pop() {
    if COLOR_STACK.current <= 0 {
        warn!("Color stack underflow. Just ignore.");
    } else {
        COLOR_STACK.current -= 1;
        pdf_dev_reset_color(0);
    };
}
/* Color special
 * See remark in spc_color.c.
 */
/* Color stack
 */

pub(crate) unsafe fn pdf_color_get_current() -> (&'static mut PdfColor, &'static mut PdfColor) {
    (
        &mut COLOR_STACK.stroke[COLOR_STACK.current as usize],
        &mut COLOR_STACK.fill[COLOR_STACK.current as usize],
    )
}
const ICC_VERSIONS: [IccVersion; 8] = [
    IccVersion::new(0, 0),
    IccVersion::new(0, 0),
    IccVersion::new(0, 0),
    IccVersion::new(0x2, 0x10),
    IccVersion::new(0x2, 0x20),
    IccVersion::new(0x4, 0),
    IccVersion::new(0x4, 0),
    IccVersion::new(0x4, 0x20),
];

unsafe fn iccp_version_supported(major: i32, minor: i32) -> i32 {
    let pdf_ver = pdf_get_version() as i32;
    if pdf_ver < 8 {
        if ICC_VERSIONS[pdf_ver as usize].major < major {
            return 0;
        } else if ICC_VERSIONS[pdf_ver as usize].major == major
            && ICC_VERSIONS[pdf_ver as usize].minor < minor
        {
            return 0;
        } else {
            return 1;
        }
    }
    0
}

fn str2iccSig(p: &[u8]) -> iccSig {
    u32::from_be_byte_slice(p)
}

unsafe fn iccp_init_iccHeader(icch: &mut iccHeader) {
    icch.size = 0;
    icch.CMMType = 0;
    icch.version = 0xffffff;
    icch.devClass = 0;
    icch.colorSpace = 0;
    icch.PCS = 0;
    icch.creationDate = [0; 12];
    icch.acsp = str2iccSig(b"ascp");
    icch.platform = 0;
    icch.flags = [0; 4];
    icch.devMnfct = 0;
    icch.devModel = 0;
    icch.devAttr = [0; 8];
    icch.intent = 0;
    icch.illuminant.X = 0;
    icch.illuminant.Y = 0;
    icch.illuminant.Z = 0;
    icch.creator = 0;
    icch.ID = [0; 16];
}
impl iccbased_cdata {
    fn new() -> Self {
        Self {
            sig: i32::from_be_bytes(*b"iccb"),
            checksum: [0; 16],
            colorspace: 0,
            alternate: -1,
        }
    }
}
unsafe fn get_num_components_iccbased(cdata: &iccbased_cdata) -> i32 {
    let mut num_components = 0;
    assert!(cdata.sig == i32::from_be_bytes(*b"iccb"));
    match (*cdata).colorspace {
        -3 => num_components = 3,
        -4 => num_components = 4,
        -1 => num_components = 1,
        2 => num_components = 3,
        _ => {}
    }
    num_components
}
unsafe fn compare_iccbased(
    ident1: &str,
    cdata1: Option<&iccbased_cdata>,
    ident2: &str,
    cdata2: Option<&iccbased_cdata>,
) -> bool {
    if let (Some(cdata1), Some(cdata2)) = (cdata1, cdata2) {
        assert!(cdata1.sig == i32::from_be_bytes(*b"iccb"));
        assert!(cdata2.sig == i32::from_be_bytes(*b"iccb"));
        if cdata1.checksum != [0; 16] && cdata2.checksum != [0; 16] {
            return cdata1.checksum == cdata2.checksum;
        }
        if cdata1.colorspace != cdata2.colorspace {
            return false;
        }
        /* Continue if checksum unknown and colorspace is same. */
    }
    if !ident1.is_empty() && !ident2.is_empty() {
        return ident1 == ident2;
    }
    /* No way to compare */
    return false; /* acsp */
}

pub(crate) unsafe fn iccp_check_colorspace(colortype: i32, profile: &[u8]) -> i32 {
    if profile.len() < 128 {
        return -1;
    }
    let colorspace = str2iccSig(&profile[16..20]);
    match colortype {
        3 | -3 => {
            if colorspace != str2iccSig(b"RGB ") {
                return -1;
            }
        }
        1 | -1 => {
            if colorspace != str2iccSig(b"GRAY") {
                return -1;
            }
        }
        -4 => {
            if colorspace != str2iccSig(b"CMYK") {
                return -1;
            }
        }
        _ => return -1,
    }
    0
}

pub(crate) unsafe fn iccp_get_rendering_intent(profile: &[u8]) -> *mut pdf_obj {
    if profile.len() < 128 {
        return ptr::null_mut();
    }
    let p = profile.as_ptr();
    let intent = (*p.offset(64) as i32) << 24
        | (*p.offset(65) as i32) << 16
        | (*p.offset(66) as i32) << 8
        | *p.offset(67) as i32;
    match intent >> 16 & 0xff {
        2 => "Saturation".into_obj(),
        0 => "Perceptual".into_obj(),
        3 => "AbsoluteColorimetric".into_obj(),
        1 => "RelativeColorimetric".into_obj(),
        _ => {
            warn!("Invalid rendering intent type: {}", intent >> 16 & 0xff);
            ptr::null_mut()
        }
    }
}
unsafe fn iccp_unpack_header(icch: &mut iccHeader, profile: &[u8], check_size: i32) -> i32 {
    let proflen = profile.len();
    if check_size != 0 {
        if proflen < 128 || proflen % 4 != 0 {
            warn!("Profile size: {}", proflen);
            return -1;
        }
    }
    let mut p = &profile[..128];
    icch.size = u32::from_be_byte_slice(&p[..4]) as i32;
    if check_size != 0 {
        if icch.size != proflen as i32 {
            warn!("ICC Profile size: {}(header) != {}", icch.size, proflen);
            return -1;
        }
    }
    p = &p[4..];
    icch.CMMType = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.version = u32::from_be_byte_slice(&p[..4]) as i32;
    p = &p[4..];
    icch.devClass = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.colorSpace = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.PCS = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.creationDate.copy_from_slice(&p[..12]);
    p = &p[12..];
    icch.acsp = str2iccSig(&p[..4]);
    if icch.acsp != str2iccSig(b"acsp") {
        warn!(
            "Invalid ICC profile: not \"acsp\" - {}{}{}{} ",
            char::from(p[0]),
            char::from(p[1]),
            char::from(p[2]),
            char::from(p[3]),
        );
        return -1;
    }
    p = &p[4..];
    icch.platform = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.flags.copy_from_slice(&p[..4]);
    p = &p[4..];
    icch.devMnfct = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.devModel = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.devAttr.copy_from_slice(&p[..8]);
    p = &p[8..];
    icch.intent = u32::from_be_byte_slice(&p[..4]) as i32;
    p = &p[4..];
    icch.illuminant.X = u32::from_be_byte_slice(&p[..4]) as i32;
    p = &p[4..];
    icch.illuminant.Y = u32::from_be_byte_slice(&p[..4]) as i32;
    p = &p[4..];
    icch.illuminant.Z = u32::from_be_byte_slice(&p[..4]) as i32;
    p = &p[4..];
    icch.creator = str2iccSig(&p[..4]);
    p = &p[4..];
    icch.ID.copy_from_slice(&p[..16]);
    p = &p[16..];
    /* 28 bytes reserved - must be set to zeros */
    while !p.is_empty() {
        if p[0] != 0 {
            warn!(
                "Reserved pad not zero: {:02x} (at offset {} in ICC profile header.)",
                p[0],
                128 - p.len(),
            );
            return -1;
        }
        p = &p[1..];
    }
    0
}
unsafe fn iccp_get_checksum(profile: &[u8]) -> [u8; 16] {
    let mut md5 = Md5::new();
    md5.input(&profile[..56]);
    md5.input(&[0u8; 12]);
    md5.input(&profile[68..84]);
    md5.input(&[0u8; 16]);
    md5.input(&profile[100..128]);
    /* body */
    md5.input(&profile[128..]);
    md5.result().into()
}

unsafe fn print_iccp_header(icch: &mut iccHeader, checksum: *mut u8) {
    info!("\n");
    info!("pdf_color>> ICC Profile Info\n");
    info!("pdf_color>> Profile Size:\t{} bytes\n", icch.size);
    if icch.CMMType == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "CMM Type");
    } else if icch
        .CMMType
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "CMM Type");
    } else {
        let chars = icch.CMMType.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "CMM Type",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    info!(
        "pdf_color>> Profile Version:\t{}.{:01}.{:01}\n",
        icch.version >> 24 & 0xff,
        icch.version >> 20 & 0xf,
        icch.version >> 16 & 0xf,
    );
    if icch.devClass == 0 {
        info!("pdf_color>> {}:\t(null)\n", "Device Class");
    } else if icch
        .devClass
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Device Class");
    } else {
        let chars = icch.devClass.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Device Class",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    if icch.colorSpace == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "Color Space");
    } else if icch
        .colorSpace
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Color Space");
    } else {
        let chars = icch.colorSpace.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Color Space",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    if icch.PCS == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "Connection Space");
    } else if icch
        .PCS
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Connection Space");
    } else {
        let chars = icch.PCS.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Connection Space",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    info!("pdf_color>> Creation Date:\t");
    for i in (0..12).step_by(2) {
        if i == 0 {
            info!("{:04}", u16::from_be_byte_slice(&icch.creationDate[..2]));
        } else {
            info!(
                ":{:02}",
                u16::from_be_byte_slice(&icch.creationDate[i..i + 2]),
            );
        }
    }
    info!("\n");
    if icch.platform == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "Primary Platform");
    } else if icch
        .platform
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Primary Platform");
    } else {
        let chars = icch.platform.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Primary Platform",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    info!(
        "pdf_color>> Profile Flags:\t{:02x}:{:02x}:{:02x}:{:02x}\n",
        icch.flags[0] as i32, icch.flags[1] as i32, icch.flags[2] as i32, icch.flags[3] as i32,
    );
    if icch.devMnfct == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "Device Mnfct");
    } else if icch
        .devMnfct
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Device Mnfct");
    } else {
        let chars = icch.devMnfct.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Device Mnfct",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    if icch.devModel == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "Device Model");
    } else if icch
        .devModel
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Device Model");
    } else {
        let chars = icch.devModel.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Device Model",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    info!("pdf_color>> Device Attr:\t");
    for i in 0..8 {
        if i == 0 {
            info!("{:02x}", icch.devAttr[i]);
        } else {
            info!(":{:02x}", icch.devAttr[i]);
        }
    }
    info!("\n");
    info!("pdf_color>> Rendering Intent:\t");
    match icch.intent >> 16 & 0xff {
        2 => {
            info!("Saturation");
        }
        0 => {
            info!("Perceptual");
        }
        3 => {
            info!("Absolute Colorimetric");
        }
        1 => {
            info!("Relative Colorimetric");
        }
        _ => {
            info!("(invalid)");
        }
    }
    info!("\n");
    if icch.creator == 0_u32 {
        info!("pdf_color>> {}:\t(null)\n", "Creator");
    } else if icch
        .creator
        .to_be_bytes()
        .iter()
        .any(|&x| libc::isprint(x as i32) == 0)
    {
        info!("pdf_color>> {}:\t(invalid)\n", "Creator");
    } else {
        let chars = icch.creator.to_be_bytes();
        info!(
            "pdf_color>> {}:\t{}{}{}{}\n",
            "Creator",
            char::from(chars[0]),
            char::from(chars[1]),
            char::from(chars[2]),
            char::from(chars[3]),
        );
    }
    info!("pdf_color>> Illuminant (XYZ):\t");
    info!(
        "{:.3} {:.3} {:.3}\n",
        icch.illuminant.X as f64 / 0x10000 as f64,
        icch.illuminant.Y as f64 / 0x10000 as f64,
        icch.illuminant.Z as f64 / 0x10000 as f64,
    );
    info!("pdf_color>> Checksum:\t");
    if icch.ID == [0; 16] {
        info!("(null)");
    } else {
        for i in 0..16 {
            if i == 0 {
                info!("{:02x}", icch.ID[i]);
            } else {
                info!(":{:02x}", icch.ID[i]);
            }
        }
    }
    info!("\n");
    if !checksum.is_null() {
        info!("pdf_color>> Calculated:\t");
        for i in 0..16 {
            if i == 0 {
                info!("{:02x}", *checksum.offset(i as isize));
            } else {
                info!(":{:02x}", *checksum.offset(i as isize));
            }
        }
        info!("\n");
    };
}
unsafe fn iccp_devClass_allowed(dev_class: i32) -> i32 {
    let _colormode = pdf_dev_get_param(2); // TODO: check
    if dev_class as u32 != str2iccSig(b"scnr")
        && dev_class as u32 != str2iccSig(b"mntr")
        && dev_class as u32 != str2iccSig(b"prtr")
        && dev_class as u32 != str2iccSig(b"spac")
    {
        return 0;
    }
    1
}

pub(crate) unsafe fn iccp_load_profile(ident: &str, profile: &[u8]) -> i32 {
    let mut cspc_id;
    let mut icch = iccHeader::default();
    let colorspace;
    iccp_init_iccHeader(&mut icch);
    if iccp_unpack_header(&mut icch, profile, 1) < 0 {
        /* check size */
        warn!("Invalid ICC profile header in \"{}\"", ident);
        print_iccp_header(&mut icch, ptr::null_mut());
        return -1;
    }
    if iccp_version_supported(icch.version >> 24 & 0xff, icch.version >> 16 & 0xff) == 0 {
        warn!("ICC profile format spec. version {}.{:01}.{:01} not supported in current PDF version setting.",
                    icch.version >> 24 & 0xff,
                    icch.version >> 20 & 0xf,
                    icch.version >> 16 & 0xf);
        warn!("ICC profile not embedded.");
        print_iccp_header(&mut icch, ptr::null_mut());
        return -1;
    }
    if iccp_devClass_allowed(icch.devClass as i32) == 0 {
        warn!("Unsupported ICC Profile Device Class:");
        print_iccp_header(&mut icch, ptr::null_mut());
        return -1;
    }
    if icch.colorSpace == str2iccSig(b"RGB ") {
        colorspace = -3
    } else if icch.colorSpace == str2iccSig(b"GRAY") {
        colorspace = -1
    } else if icch.colorSpace == str2iccSig(b"CMYK") {
        colorspace = -4
    } else {
        warn!("Unsupported input color space.");
        print_iccp_header(&mut icch, ptr::null_mut());
        return -1;
    }
    let mut checksum = iccp_get_checksum(profile);
    if icch.ID != [0; 16] && icch.ID != checksum {
        warn!("Invalid ICC profile: Inconsistent checksum.");
        print_iccp_header(&mut icch, checksum.as_mut_ptr());
        return -1;
    }
    let mut cdata = Box::new(iccbased_cdata::new());
    cdata.colorspace = colorspace;
    cdata.checksum = checksum;
    cspc_id = pdf_colorspace_findresource(ident, 4, &cdata);
    if cspc_id >= 0 {
        if VERBOSE != 0 {
            info!("(ICCP:[id={}])", cspc_id);
        }
        assert!(cdata.sig == i32::from_be_bytes(*b"iccb"));
        return cspc_id;
    }
    if VERBOSE > 1 {
        print_iccp_header(&mut icch, checksum.as_mut_ptr());
    }
    let mut resource = vec![];
    let mut stream = pdf_stream::new(STREAM_COMPRESS);
    resource.push_obj("ICCBased");
    stream
        .get_dict_mut()
        .set("N", get_num_components_iccbased(&cdata) as f64);
    stream.add_slice(profile);
    resource.push_obj(stream.into_ref());
    cspc_id = pdf_colorspace_defineresource(ident, 4, cdata, resource.into_obj());
    cspc_id
}
static mut CSPC_CACHE: Vec<pdf_colorspace> = Vec::new();

unsafe fn pdf_colorspace_findresource(ident: &str, type_0: i32, cdata: &iccbased_cdata) -> i32 {
    let mut cmp = false;
    let mut cspc_id = 0;
    while !cmp && cspc_id < CSPC_CACHE.len() {
        let colorspace = &mut CSPC_CACHE[cspc_id];
        if !(colorspace.subtype != type_0) {
            match colorspace.subtype {
                4 => {
                    cmp = compare_iccbased(
                        ident,
                        Some(cdata),
                        &colorspace.ident,
                        colorspace.cdata.as_deref(),
                    )
                }
                _ => {}
            }
            if cmp {
                return cspc_id as i32;
            }
        }
        cspc_id += 1
    }
    return -1;
    /* not found */
}

impl pdf_colorspace {
    fn new() -> Self {
        Self {
            ident: String::new(),
            subtype: 0,
            resource: ptr::null_mut(),
            reference: ptr::null_mut(),
            cdata: None,
        }
    }
}
unsafe fn pdf_clean_colorspace_struct(colorspace: &mut pdf_colorspace) {
    pdf_release_obj(colorspace.resource);
    pdf_release_obj(colorspace.reference);
    colorspace.resource = ptr::null_mut();
    colorspace.reference = ptr::null_mut();
    if let Some(cdata) = colorspace.cdata.as_ref() {
        match colorspace.subtype {
            4 => {
                assert!(cdata.sig == i32::from_be_bytes(*b"iccb"));
            }
            _ => {}
        }
    }
    colorspace.cdata = None;
    colorspace.subtype = 0;
}
unsafe fn pdf_flush_colorspace(colorspace: &mut pdf_colorspace) {
    pdf_release_obj(colorspace.resource);
    pdf_release_obj(colorspace.reference);
    colorspace.resource = ptr::null_mut();
    colorspace.reference = ptr::null_mut();
}
/* **************************** COLOR SPACE *****************************/
unsafe fn pdf_colorspace_defineresource(
    ident: &str,
    subtype: i32,
    cdata: Box<iccbased_cdata>,
    resource: *mut pdf_obj,
) -> i32 {
    let cspc_id = CSPC_CACHE.len() as i32;
    let mut colorspace = pdf_colorspace::new();
    if !ident.is_empty() {
        colorspace.ident = ident.to_string();
    }
    colorspace.subtype = subtype;
    colorspace.cdata = Some(cdata);
    colorspace.resource = resource;
    if VERBOSE != 0 {
        info!("(ColorSpace:{}", ident);
        if VERBOSE > 1 {
            match subtype {
                4 => {
                    info!("[ICCBased]");
                }
                3 => {
                    info!("[CalRGB]");
                }
                1 => {
                    info!("[CalGray]");
                }
                _ => {}
            }
        }
        info!(")");
    }
    CSPC_CACHE.push(colorspace);
    cspc_id
}

pub(crate) unsafe fn pdf_get_colorspace_reference(cspc_id: i32) -> *mut pdf_obj {
    let colorspace = &mut CSPC_CACHE[cspc_id as usize];
    if colorspace.reference.is_null() {
        colorspace.reference = pdf_ref_obj(colorspace.resource);
        pdf_release_obj(colorspace.resource);
        colorspace.resource = ptr::null_mut()
    }
    pdf_link_obj(colorspace.reference)
}

pub(crate) unsafe fn pdf_init_colors() {
    CSPC_CACHE = Vec::new();
}
/* Not check size */
/* returns colorspace ID */

impl Drop for pdf_colorspace {
    fn drop(&mut self) {
        unsafe {
            pdf_flush_colorspace(self);
            pdf_clean_colorspace_struct(self);
        }
    }
}

pub(crate) unsafe fn pdf_close_colors() {
    CSPC_CACHE = Vec::new();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn is_valid() {
        assert!(WHITE.is_valid());
        assert!(pdf_color::rgb(0.5, 0.5, 0.5).unwrap().is_valid());
        assert!(pdf_color::cmyk(0.3, 0.4, 0.5, 0.6).unwrap().is_valid());
    }
}
