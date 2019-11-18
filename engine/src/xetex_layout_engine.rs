use harfbuzz_sys::{hb_ot_math_glyph_part_t, hb_tag_t, hb_feature_t};

/// PlatformFontRef matches C++
#[cfg(not(target_os = "macos"))]
pub type PlatformFontRef = *mut FcPattern;
#[cfg(target_os = "macos")]
use crate::xetex_aatfont::cf_prelude::CTFontDescriptorRef;
#[cfg(target_os = "macos")]
pub type PlatformFontRef = CTFontDescriptorRef;

pub use crate::xetex_font_info::GlyphBBox;

pub type Fixed = i32;

#[derive(Copy, Clone)]
#[cfg_attr(not(target_os = "macos"), repr(C))]
#[cfg_attr(target_os = "macos", repr(C, packed(2)))]
pub struct FixedPoint {
    pub x: Fixed,
    pub y: Fixed,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct FloatPoint {
    pub x: f32,
    pub y: f32,
}

#[cfg(not(target_os = "macos"))]
extern "C" {
    pub type _FcPattern;
}
#[cfg(not(target_os = "macos"))]
pub type FcPattern = _FcPattern;

pub type scaled_t = i32;
