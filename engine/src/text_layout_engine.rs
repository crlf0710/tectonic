#![allow(dead_code)]

// XXX: should be no harfbuzz in the interface
use harfbuzz_sys::hb_tag_t;
use crate::xetex_ext::UniChar;
use crate::xetex_layout_engine::{Fixed, FixedPoint};
use crate::xetex_font_info::{XeTeXFontInst, GlyphBBox};
use crate::xetex_ini::memory_word;
use crate::xetex_font_manager::PlatformFontRef;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum TextDirection {
    LTR,
    RTL,
}

// Annoying XeTeXFontMgr singleton accessors
// pub unsafe fn getFullName(fontRef: PlatformFontRef) -> *const libc::c_char;
// pub unsafe fn getDesignSize(font: *mut XeTeXFontInst) -> f64;
// pub unsafe fn findFontByName(name: &CStr, var: Option<&mut String>, size: f64) -> PlatformFontRef;
// pub unsafe fn terminate_font_manager();
// pub unsafe fn destroy_font_manager();

// Internal to XeTeXLayoutEngine but could use improvement

// pub unsafe fn getGlyphs(engine: XeTeXLayoutEngine, glyphs: *mut u32);
// pub unsafe fn getGlyphAdvances(engine: XeTeXLayoutEngine, advances: *mut f32);
// pub unsafe fn getGlyphPositions(engine: XeTeXLayoutEngine, positions: *mut FloatPoint);

// engine : *font_layout_engine.offset((*node.offset(4)).b16.s2 as isize) as CFDictionaryRef;
pub struct LayoutRequest<'a> {
    // ```text
    // let txtLen = (*node.offset(4)).b16.s1 as libc::c_long;
    // let txtPtr = node.offset(6) as *mut UniChar;
    // slice::from_raw_parts(txtPtr, txtLen)
    // ```
    pub text: &'a [u16],
    // node.offset(1).b32.s1
    pub line_width: Fixed,
    // let f = let mut f: libc::c_uint = (*node.offset(4)).b16.s2 as libc::c_uint;
    // *font_letter_space.offset(f as usize)
    pub letter_space_unit: Fixed,
}

impl LayoutRequest<'_> {
    /// Unsafety: obviously, dereferences raw node pointer. The lifetime is also pulled out of
    /// thin air, so just keep it in scope, ok?
    pub unsafe fn from_node<'a>(node: *mut memory_word) -> LayoutRequest<'a> {

        use crate::xetex_ini::font_letter_space;

        let txtLen = (*node.offset(4)).b16.s1 as usize;
        let txtPtr = node.offset(6) as *mut UniChar;
        let text = std::slice::from_raw_parts(txtPtr, txtLen);
        let line_width = (*node.offset(1)).b32.s1;
        let mut f = (*node.offset(4)).b16.s2 as u32;
        let letter_space_unit = *font_letter_space.offset(f as isize);
        LayoutRequest {
            text,
            line_width,
            letter_space_unit,
        }
    }
}

pub struct NodeLayout {
    // if lsDelta != 0i32 {
    //     lsDelta -= lsUnit;
    //     let ref mut fresh2 = (*node.offset(1)).b32.s1;
    //     *fresh2 += lsDelta
    // }
    pub lsDelta: Option<Fixed>,
    // (*node.offset(1)).b32.s1 = width as int32_t;
    pub width: Fixed,
    // (*node.offset(4)).b16.s0 = totalGlyphCount as uint16_t;
    pub total_glyph_count: u16,
    // let ref mut fresh0 = (*node.offset(5)).ptr;
    // *fresh0 = glyph_info;
    pub glyph_info: *mut FixedPoint,
}

impl NodeLayout {
    pub unsafe fn write_node(&self, node: *mut memory_word) {
            let NodeLayout {
                lsDelta,
                width,
                total_glyph_count,
                glyph_info,
            } = *self;

            (*node.offset(1)).b32.s1 = width + lsDelta.unwrap_or(0);
            (*node.offset(4)).b16.s0 = total_glyph_count;
            let ref mut fresh0 = (*node.offset(5)).ptr;
            *fresh0 = glyph_info as *mut libc::c_void;
    }
}

/// Stuff that should be added as XeTeXFontInst methods
trait FontInstance {
    unsafe fn countGlyphs(font: *mut XeTeXFontInst) -> u32;
    unsafe fn getGlyphWidth(font: *mut XeTeXFontInst, gid: u32) -> f32;
    unsafe fn setFontLayoutDir(font: *mut XeTeXFontInst, vertical: libc::c_int);

    /// getGlyphName
    /// Only used for debugging. Should be a String/CStr then!
    unsafe fn glyph_name(
        font: *mut XeTeXFontInst,
        gid: u16,
        len: *mut libc::c_int,
    ) -> *const libc::c_char;

    unsafe fn getIndLanguage(
        font: *mut XeTeXFontInst,
        script: hb_tag_t,
        index: u32,
    ) -> hb_tag_t;
    unsafe fn countFeatures(
        font: *mut XeTeXFontInst,
        script: hb_tag_t,
        language: hb_tag_t,
    ) -> u32;
    unsafe fn getIndFeature(
        font: *mut XeTeXFontInst,
        script: hb_tag_t,
        language: hb_tag_t,
        index: u32,
    ) -> hb_tag_t;
    unsafe fn countScripts(font: *mut XeTeXFontInst) -> u32;
    unsafe fn getIndScript(font: *mut XeTeXFontInst, index: u32) -> hb_tag_t;
    unsafe fn countLanguages(font: *mut XeTeXFontInst, script: hb_tag_t) -> u32;
    unsafe fn getSlant(font: *mut XeTeXFontInst) -> Fixed;
    unsafe fn getFontTablePtr(font: *mut XeTeXFontInst, tableTag: u32) -> *mut libc::c_void;
    // unsafe fn deleteFont(mut font: *mut XeTeXFontInst);
}

// Not quite layout engine things

// pub unsafe fn createFont(fontRef: PlatformFontRef, pointSize: Fixed) -> *mut XeTeXFontInst;
// pub unsafe fn createFontFromFile(
//     filename: &CStr,
//     index: libc::c_int,
//     pointSize: Fixed,
// ) -> *mut XeTeXFontInst;

// // Misc static dictionary lookups/setters
// pub unsafe fn set_cp_code(fontNum: libc::c_int, code: libc::c_uint, side: libc::c_int, value: libc::c_int);
// pub unsafe fn get_cp_code(
//     fontNum: libc::c_int,
//     code: libc::c_uint,
//     side: libc::c_int,
// ) -> libc::c_int;

pub struct GlyphBBoxCache {
    // ...
}
impl GlyphBBoxCache {
    /// getCachedGlyphBBox
    pub unsafe fn get(fontID: u16, glyphID: u16) -> Option<GlyphBBox> {
        unimplemented!()
    }
    pub unsafe fn store(fontID: u16, glyphID: u16, bbox: GlyphBBox) {
        unimplemented!()
    }
}

#[repr(u8)]
pub enum GlyphEdge {
    Left = 1,
    Top = 2,
    Right = 3,
    Bottom = 4,
}

impl GlyphEdge {
    /// If a glyph is left or right
    #[inline]
    pub fn is_side(&self) -> bool {
        match *self {
            GlyphEdge::Left | GlyphEdge::Right => true,
            _ => false,
        }
    }
    #[inline]
    pub fn pick_from(&self, options: &(f32, f32)) -> f32 {
        match *self {
            GlyphEdge::Left | GlyphEdge::Top => options.0,
            GlyphEdge::Right | GlyphEdge::Bottom => options.1,
        }
    }
    pub fn from_int(i: i32) -> Option<Self> {
        Some(match i {
            1 => GlyphEdge::Left,
            2 => GlyphEdge::Top,
            3 => GlyphEdge::Right,
            4 => GlyphEdge::Bottom,
            _ => return None,
        })
    }
}

pub trait TextLayoutEngine {

    /// The most important trait method. Lay out some text and return its size.
    unsafe fn layout_text(&mut self, request: LayoutRequest) -> NodeLayout;

    /// getFontFilename
    /// Only for make_font_def. Should use CStr, probably.
    fn font_filename(
        &self,
        index: &mut u32,
    ) -> *mut libc::c_char;

    /// getFontRef
    fn platform_font_ref(&self) -> PlatformFontRef;

    /// getFontInst
    fn font_instance(&self) -> *mut XeTeXFontInst;

    // should implement Drop
    // unsafe fn deleteLayoutEngine(mut engine: XeTeXLayoutEngine);

    /// getExtendFactor
    fn extend_factor(&self) -> f32;
    /// getPointSize
    fn point_size(&self) -> f32;
    /// getAscentAndDescent
    fn ascent_and_descent(&self, ascent: &mut f32, descent: &mut f32);
    /// getCapAndXHeight
    fn cap_and_x_height(&self, capheight: &mut f32, xheight: &mut f32);
    /// getEmboldenFactor
    fn embolden_factor(&self) -> f32;
    /// getDefaultDirection
    // TODO: TextDirection
    fn default_direction(&self) -> i32;
    /// getRgbValue
    fn rgb_value(&self) -> u32;

    /// getGlyphBounds (had out param)
    unsafe fn glyph_bbox(&self, glyphID: u32) -> Option<GlyphBBox>;

    unsafe fn getGlyphWidthFromEngine(&self, glyphID: u32) -> f32;

    /// getGlyphHeightDepth (had out params height, depth)
    unsafe fn glyph_height_depth(&self, glyphID: u32) -> Option<(f32, f32)>;

    /// getGlyphSidebearings (had out params lsb, rsb)
    unsafe fn glyph_sidebearings(&self, glyphID: u32) -> Option<(f32, f32)>;

    /// getGlyphItalCorr
    unsafe fn glyph_ital_correction(&self, glyphID: u32) -> Option<f32>;

    /// mapCharToGlyph
    /// Should probably just use engine.font as this just passes on the call
    /// This is used for 'fallback in case lacks an OS/2 table', and also for adding accents
    /// (get_native_char_sidebearings).
    /// Although the shaping engine should probably be doing the latter, not xetex0!
    unsafe fn map_codepoint_to_glyph(&self, codepoint: u32) -> u32;

    /// getFontCharRange
    /// Another candidate for using XeTeXFontInst directly
    unsafe fn font_char_range(&self, reqFirst: libc::c_int) -> libc::c_int;

    /// mapGlyphToIndex
    /// Should use engine.font directly
    unsafe fn map_glyph_to_index(&self, glyphName: *const libc::c_char) -> i32;


    // Provided methods, override if using stuff

    /// Default impl is { false }.
    /// Only used directly with xetex0.
    unsafe fn usingGraphite(&self) -> bool {
        false
    }

    /// Returns true if "user asked for Graphite line breaking and the font supports it"
    /// Only relevant if this engine actually uses graphite, hence default impl of { false }
    unsafe fn initGraphiteBreaking(
        &mut self,
        txtPtr: *const u16,
        txtLen: i32,
    ) -> bool {
        false
    }

    /// Not sure what AAT should return, since this is only called with random casts to
    /// XeTeXLayoutENgine in xetex0.
    unsafe fn usingOpenType(&self) -> bool {
        false
    }

    unsafe fn isOpenTypeMathFont(&self) -> bool {
        false
    }

}

trait GraphiteFontSomething {
    unsafe fn countGraphiteFeatures(&self) -> u32;
    unsafe fn getGraphiteFeatureCode(
        &self,
        index: u32,
    ) -> u32;
    unsafe fn countGraphiteFeatureSettings(
        &self,
        featureID: u32,
    ) -> u32;
    unsafe fn getGraphiteFeatureSettingCode(
        &self,
        featureID: u32,
        index: u32,
    ) -> u32;
    unsafe fn getGraphiteFeatureDefaultSetting(
        &self,
        featureID: u32,
    ) -> u32;
    unsafe fn getGraphiteFeatureLabel(
        &self,
        featureID: u32,
    ) -> *mut libc::c_char;
    unsafe fn getGraphiteFeatureSettingLabel(
        &self,
        featureID: u32,
        settingID: u32,
    ) -> *mut libc::c_char;
    unsafe fn findGraphiteFeature(
        &self,
        s: *const libc::c_char,
        e: *const libc::c_char,
        f: *mut hb_tag_t,
        v: *mut libc::c_int,
    ) -> bool;
    unsafe fn findGraphiteFeatureNamed(
        &self,
        name: *const libc::c_char,
        namelength: libc::c_int,
    ) -> libc::c_long;
    unsafe fn findGraphiteFeatureSettingNamed(
        &self,
        id: u32,
        name: *const libc::c_char,
        namelength: libc::c_int,
    ) -> libc::c_long;
}

