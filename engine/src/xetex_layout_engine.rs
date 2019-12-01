use harfbuzz_sys::{hb_feature_t, hb_ot_math_glyph_part_t, hb_tag_t};

pub(crate) type XeTeXLayoutEngine = *mut XeTeXLayoutEngine_rec;
/// PlatformFontRef matches C++
#[cfg(not(target_os = "macos"))]
pub(crate) type PlatformFontRef = *mut FcPattern;
#[cfg(target_os = "macos")]
use crate::xetex_aatfont::cf_prelude::CTFontDescriptorRef;
#[cfg(target_os = "macos")]
pub(crate) type PlatformFontRef = CTFontDescriptorRef;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct GlyphAssembly {
    pub(crate) count: u32,
    pub(crate) parts: *mut hb_ot_math_glyph_part_t,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct GlyphBBox {
    pub(crate) xMin: f32,
    pub(crate) yMin: f32,
    pub(crate) xMax: f32,
    pub(crate) yMax: f32,
}
extern "C" {
    pub(crate) type XeTeXFont_rec;
    pub(crate) type XeTeXLayoutEngine_rec;
    #[no_mangle]
    pub(crate) fn get_ot_math_constant(f: i32, n: i32) -> i32;
    #[no_mangle]
    pub(crate) fn set_cp_code(fontNum: i32, code: u32, side: i32, value: i32);
    #[no_mangle]
    pub(crate) fn get_cp_code(fontNum: i32, code: u32, side: i32) -> i32;
    #[no_mangle]
    pub(crate) fn isOpenTypeMathFont(engine: XeTeXLayoutEngine) -> bool;
    #[no_mangle]
    pub(crate) fn usingGraphite(engine: XeTeXLayoutEngine) -> bool;
    #[no_mangle]
    pub(crate) fn usingOpenType(engine: XeTeXLayoutEngine) -> bool;
    #[no_mangle]
    pub(crate) fn terminate_font_manager();
    #[no_mangle]
    pub(crate) fn destroy_font_manager();
    #[no_mangle]
    pub(crate) fn get_native_mathsy_param(f: i32, n: i32) -> i32;
    #[no_mangle]
    pub(crate) fn get_native_mathex_param(f: i32, n: i32) -> i32;
    #[no_mangle]
    pub(crate) fn get_ot_math_variant(f: i32, g: i32, v: i32, adv: *mut i32, horiz: i32) -> i32;
    #[no_mangle]
    pub(crate) fn get_ot_assembly_ptr(f: i32, g: i32, horiz: i32) -> *mut libc::c_void;
    #[no_mangle]
    pub(crate) fn free_ot_assembly(a: *mut GlyphAssembly);
    #[no_mangle]
    pub(crate) fn get_ot_math_ital_corr(f: i32, g: i32) -> i32;
    #[no_mangle]
    pub(crate) fn get_ot_math_accent_pos(f: i32, g: i32) -> i32;
    #[no_mangle]
    pub(crate) fn get_ot_math_kern(f: i32, g: i32, sf: i32, sg: i32, cmd: i32, shift: i32) -> i32;
    #[no_mangle]
    pub(crate) fn ot_part_count(a: *const GlyphAssembly) -> i32;
    #[no_mangle]
    pub(crate) fn ot_part_glyph(a: *const GlyphAssembly, i: i32) -> i32;
    #[no_mangle]
    pub(crate) fn ot_part_is_extender(a: *const GlyphAssembly, i: i32) -> bool;
    #[no_mangle]
    pub(crate) fn ot_part_start_connector(f: i32, a: *const GlyphAssembly, i: i32) -> i32;
    #[no_mangle]
    pub(crate) fn ot_part_end_connector(f: i32, a: *const GlyphAssembly, i: i32) -> i32;
    #[no_mangle]
    pub(crate) fn ot_part_full_advance(f: i32, a: *const GlyphAssembly, i: i32) -> i32;
    #[no_mangle]
    pub(crate) fn ot_min_connector_overlap(f: i32) -> i32;
    #[no_mangle]
    pub(crate) fn getCachedGlyphBBox(fontID: u16, glyphID: u16, bbox: *mut GlyphBBox) -> i32;
    #[no_mangle]
    pub(crate) fn cacheGlyphBBox(fontID: u16, glyphID: u16, bbox: *const GlyphBBox);
    #[no_mangle]
    pub(crate) fn maketexstring(s: *const i8) -> i32;
    #[no_mangle]
    pub(crate) fn getDefaultDirection(engine: XeTeXLayoutEngine) -> i32;
    #[no_mangle]
    pub(crate) fn createFont(fontRef: PlatformFontRef, pointSize: Fixed) -> XeTeXFont;
    #[no_mangle]
    pub(crate) fn getAscentAndDescent(
        engine: XeTeXLayoutEngine,
        ascent: *mut f32,
        descent: *mut f32,
    );
    #[no_mangle]
    pub(crate) fn setFontLayoutDir(font: XeTeXFont, vertical: i32);
    #[no_mangle]
    pub(crate) fn layoutChars(
        engine: XeTeXLayoutEngine,
        chars: *mut u16,
        offset: i32,
        count: i32,
        max: i32,
        rightToLeft: bool,
    ) -> i32;
    #[no_mangle]
    pub(crate) fn getPointSize(engine: XeTeXLayoutEngine) -> f32;
    #[no_mangle]
    pub(crate) fn getGlyphPositions(engine: XeTeXLayoutEngine, positions: *mut FloatPoint);
    #[no_mangle]
    pub(crate) fn getGlyphAdvances(engine: XeTeXLayoutEngine, advances: *mut f32);
    #[no_mangle]
    pub(crate) fn getGlyphs(engine: XeTeXLayoutEngine, glyphs: *mut u32);
    #[no_mangle]
    pub(crate) fn findFontByName(name: *const i8, var: *mut i8, size: f64) -> PlatformFontRef;
    #[no_mangle]
    pub(crate) fn getReqEngine() -> i8;
    #[no_mangle]
    pub(crate) fn setReqEngine(reqEngine: i8);
    #[no_mangle]
    pub(crate) fn getFullName(fontRef: PlatformFontRef) -> *const i8;
    #[no_mangle]
    pub(crate) fn getFontFilename(engine: XeTeXLayoutEngine, index: *mut u32) -> *mut i8;
    #[no_mangle]
    pub(crate) fn getDesignSize(font: XeTeXFont) -> f64;
    #[no_mangle]
    pub(crate) fn deleteFont(font: XeTeXFont);
    #[no_mangle]
    pub(crate) fn getSlant(font: XeTeXFont) -> Fixed;
    #[no_mangle]
    pub(crate) fn getFontTablePtr(font: XeTeXFont, tableTag: u32) -> *mut libc::c_void;
    #[no_mangle]
    pub(crate) fn countScripts(font: XeTeXFont) -> u32;
    #[no_mangle]
    pub(crate) fn countLanguages(font: XeTeXFont, script: hb_tag_t) -> u32;
    #[no_mangle]
    pub(crate) fn countFeatures(font: XeTeXFont, script: hb_tag_t, language: hb_tag_t) -> u32;
    #[no_mangle]
    pub(crate) fn countGlyphs(font: XeTeXFont) -> u32;
    #[no_mangle]
    pub(crate) fn getIndScript(font: XeTeXFont, index: u32) -> hb_tag_t;
    #[no_mangle]
    pub(crate) fn getIndLanguage(font: XeTeXFont, script: hb_tag_t, index: u32) -> hb_tag_t;
    #[no_mangle]
    pub(crate) fn getIndFeature(
        font: XeTeXFont,
        script: hb_tag_t,
        language: hb_tag_t,
        index: u32,
    ) -> hb_tag_t;
    #[no_mangle]
    pub(crate) fn getGlyphWidth(font: XeTeXFont, gid: u32) -> f32;
    #[no_mangle]
    pub(crate) fn createFontFromFile(
        filename: *const i8,
        index: i32,
        pointSize: Fixed,
    ) -> XeTeXFont;
    #[no_mangle]
    pub(crate) fn getCapAndXHeight(
        engine: XeTeXLayoutEngine,
        capheight: *mut f32,
        xheight: *mut f32,
    );
    #[no_mangle]
    pub(crate) fn getEmboldenFactor(engine: XeTeXLayoutEngine) -> f32;
    #[no_mangle]
    pub(crate) fn getSlantFactor(engine: XeTeXLayoutEngine) -> f32;
    #[no_mangle]
    pub(crate) fn getExtendFactor(engine: XeTeXLayoutEngine) -> f32;
    #[no_mangle]
    pub(crate) fn getFontRef(engine: XeTeXLayoutEngine) -> PlatformFontRef;
    #[no_mangle]
    pub(crate) fn getFont(engine: XeTeXLayoutEngine) -> XeTeXFont;
    #[no_mangle]
    pub(crate) fn deleteLayoutEngine(engine: XeTeXLayoutEngine);
    #[no_mangle]
    pub(crate) fn createLayoutEngine(
        fontRef: PlatformFontRef,
        font: XeTeXFont,
        script: hb_tag_t,
        language: *mut i8,
        features: *mut hb_feature_t,
        nFeatures: i32,
        shapers: *mut *mut i8,
        rgbValue: u32,
        extend: f32,
        slant: f32,
        embolden: f32,
    ) -> XeTeXLayoutEngine;
    /* graphite interface functions... */
    #[no_mangle]
    pub(crate) fn findGraphiteFeature(
        engine: XeTeXLayoutEngine,
        s: *const i8,
        e: *const i8,
        f: *mut hb_tag_t,
        v: *mut i32,
    ) -> bool;
    #[no_mangle]
    pub(crate) fn findNextGraphiteBreak() -> i32;
    #[no_mangle]
    pub(crate) fn initGraphiteBreaking(
        engine: XeTeXLayoutEngine,
        txtPtr: *const u16,
        txtLen: i32,
    ) -> bool;
    #[no_mangle]
    pub(crate) fn getFontCharRange(engine: XeTeXLayoutEngine, reqFirst: i32) -> i32;
    #[no_mangle]
    pub(crate) fn getGlyphName(font: XeTeXFont, gid: u16, len: *mut i32) -> *const i8;
    #[no_mangle]
    pub(crate) fn mapGlyphToIndex(engine: XeTeXLayoutEngine, glyphName: *const i8) -> i32;
    #[no_mangle]
    pub(crate) fn mapCharToGlyph(engine: XeTeXLayoutEngine, charCode: u32) -> u32;
    #[no_mangle]
    pub(crate) fn getGlyphItalCorr(engine: XeTeXLayoutEngine, glyphID: u32) -> f32;
    #[no_mangle]
    pub(crate) fn getGlyphSidebearings(
        engine: XeTeXLayoutEngine,
        glyphID: u32,
        lsb: *mut f32,
        rsb: *mut f32,
    );
    #[no_mangle]
    pub(crate) fn getGlyphHeightDepth(
        engine: XeTeXLayoutEngine,
        glyphID: u32,
        height: *mut f32,
        depth: *mut f32,
    );
    #[no_mangle]
    pub(crate) fn getGlyphWidthFromEngine(engine: XeTeXLayoutEngine, glyphID: u32) -> f32;
    #[no_mangle]
    pub(crate) fn getGlyphBounds(engine: XeTeXLayoutEngine, glyphID: u32, bbox: *mut GlyphBBox);
    #[no_mangle]
    pub(crate) fn getRgbValue(engine: XeTeXLayoutEngine) -> u32;
    #[no_mangle]
    pub(crate) fn countGraphiteFeatures(engine: XeTeXLayoutEngine) -> u32;
    #[no_mangle]
    pub(crate) fn getGraphiteFeatureCode(engine: XeTeXLayoutEngine, index: u32) -> u32;
    #[no_mangle]
    pub(crate) fn countGraphiteFeatureSettings(engine: XeTeXLayoutEngine, feature: u32) -> u32;
    #[no_mangle]
    pub(crate) fn getGraphiteFeatureSettingCode(
        engine: XeTeXLayoutEngine,
        feature: u32,
        index: u32,
    ) -> u32;
    #[no_mangle]
    pub(crate) fn getGraphiteFeatureDefaultSetting(engine: XeTeXLayoutEngine, feature: u32) -> u32;
    #[no_mangle]
    pub(crate) fn getGraphiteFeatureLabel(engine: XeTeXLayoutEngine, feature: u32) -> *mut i8;
    #[no_mangle]
    pub(crate) fn getGraphiteFeatureSettingLabel(
        engine: XeTeXLayoutEngine,
        feature: u32,
        setting: u32,
    ) -> *mut i8;
    #[no_mangle]
    pub(crate) fn findGraphiteFeatureNamed(
        engine: XeTeXLayoutEngine,
        name: *const i8,
        namelength: i32,
    ) -> i64;
    #[no_mangle]
    pub(crate) fn findGraphiteFeatureSettingNamed(
        engine: XeTeXLayoutEngine,
        feature: u32,
        name: *const i8,
        namelength: i32,
    ) -> i64;
    /* not the MS compiler, so try Metrowerks' platform macros */
    /* this seems to be needed for a gcc-mingw32 build to work... */
    /*
        Create a converter object from a compiled mapping
    */
    #[no_mangle]
    pub(crate) fn gr_label_destroy(label: *mut libc::c_void);
}
pub(crate) type XeTeXFont = *mut XeTeXFont_rec;
#[cfg(not(target_os = "macos"))]
pub(crate) type Fixed = scaled_t;
#[cfg(target_os = "macos")]
pub(crate) type Fixed = SInt32;

#[derive(Copy, Clone)]
#[cfg_attr(not(target_os = "macos"), repr(C))]
#[cfg_attr(target_os = "macos", repr(C, packed(2)))]
pub(crate) struct FixedPoint {
    pub(crate) x: Fixed,
    pub(crate) y: Fixed,
}
pub(crate) type scaled_t = i32;
pub(crate) type SInt32 = i32;
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct FloatPoint {
    pub(crate) x: f32,
    pub(crate) y: f32,
}

#[cfg(not(target_os = "macos"))]
extern "C" {
    pub(crate) type _FcPattern;
}
#[cfg(not(target_os = "macos"))]
pub(crate) type FcPattern = _FcPattern;
