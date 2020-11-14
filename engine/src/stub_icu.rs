pub(crate) type UChar = u16;
pub(crate) type UErrorCode = i32;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct UBreakIterator {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct UConverter {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct UBiDi {
    _unused: [u8; 0],
}

pub(crate) type UConverterType = i32;
pub(crate) type UBiDiLevel = u8;

pub(crate) const UCNV_UTF32_LittleEndian: UConverterType = 8;
pub(crate) const U_ZERO_ERROR: UErrorCode = 0;

pub(crate) type UBiDiDirection = u32;
pub(crate) const UBIDI_RTL: UBiDiDirection = 1;
pub(crate) const UBIDI_MIXED: UBiDiDirection = 2;

pub(crate) type UBreakIteratorType = u32;
pub(crate) const UBRK_LINE: UBreakIteratorType = 2;

extern_and_forward_stub! {
    pub(crate) fn ubidi_open => tt_ubidi_open() -> *mut UBiDi;
    pub(crate) fn ubidi_close => tt_ubidi_close(pBiDi: *mut UBiDi) -> ();
    pub(crate) fn ubidi_setPara => tt_ubidi_setPara(
        pBiDi: *mut UBiDi,
        text: *const UChar,
        length: i32,
        paraLevel: UBiDiLevel,
        embeddingLevels: *mut UBiDiLevel,
        pErrorCode: *mut UErrorCode
    ) -> ();
    pub(crate) fn ubidi_getDirection => tt_ubidi_getDirection(pBiDi: *const UBiDi) -> UBiDiDirection;
    pub(crate) fn ubidi_getVisualRun => tt_ubidi_getVisualRun(
        pBiDi: *mut UBiDi,
        runIndex: i32,
        pLogicalStart: *mut i32,
        pLength: *mut i32
    ) -> UBiDiDirection;
    pub(crate) fn ubidi_countRuns => tt_ubidi_countRuns(pBiDi: *mut UBiDi, pErrorCode: *mut UErrorCode) -> i32;
    pub(crate) fn ubrk_next => tt_ubrk_next(bi: *mut UBreakIterator) -> i32;
    pub(crate) fn ubrk_close => tt_ubrk_close(bi: *mut UBreakIterator) -> ();
    pub(crate) fn ubrk_open => tt_ubrk_open(
        type_0: UBreakIteratorType,
        locale: *const i8,
        text: *const UChar,
        textLength: i32,
        status: *mut UErrorCode
    ) -> *mut UBreakIterator;
    pub(crate) fn ubrk_setText => tt_ubrk_setText(
        bi: *mut UBreakIterator,
        text: *const UChar,
        textLength: i32,
        status: *mut UErrorCode
    ) -> ();
    pub(crate) fn ucnv_open => tt_ucnv_open(converterName: *const i8, err: *mut UErrorCode) -> *mut UConverter;
    pub(crate) fn ucnv_close => tt_ucnv_close(converter: *mut UConverter) -> ();
    pub(crate) fn ucnv_toAlgorithmic => tt_ucnv_toAlgorithmic(
        algorithmicType: UConverterType,
        cnv: *mut UConverter,
        target: *mut i8,
        targetCapacity: i32,
        source: *const i8,
        sourceLength: i32,
        pErrorCode: *mut UErrorCode
    ) -> i32;
    pub(crate) fn ucnv_fromUChars => tt_ucnv_fromUChars (
        cnv: *mut UConverter,
        dest: *mut i8,
        destCapacity: i32,
        src: *const UChar,
        srcLength: i32,
        pErrorCode: *mut UErrorCode
    ) -> i32;
    pub(crate) fn ucnv_toUChars => tt_ucnv_toUChars (
        cnv: *mut UConverter,
        dest: *mut UChar,
        destCapacity: i32,
        src: *const i8,
        srcLength: i32,
        pErrorCode: *mut UErrorCode
    ) -> i32;
}
