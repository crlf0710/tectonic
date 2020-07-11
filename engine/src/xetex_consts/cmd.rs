/// Commands
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, enumn::N)]
pub(crate) enum Cmd {
    Relax = 0,
    LeftBrace = 1,
    RightBrace = 2,
    MathShift = 3,
    TabMark = 4,
    CarRet = 5,
    MacParam = 6,
    SupMark = 7,
    SubMark = 8,
    EndV = 9,
    Spacer = 10,
    Letter = 11,
    OtherChar = 12,
    ActiveChar = 13,
    Comment = 14,
    DelimNum = 15,
    CharNum = 16,
    MathCharNum = 17,
    Mark = 18,
    XRay = 19,
    MakeBox = 20,
    HMove = 21,
    VMove = 22,
    UnHBox = 23,
    UnVBox = 24,
    RemoveItem = 25,
    HSkip = 26,
    VSkip = 27,
    MSkip = 28,
    Kern = 29,
    MKern = 30,
    LeaderShip = 31,
    HAlign = 32,
    VAlign = 33,
    NoAlign = 34,
    VRule = 35,
    HRule = 36,
    Insert = 37,
    VAdjust = 38,
    IgnoreSpaces = 39,
    AfterAssignment = 40,
    AfterGroup = 41,
    BreakPenalty = 42,
    StartPar = 43,
    ItalCorr = 44,
    Accent = 45,
    MathAccent = 46,
    Discretionary = 47,
    EqNo = 48,
    LeftRight = 49,
    MathComp = 50,
    LimitSwitch = 51,
    Above = 52,
    MathStyle = 53,
    MathChoice = 54,
    NonScript = 55,
    VCenter = 56,
    CaseShift = 57,
    Message = 58,
    Extension = 59,
    InStream = 60,
    BeginGroup = 61,
    EndGroup = 62,
    Omit = 63,
    ExSpace = 64,
    NoBoundary = 65,
    Radical = 66,
    EndCSName = 67,
    CharGiven = 68,
    MathGiven = 69,
    XetexMathGiven = 70,
    LastItem = 71,
    ToksRegister = 72,
    AssignToks = 73,
    AssignInt = 74,
    AssignDimen = 75,
    AssignGlue = 76,
    AssignMuGlue = 77,
    AssignFontDimen = 78,
    AssignFontInt = 79,
    SetAux = 80,
    SetPrevGraf = 81,
    SetPageDimen = 82,
    SetPageInt = 83,
    SetBoxDimen = 84,
    SetShape = 85,
    DefCode = 86,
    XetexDefCode = 87,
    DefFamily = 88,
    SetFont = 89,
    DefFont = 90,
    Register = 91,
    Advance = 92,
    Multiply = 93,
    Divide = 94,
    Prefix = 95,
    Let = 96,
    ShorthandDef = 97,
    ReadToCS = 98,
    Def = 99,
    SetBox = 100,
    HyphData = 101,
    SetInteraction = 102,
    UndefinedCS = 103,
    ExpandAfter = 104,
    NoExpand = 105,
    Input = 106,
    IfTest = 107,
    FiOrElse = 108,
    CSName = 109,
    Convert = 110,
    The = 111,
    TopBotMark = 112,
    Call = 113,
    LongCall = 114,
    OuterCall = 115,
    LongOuterCall = 116,
    EndTemplate = 117,
    DontExpand = 118,
    GlueRef = 119,
    ShapeRef = 120,
    BoxRef = 121,
    Data = 122,
}

impl From<u16> for Cmd {
    fn from(n: u16) -> Self {
        Self::n(n as u8).expect(&format!("incorrect command = {}", n))
    }
}

pub(crate) const ESCAPE: Cmd = Cmd::Relax;
pub(crate) const OUT_PARAM: Cmd = Cmd::CarRet;
pub(crate) const IGNORE: Cmd = Cmd::EndV;

pub(crate) const PAR_END: Cmd = Cmd::ActiveChar;
pub(crate) const MATCH: Cmd = Cmd::ActiveChar;

pub(crate) const END_MATCH: Cmd = Cmd::Comment;
pub(crate) const STOP: Cmd = Cmd::Comment;

pub(crate) const INVALID_CHAR: Cmd = Cmd::DelimNum;

pub(crate) const MAX_NON_PREFIXED_COMMAND: Cmd = Cmd::LastItem;
pub(crate) const MIN_INTERNAL: Cmd = Cmd::CharGiven;
pub(crate) const MAX_INTERNAL: Cmd = Cmd::Register;
pub(crate) const MAX_COMMAND: Cmd = Cmd::SetInteraction;

/// args to Cmd::LastItem -- heavily overloaded by (X)eTeX for extensions
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, enumn::N)]
pub(crate) enum LastItemCode {
    LastPenalty = 0,
    LastKern = 1,
    LastSkip = 2,
    LastNodeType = 3,
    InputLineNo = 4,
    Badness = 5,
    EtexVersion = 6,
    CurrentGroupLevel = 7,
    CurrentGroupType = 8,
    CurrentIfLevel = 9,
    CurrentIfType = 10,
    CurrentIfBranch = 11,
    GlueStretchOrder = 12,
    GlueShrinkOrder = 13,
    XetexVersion = 14,
    XetexCountGlyphs = 15,
    XetexCountVariations = 16,
    XetexVariation = 17,
    XetexFindVariationByName = 18,
    XetexVariationMin = 19,
    XetexVariationMax = 20,
    XetexVariationDefault = 21,
    XetexCountFeatures = 22,
    XetexFeatureCode = 23,
    XetexFindFeatureByName = 24,
    XetexIsExclusiveFeature = 25,
    XetexCountSelectors = 26,
    XetexSelectorCode = 27,
    XetexFindSelectorByName = 28,
    XetexIsDefaultSelector = 29,
    XetexOTCountScripts = 30,
    XetexOTCountLanguages = 31,
    XetexOTCountFeatures = 32,
    XetexOTScript = 33,
    XetexOTLanguage = 34,
    XetexOTFeature = 35,
    XetexMapCharToGlyph = 36,
    XetexGlyphIndex = 37,
    XetexFontType = 38,
    XetexFirstChar = 39,
    XetexLastChar = 40,
    PdfLastXPos = 41,
    PdfLastYPos = 42,
    PdfShellEscape = 45,
    XetexPdfPageCount = 46,
    XetexGlyphBounds = 47,
    FontCharWd = 48,
    FontCharHt = 49,
    FontCharDp = 50,
    FontCharIc = 51,
    ParShapeLength = 52,
    ParShapeIndent = 53,
    ParShapeDimen = 54,
    GlueStretch = 55,
    GlueShrink = 56,
    MuToGlue = 57, // ETEX_GLUE
    GlueToMu = 58, // ETEX_MU
    EtexExprInt = 59,
    EtexExprDimen = 60,
    EtexExprGlue = 61,
    EtexExprMu = 62,
}

pub(crate) const XETEX_DIM: LastItemCode = LastItemCode::XetexGlyphBounds;

/// command codes for XeTeX extension commands
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExtCmd {
    XetexCountGlyphs = 1,
    XetexCountFeatures = 8,
    XetexFeatureCode = 9,
    XetexFindFeatureByName = 10,
    XetexIsExclusiveFeature = 11,
    XetexCountSelectors = 12,
    XetexSelectorCode = 13,
    XetexFindSelectorByName = 14,
    XetexIsDefaultSelector = 15,
    XetexOTCountScripts = 16,
    XetexOTCountLanguages = 17,
    XetexOTCountFeatures = 18,
    XetexOTScript = 19,
    XetexOTLanguage = 20,
    XetexOTFeature = 21,
    XetexMapCharToGlyph = 22,
}

impl From<LastItemCode> for ExtCmd {
    fn from(code: LastItemCode) -> Self {
        use LastItemCode::*;
        match code {
            XetexCountGlyphs => Self::XetexCountGlyphs,
            XetexCountFeatures => Self::XetexCountFeatures,
            XetexFeatureCode => Self::XetexFeatureCode,
            XetexFindFeatureByName => Self::XetexFindFeatureByName,
            XetexIsExclusiveFeature => Self::XetexIsExclusiveFeature,
            XetexCountSelectors => Self::XetexCountSelectors,
            XetexSelectorCode => Self::XetexSelectorCode,
            XetexFindSelectorByName => Self::XetexFindSelectorByName,
            XetexIsDefaultSelector => Self::XetexIsDefaultSelector,
            XetexOTCountScripts => Self::XetexOTCountScripts,
            XetexOTCountLanguages => Self::XetexOTCountLanguages,
            XetexOTCountFeatures => Self::XetexOTCountFeatures,
            XetexOTScript => Self::XetexOTScript,
            XetexOTLanguage => Self::XetexOTLanguage,
            XetexOTFeature => Self::XetexOTFeature,
            XetexMapCharToGlyph => Self::XetexMapCharToGlyph,
            _ => unreachable!(),
        }
    }
}

/// args to Cmd::SetBoxDimen
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum SetBoxDimen {
    WidthOffset = 1,
    DepthOffset = 2,
    HeightOffset = 3,
}

/// args to Cmd::Convert -- also heavily overloaded
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum ConvertCode {
    Number = 0,
    RomanNumeral = 1,
    String = 2,
    Meaning = 3,
    FontName = 4,
    EtexRevision = 5,
    XetexRevision = 6,
    XetexVariationName = 7,
    XetexFeatureName = 8,
    XetexSelectorName = 9,
    XetexGlyphName = 10,
    LeftMarginKern = 11,
    RightMarginKern = 12,
    XetexUchar = 13,
    XetexUcharcat = 14,
    JobName = 15,
    PdfStrcmp = 43,
    PdfMdfiveSum = 44,
}

/// args to Cmd::IfTest
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum IfTestCode {
    IfChar = 0,
    IfCat = 1,
    IfInt = 2,
    IfDim = 3,
    IfOdd = 4,
    IfVMode = 5,
    IfHMode = 6,
    IfMMode = 7,
    IfInner = 8,
    IfVoid = 9,
    IfHBox = 10,
    IfVBox = 11,
    Ifx = 12,
    IfEof = 13,
    IfTrue = 14,
    IfFalse = 15,
    IfCase = 16,
    IfDef = 17,
    IfCS = 18,
    IfFontChar = 19,
    IfInCSName = 20,
    IfPrimitive = 21,
}

pub(crate) const UNLESS_CODE: i32 = 32;

/// args to Cmd::TopBotMark
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum TopBotMarkCode {
    Top = 0,
    First = 1,
    Bot = 2,
    SplitFirst = 3,
    SplitBot = 4,
}

/// Cmd::HSkip, Cmd::VSkip, Cmd::MSkip
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum SkipCode {
    Fil = 0,
    Fill = 1,
    Ss = 2,
    FilNeg = 3,
    Skip = 4,
    MSkip = 5,
}

/// Cmd::MakeBox, Cmd::UnHBox, Cmd::UnVBox
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum BoxCode {
    Box = 0,
    Copy = 1,
    LastBox = 2,
    VSplit = 3,
    VTop = 4,
    VBox = 5,   // VTop + ListMode::VMode
    HBox = 108, // VTop + ListMode::HMode
}

/// args to Cmd::FiOrElse
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum FiOrElseCode {
    Normal = 0,
    If = 1,
    Fi = 2,
    Else = 3,
    Or = 4,
}

/* Cmd::Above */
pub(crate) const ABOVE_CODE: i32 = 0;
pub(crate) const OVER_CODE: i32 = 1;
pub(crate) const ATOP_CODE: i32 = 2;
pub(crate) const DELIMITED_CODE: i32 = 3;

/// Cmd::ShorthandDef
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum ShorthandDefCode {
    Char = 0,
    MathChar = 1,
    Count = 2,
    Dimen = 3,
    Skip = 4,
    MuSkip = 5,
    Toks = 6,
    CharSub = 7,
    XetexMathCharNum = 8,
    XetexMathChar = 9,
}

/* XRAY */
pub(crate) const SHOW_CODE: i32 = 0;
pub(crate) const SHOW_BOX_CODE: i32 = 1;
pub(crate) const SHOW_THE_CODE: i32 = 2;
pub(crate) const SHOW_LISTS: i32 = 3;
pub(crate) const SHOW_GROUPS: i32 = 4;
pub(crate) const SHOW_TOKENS: i32 = 5;
pub(crate) const SHOW_IFS: i32 = 6;

/* Cmd::Extension */
pub(crate) const IMMEDIATE_CODE: u16 = 4;
pub(crate) const SET_LANGUAGE_CODE: u16 = 5;
pub(crate) const PDFTEX_FIRST_EXTENSION_CODE: u16 = 6;
pub(crate) const PIC_FILE_CODE: u16 = 41;
pub(crate) const PDF_FILE_CODE: u16 = 42;
pub(crate) const GLYPH_CODE: u16 = 43;
pub(crate) const XETEX_INPUT_ENCODING_EXTENSION_CODE: u16 = 44;
pub(crate) const XETEX_DEFAULT_ENCODING_EXTENSION_CODE: u16 = 45;
pub(crate) const XETEX_LINEBREAK_LOCALE_EXTENSION_CODE: u16 = 46;

/* Cmd::VAlign overloads */
pub(crate) const BEGIN_L_CODE: i32 = 6;
pub(crate) const END_L_CODE: i32 = 7;
pub(crate) const BEGIN_R_CODE: i32 = 10;
pub(crate) const END_R_CODE: i32 = 11;

/* Cmd::SetInteraction */
pub(crate) const BATCH_MODE: u8 = 0;
pub(crate) const NONSTOP_MODE: u8 = 1;
pub(crate) const SCROLL_MODE: u8 = 2;
pub(crate) const ERROR_STOP_MODE: u8 = 3;
pub(crate) const UNSPECIFIED_MODE: u8 = 4;

macro_rules! try_into {
    ($($T:ty),+) => (
        $(
            impl core::convert::TryInto<i32> for $T {
                type Error = &'static str;
                fn try_into(self) -> Result<i32, Self::Error> {
                    Ok(self as i32)
                }
            }
        )+
    )
}

try_into!(
    LastItemCode,
    SetBoxDimen,
    ConvertCode,
    IfTestCode,
    TopBotMarkCode,
    SkipCode,
    BoxCode,
    FiOrElseCode,
    ShorthandDefCode
);
