//! The command codes.
//!
//! Before we can go any further, we need to define symbolic names for the internal
//! code numbers that represent the various commands obeyed by \TeX. These codes
//! are somewhat arbitrary, but not completely so. For example, the command
//! codes for character types are fixed by the language, since a user says,
//! e.g., `\catcode \`${} = 3` to make `\char'44` a math delimiter,
//! and the command code `math_shift` is equal to 3. Some other codes have
//! been made adjacent so that `case` statements in the program need not consider
//! cases that are widely spaced, or so that `case` statements can be replaced
//! by `if` statements.
//!
//! At any rate, here is the list, for future reference. First come the
//! "catcode" commands, several of which share their numeric codes with
//! ordinary commands when the catcode cannot emerge from \TeX's scanning routine.

/// Commands
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, enumn::N)]
pub(crate) enum Cmd {
    /// do nothing (`\relax`)
    Relax = 0,
    /// beginning of a group (`{`)
    LeftBrace = 1,
    /// ending of a group (`}`)
    RightBrace = 2,
    /// mathematics shift character (`$`)
    MathShift = 3,
    /// alignment delimiter (`&`, `\span`)
    TabMark = 4,
    /// end of line (`carriage_return`, `\cr`, `\crcr`)
    CarRet = 5,
    /// macro parameter symbol (`#`)
    MacParam = 6,
    /// superscript (`\char'136`)
    SupMark = 7,
    /// subscript (`\char'137`)
    SubMark = 8,
    /// end of `<v_j>` list in alignment template
    EndV = 9,
    /// characters equivalent to blank space (` `)
    Spacer = 10,
    /// characters regarded as letters (`A..Z`, `a..z`)
    Letter = 11,
    /// none of the special character types
    OtherChar = 12,
    /// characters that invoke macros (`\char'\~`)
    ActiveChar = 13,
    /// characters that introduce comments (`%`)
    Comment = 14,
    /// specify delimiter numerically (`\delimiter`)
    DelimNum = 15,

    // Next are the ordinary run-of-the-mill command codes.  Codes that are
    // `min_internal` or more represent internal quantities that might be
    // expanded by `\the`
    /// character specified numerically (`\char`)
    CharNum = 16,
    /// explicit math code (`\mathchar`)
    MathCharNum = 17,
    /// mark definition (`\mark`)
    Mark = 18,
    /// peek inside of \TeX (`\show`, `\showbox`, etc.)
    XRay = 19,
    /// make a box (`\box`, `\copy`, `\hbox`, etc.)
    MakeBox = 20,
    /// horizontal motion (`\moveleft`, `\moveright`)
    HMove = 21,
    /// vertical motion (`\raise`, `\lower`)
    VMove = 22,
    /// unglue a box (`\unhbox`, `\unhcopy`)
    UnHBox = 23,
    /// unglue a box (`\unvbox`, `\unvcopy`)
    /// ( or `\pagediscards`, `\splitdiscards`)
    UnVBox = 24,
    /// nullify last item (`\unpenalty`, `\unkern`, `\unskip`)
    RemoveItem = 25,
    /// horizontal glue (`\hskip`, `\hfil`, etc.)
    HSkip = 26,
    /// vertical glue (`\vskip`, `\vfil`, etc.)
    VSkip = 27,
    /// math glue (`\mskip`)
    MSkip = 28,
    /// fixed space (`\kern`)
    Kern = 29,
    /// math kern (`\mkern`)
    MKern = 30,
    /// use a box (`\shipout`, `\leaders`, etc.)
    LeaderShip = 31,
    /// horizontal table alignment (`\halign`)
    HAlign = 32,
    /// vertical table alignment (`\valign`)
    /// or text direction directives (`\beginL`, etc.)
    VAlign = 33,
    /// temporary escape from alignment (`\noalign`)
    NoAlign = 34,
    /// vertical rule (`\vrule`)
    VRule = 35,
    /// horizontal rule (`\hrule`)
    HRule = 36,
    /// vlist inserted in box (`\insert`)
    Insert = 37,
    /// vlist inserted in enclosing paragraph (`\vadjust`)
    VAdjust = 38,
    /// gobble `spacer` tokens (`\ignorespaces`)
    IgnoreSpaces = 39,
    /// save till assignment is done (`\afterassignment`)
    AfterAssignment = 40,
    /// save till group is done (`\aftergroup`)
    AfterGroup = 41,
    /// additional badness (`\penalty`)
    BreakPenalty = 42,
    /// begin paragraph (`\indent`, `\noindent`)
    StartPar = 43,
    /// italic correction (`\/`)
    ItalCorr = 44,
    /// attach accent in text (`\accent`)
    Accent = 45,
    /// attach accent in math (`\mathaccent`)
    MathAccent = 46,
    /// discretionary texts (`\-`, `\discretionary`)
    Discretionary = 47,
    /// equation number (`\eqno`, `\leqno`)
    EqNo = 48,
    /// variable delimiter (`\left`, `\right`)
    /// (or `\middle`)
    LeftRight = 49,
    /// component of formula (`\mathbin`, etc.)
    MathComp = 50,
    /// diddle limit conventions (`\displaylimits`, etc.)
    LimitSwitch = 51,
    /// generalized fraction (`\above`, `\atop`, etc.)
    Above = 52,
    /// style specification (`\displaystyle`, etc.)
    MathStyle = 53,
    /// choice specification (`\mathchoice`)
    MathChoice = 54,
    /// conditional math glue (`\nonscript`)
    NonScript = 55,
    /// vertically center a vbox (`\vcenter`)
    VCenter = 56,
    /// force specific case (`\lowercase`, `\uppercase`)
    CaseShift = 57,
    /// send to user (`\message`, `\errmessage`)
    Message = 58,
    /// extensions to \TeX (`\write`, `\special`, etc.)
    Extension = 59,
    /// files for reading (`\openin`, `\closein`)
    InStream = 60,
    /// begin local grouping (`\begingroup`)
    BeginGroup = 61,
    /// end local grouping (`\endgroup`)
    EndGroup = 62,
    /// omit alignment template (`\omit`)
    Omit = 63,
    /// explicit space (`\ `)
    ExSpace = 64,
    /// suppress boundary ligatures (`\noboundary`)
    NoBoundary = 65,
    /// square root and similar signs (`\radical`)
    Radical = 66,
    /// end control sequence (`\endcsname`)
    EndCSName = 67,
    /// character code defined by `\chardef`
    CharGiven = 68,
    /// math code defined by `\mathchardef`
    MathGiven = 69,
    /// extended math code defined by `\Umathchardef`
    XetexMathGiven = 70,
    /// most recent item (`\lastpenalty`, `\lastkern`, `\lastskip`)
    LastItem = 71,

    // The next codes are special; they all relate to mode-independent
    // assignment of values to \TeX's internal registers or tables.
    // Codes that are `max_internal` or less represent internal quantities
    // that might be expanded by `\the`.
    /// token list register (`\toks`)
    ToksRegister = 72,
    /// special token list (`\output}`, `\everypar`, etc.)
    AssignToks = 73,
    /// user-defined integer (`\tolerance`, `\day`, etc.)
    AssignInt = 74,
    /// user-defined length (`\hsize`, etc.)
    AssignDimen = 75,
    /// user-defined glue (`\baselineskip`, etc.)
    AssignGlue = 76,
    /// user-defined muglue (`\thinmuskip`, etc.)
    AssignMuGlue = 77,
    /// user-defined font dimension (`\fontdimen`)
    AssignFontDimen = 78,
    /// user-defined font integer (`\hyphenchar`, `\skewchar`)
    AssignFontInt = 79,
    /// specify state info (`\spacefactor`, `\prevdepth`)
    SetAux = 80,
    /// specify state info (`\prevgraf`)
    SetPrevGraf = 81,
    /// specify state info (`\pagegoal`, etc.)
    SetPageDimen = 82,
    /// specify state info (`\deadcycles`, `\insertpenalties`)
    /// (or `\interactionmode`)
    SetPageInt = 83,
    /// change dimension of box (`\wd`, `\ht`, `\dp`)
    SetBoxDimen = 84,
    /// specify fancy paragraph shape (`\parshape`)
    /// (or `\interlinepenalties`, etc.)
    SetShape = 85,
    /// define a character code (`\catcode`, etc.)
    DefCode = 86,
    /// `\Umathcode`, `\Udelcode`
    XetexDefCode = 87,
    /// declare math fonts (`\textfont`, etc.)
    DefFamily = 88,
    /// set current font (font identifiers)
    SetFont = 89,
    /// define a font file (`\font`)
    DefFont = 90,
    /// internal register (`\count`, `\dimen`, etc.)
    Register = 91,
    /// advance a register or parameter (`\advance`)
    Advance = 92,
    /// multiply a register or parameter (`\multiply`)
    Multiply = 93,
    /// divide a register or parameter (`\divide`)
    Divide = 94,
    /// qualify a definition (`\global`, `\long`, `\outer`)
    /// (or `\protected`)
    Prefix = 95,
    /// assign a command code (`\let`, `\futurelet`)
    Let = 96,
    /// code definition (`\chardef`, `\countdef`, etc.)
    ShorthandDef = 97,
    /// read into a control sequence (`\read`)
    /// ( or `\readline`)
    ReadToCS = 98,
    /// macro definition (`\def`, `\gdef`, `\xdef`, `\edef`)
    Def = 99,
    /// set a box (`\setbox`)
    SetBox = 100,
    /// hyphenation data (`\hyphenation`, `\patterns`)
    HyphData = 101,
    /// define level of interaction (`\batchmode`, etc.)
    SetInteraction = 102,

    // The remaining command codes are extra special, since they cannot get through
    // \TeX's scanner to the main control routine. They have been given values higher
    // than `max_command` so that their special nature is easily discernible.
    // The "expandable" commands come first.
    /// initial state of most `EQ_TYPE` fields
    UndefinedCS = 103,
    /// special expansion (`\expandafter`)
    ExpandAfter = 104,
    /// special nonexpansion (`\noexpand`)
    NoExpand = 105,
    /// input a source file (`\input`, `\endinput`)
    /// (or `\scantokens`)
    Input = 106,
    /// conditional text (`\if`, `\ifcase`, etc.)
    IfTest = 107,
    /// delimiters for conditionals (`\else`, etc.)
    FiOrElse = 108,
    /// make a control sequence from tokens (`\csname`)
    CSName = 109,
    /// convert to text (`\number`, `\string`, etc.)
    Convert = 110,
    /// expand an internal quantity (`\the`)
    /// (or \.{\\unexpanded}, `\detokenize`)
    The = 111,
    /// inserted mark (`\topmark`, etc.)
    TopBotMark = 112,
    /// non-long, non-outer control sequence
    Call = 113,
    /// long, non-outer control sequence
    LongCall = 114,
    /// non-long, outer control sequence
    OuterCall = 115,
    /// long, outer control sequence
    LongOuterCall = 116,
    /// end of an alignment template
    EndTemplate = 117,
    /// the following token was marked by `\noexpand`
    DontExpand = 118,
    /// the equivalent points to a glue specification
    GlueRef = 119,
    /// the equivalent points to a parshape specification
    ShapeRef = 120,
    /// the equivalent points to a box node, or is `null`
    BoxRef = 121,
    /// the equivalent is simply a halfword number
    Data = 122,
}

impl From<u16> for Cmd {
    fn from(n: u16) -> Self {
        Self::n(n as u8).unwrap_or_else(|| panic!("incorrect command = {}", n))
    }
}

/// escape delimiter (called "\\" in *The \TeX book*)
pub(crate) const ESCAPE: Cmd = Cmd::Relax;
/// output a macro parameter
pub(crate) const OUT_PARAM: Cmd = Cmd::CarRet;
/// characters to ignore (`^^@@`)
pub(crate) const IGNORE: Cmd = Cmd::EndV;
/// end of paragraph (`\par`)
pub(crate) const PAR_END: Cmd = Cmd::ActiveChar;
/// match a macro parameter
pub(crate) const MATCH: Cmd = Cmd::ActiveChar;
/// end of parameters to macro
pub(crate) const END_MATCH: Cmd = Cmd::Comment;
/// end of job (`\end`, `\dump`)
pub(crate) const STOP: Cmd = Cmd::Comment;
/// characters that shouldn't appear (`^^?`)
pub(crate) const INVALID_CHAR: Cmd = Cmd::DelimNum;

/// largest catcode for individual characters
pub(crate) const MAX_CHAR_CODE: i32 = 15;
/// largest command code that can't be `\global`
pub(crate) const MAX_NON_PREFIXED_COMMAND: Cmd = Cmd::LastItem;
/// the smallest code that can follow `\the`
pub(crate) const MIN_INTERNAL: Cmd = Cmd::CharGiven;
/// the largest code that can follow `\the`
pub(crate) const MAX_INTERNAL: Cmd = Cmd::Register;
/// the largest command code seen at `big_switch|
pub(crate) const MAX_COMMAND: Cmd = Cmd::SetInteraction;

#[repr(i32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum AssignFontInt {
    HyphenChar = 0,
    SkewChar = 1,
    LpCode = 2,
    RpCode = 3,
}
impl From<i32> for AssignFontInt {
    fn from(n: i32) -> Self {
        Self::n(n).unwrap_or_else(|| panic!("incorrect assign font int = {}", n))
    }
}
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

    PdfLastXPos = 12,
    PdfLastYPos = 13,
    ElapsedTime = 16,
    PdfShellEscape = 17,
    RandomSeed = 18,

    EtexVersion = 19,
    CurrentGroupLevel = 20,
    CurrentGroupType = 21,
    CurrentIfLevel = 22,
    CurrentIfType = 23,
    CurrentIfBranch = 24,
    GlueStretchOrder = 25,
    GlueShrinkOrder = 26,

    XetexVersion = 27,
    XetexCountGlyphs = 28,
    XetexCountVariations = 29,
    XetexVariation = 30,
    XetexFindVariationByName = 31,
    XetexVariationMin = 32,
    XetexVariationMax = 33,
    XetexVariationDefault = 34,
    XetexCountFeatures = 35,
    XetexFeatureCode = 36,
    XetexFindFeatureByName = 37,
    XetexIsExclusiveFeature = 38,
    XetexCountSelectors = 39,
    XetexSelectorCode = 40,
    XetexFindSelectorByName = 41,
    XetexIsDefaultSelector = 42,
    XetexOTCountScripts = 43,
    XetexOTCountLanguages = 44,
    XetexOTCountFeatures = 45,
    XetexOTScript = 46,
    XetexOTLanguage = 47,
    XetexOTFeature = 48,
    XetexMapCharToGlyph = 49,
    XetexGlyphIndex = 50,
    XetexFontType = 51,
    XetexFirstChar = 52,
    XetexLastChar = 53,
    XetexPdfPageCount = 54,
    XetexGlyphBounds = 55,
    FontCharWd = 56,
    FontCharHt = 57,
    FontCharDp = 58,
    FontCharIc = 59,
    ParShapeLength = 60,
    ParShapeIndent = 61,
    ParShapeDimen = 62,
    GlueStretch = 63,
    GlueShrink = 64,
    MuToGlue = 65, // ETEX_GLUE
    GlueToMu = 66, // ETEX_MU
    EtexExprInt = 67,
    EtexExprDimen = 68,
    EtexExprGlue = 69,
    EtexExprMu = 70,
}

pub(crate) const XETEX_DIM: LastItemCode = LastItemCode::XetexGlyphBounds;

/// command codes for XeTeX extension commands
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum XetexExtCmd {
    CountGlyphs = 1,
    CountFeatures = 8,
    FeatureCode = 9,
    FindFeatureByName = 10,
    IsExclusiveFeature = 11,
    CountSelectors = 12,
    SelectorCode = 13,
    FindSelectorByName = 14,
    IsDefaultSelector = 15,
    OTCountScripts = 16,
    OTCountLanguages = 17,
    OTCountFeatures = 18,
    OTScript = 19,
    OTLanguage = 20,
    OTFeature = 21,
    MapCharToGlyph = 22,
}

impl From<LastItemCode> for XetexExtCmd {
    fn from(code: LastItemCode) -> Self {
        use LastItemCode::*;
        match code {
            XetexCountGlyphs => Self::CountGlyphs,
            XetexCountFeatures => Self::CountFeatures,
            XetexFeatureCode => Self::FeatureCode,
            XetexFindFeatureByName => Self::FindFeatureByName,
            XetexIsExclusiveFeature => Self::IsExclusiveFeature,
            XetexCountSelectors => Self::CountSelectors,
            XetexSelectorCode => Self::SelectorCode,
            XetexFindSelectorByName => Self::FindSelectorByName,
            XetexIsDefaultSelector => Self::IsDefaultSelector,
            XetexOTCountScripts => Self::OTCountScripts,
            XetexOTCountLanguages => Self::OTCountLanguages,
            XetexOTCountFeatures => Self::OTCountFeatures,
            XetexOTScript => Self::OTScript,
            XetexOTLanguage => Self::OTLanguage,
            XetexOTFeature => Self::OTFeature,
            XetexMapCharToGlyph => Self::MapCharToGlyph,
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
    EtexRevision = 5, /* = ETEX_CONVERT_BASE */
    Expanded = 6,     /* = ETEX_CONVERT_CODES */
    XetexFeatureNameOld = 8,
    XetexSelectorNameOld = 9,
    LeftMarginKern = 16,
    RightMarginKern = 17,
    PdfStrcmp = 18,
    PdfCreationDate = 22,
    PdfFileModDate = 23,
    PdfFileSize = 24,
    PdfMdfiveSum = 25,
    PdfFileDump = 26,
    UniformDeviate = 29,
    NormalDeviate = 30,
    XetexVariationName = 32,
    XetexRevision = 33,
    XetexFeatureName = 35,
    XetexSelectorName = 36,
    XetexGlyphName = 37,
    XetexUchar = 38,
    XetexUcharcat = 39,
    JobName = 40,
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
pub(crate) const PDF_SAVE_POS_NODE: u16 = PDFTEX_FIRST_EXTENSION_CODE + 15;
pub(crate) const RESET_TIMER_CODE: u16 = PDFTEX_FIRST_EXTENSION_CODE + 25;
pub(crate) const SET_RANDOM_SEED_CODE: u16 = PDFTEX_FIRST_EXTENSION_CODE + 27;
pub(crate) const PIC_FILE_CODE: u16 = 41;
pub(crate) const PDF_FILE_CODE: u16 = 42;
pub(crate) const GLYPH_CODE: u16 = 43;
pub(crate) const XETEX_INPUT_ENCODING_EXTENSION_CODE: u16 = 44;
pub(crate) const XETEX_DEFAULT_ENCODING_EXTENSION_CODE: u16 = 45;
pub(crate) const XETEX_LINEBREAK_LOCALE_EXTENSION_CODE: u16 = 46;

/// Cmd::SetInteraction
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, enumn::N)]
pub(crate) enum InteractionMode {
    Batch = 0,
    NonStop = 1,
    Scroll = 2,
    ErrorStop = 3,
}
//pub(crate) const UNSPECIFIED_MODE: u8 = 4;

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
    ShorthandDefCode,
    InteractionMode
);
