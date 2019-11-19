/* DVI op codes */
pub const SET_CHAR_0: u8 = 0;
pub const SET_CHAR_1: u8 = 1;
/* etc. */
pub const SET_CHAR_127: u8 = 127;
pub const SET1: u8 = 128; /* Typesets its single operand between 128 and 255 */
pub const SET2: u8 = 129; /* Typesets its single two byte unsigned operand */
pub const SET3: u8 = 130; /* Typesets its single three byte unsigned operand */
pub const SET4: u8 = 131; /* Typesets its single four byte signed operand */
pub const SET_RULE: u8 = 132; /* Sets a rule of height param1(four bytes) and width param2(four bytes) */
/* These are *signed*.  Nothing typeset for nonpositive values */
/* However, negative value *do* change current point */
pub const PUT1: u8 = 133; /* Like SET1, but point doesn't change */
pub const PUT2: u8 = 134; /* Like SET2 */
pub const PUT3: u8 = 135; /* Like SET3 */
pub const PUT4: u8 = 136; /* Like SET4 */
pub const PUT_RULE: u8 = 137; /* Like SET_RULE */
pub const NOP: u8 = 138;
pub const BOP: u8 = 139; /* Followed by 10 four byte count registers (signed?).  Last parameter points to */
/* previous BOP (backward linked, first BOP has -1).  BOP clears stack and resets current point. */
pub const EOP: u8 = 140;
pub const PUSH: u8 = 141; /* Pushes h,v,w,x,y,z */
pub const POP: u8 = 142; /* Opposite of push*/
pub const RIGHT1: u8 = 143; /* Move right by one byte signed operand */
pub const RIGHT2: u8 = 144; /* Move right by two byte signed operand */
pub const RIGHT3: u8 = 145; /* Move right by three byte signed operand */
pub const RIGHT4: u8 = 146; /* Move right by four byte signed operand */
pub const W0: u8 = 147; /* Move right w */
pub const W1: u8 = 148; /* w <- single byte signed operand.  Move right by same amount */
pub const W2: u8 = 149; /* Same as W1 with two byte signed operand */
pub const W3: u8 = 150; /* Three byte signed operand */
pub const W4: u8 = 151; /* Four byte signed operand */
pub const X0: u8 = 152; /* Move right x */
pub const X1: u8 = 153; /* Like W1 */
pub const X2: u8 = 154; /* Like W2 */
pub const X3: u8 = 155; /* Like W3 */
pub const X4: u8 = 156; /* Like W4 */
pub const DOWN1: u8 = 157; /* Move down by one byte signed operand */
pub const DOWN2: u8 = 158; /* Two byte signed operand */
pub const DOWN3: u8 = 159; /* Three byte signed operand */
pub const DOWN4: u8 = 160; /* Four byte signed operand */
pub const Y0: u8 = 161; /* Move down by y */
pub const Y1: u8 = 162; /* Move down by one byte signed operand, which replaces Y */
pub const Y2: u8 = 163; /* Two byte signed operand */
pub const Y3: u8 = 164; /* Three byte signed operand */
pub const Y4: u8 = 165; /* Four byte signed operand */
pub const Z0: u8 = 166; /* Like Y0, but use z */
pub const Z1: u8 = 167; /* Like Y1 */
pub const Z2: u8 = 168; /* Like Y2 */
pub const Z3: u8 = 169; /* Like Y3 */
pub const Z4: u8 = 170; /* Like Y4 */
pub const FNT_NUM_0: u8 = 171; /* Switch to font 0 */
pub const FNT_NUM_1: u8 = 172; /* Switch to font 1 */
/* etc. */
pub const FNT_NUM_63: u8 = 234; /* Switch to font 63 */
pub const FNT1: u8 = 235; /* Switch to font described by single byte unsigned operand */
pub const FNT2: u8 = 236; /* Switch to font described by two byte unsigned operand */
pub const FNT3: u8 = 237; /* Three byte font descriptor */
pub const FNT4: u8 = 238; /* Four byte operator (Knuth says signed, but what would be the point? */
pub const XXX1: u8 = 239; /* Special.  Operand is one byte length.  Special follows immediately */
pub const XXX2: u8 = 240; /* Two byte operand */
pub const XXX3: u8 = 241; /* Three byte operand */
pub const XXX4: u8 = 242; /* Four byte operand (Knuth says TeX uses only XXX1 and XXX4 */
pub const FNT_DEF1: u8 = 243; /* One byte font number, four byte checksum, four byte magnified size (DVI units),
                              four byte designed size, single byte directory length, single byte name length,
                              followed by complete name (area+name) */
pub const FNT_DEF2: u8 = 244; /* Same for two byte font number */
pub const FNT_DEF3: u8 = 245; /* Same for three byte font number */
pub const FNT_DEF4: u8 = 246; /* Four byte font number (Knuth says signed) */
pub const PRE: u8 = 247; /* Preamble:
                         one byte DVI version (should be 2)
                         four byte unsigned numerator
                         four byte unsigned denominator -- one DVI unit = den/num*10^(-7) m
                         four byte magnification (multiplied by 1000)
                         one byte unsigned comment length followed by comment. */
pub const DVI_ID: u8 = 2; /* ID Byte for current DVI file */
pub const DVIV_ID: u8 = 3; /* with Ascii pTeX VW mode extension */
pub const XDV_ID_OLD: u8 = 6; /* older XeTeX ".xdv" output that does not have XDV_TEXT_AND_GLYPHS */
pub const XDV_ID: u8 = 7; /* XeTeX ".xdv" output that uses XDV opcodes below */
pub const POST: u8 = 248; /* Postamble- -- similar to preamble
                          four byte pointer to final bop
                          four byte numerator
                          four byte denominator
                          four byte mag
                          four byte maximum height (signed?)
                          four byte maximum width
                          two byte max stack depth required to process file
                          two byte number of pages */
pub const POST_POST: u8 = 249; /* End of postamble
                               four byte pointer to POST command
                               Version byte (same as preamble)
                               Padded by four or more 223's to the end of the file. */
pub const PADDING: u8 = 223;

pub const BEGIN_REFLECT: u8 = 250; /* TeX-XeT begin_reflect */
pub const END_REFLECT: u8 = 251; /* TeX-XeT end_reflect */

/* XeTeX ".xdv" codes */
pub const XDV_NATIVE_FONT_DEF: u8 = 252; /* fontdef for native platform font */
pub const XDV_GLYPHS: u8 = 253; /* string of glyph IDs with X and Y positions */
pub const XDV_TEXT_AND_GLYPHS: u8 = 254; /* like XDV_GLYPHS plus original Unicode text */

pub const PTEXDIR: u8 = 255; /* Ascii pTeX DIR command */
