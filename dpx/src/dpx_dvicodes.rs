/* DVI op codes */
pub(crate) const SET_CHAR_0: u8 = 0;
pub(crate) const SET_CHAR_1: u8 = 1;
/* etc. */
pub(crate) const SET_CHAR_127: u8 = 127;
pub(crate) const SET1: u8 = 128; /* Typesets its single operand between 128 and 255 */
pub(crate) const SET2: u8 = 129; /* Typesets its single two byte unsigned operand */
pub(crate) const SET3: u8 = 130; /* Typesets its single three byte unsigned operand */
pub(crate) const SET4: u8 = 131; /* Typesets its single four byte signed operand */
pub(crate) const SET_RULE: u8 = 132; /* Sets a rule of height param1(four bytes) and width param2(four bytes) */
/* These are *signed*.  Nothing typeset for nonpositive values */
/* However, negative value *do* change current point */
pub(crate) const PUT1: u8 = 133; /* Like SET1, but point doesn't change */
pub(crate) const PUT2: u8 = 134; /* Like SET2 */
pub(crate) const PUT3: u8 = 135; /* Like SET3 */
pub(crate) const PUT4: u8 = 136; /* Like SET4 */
pub(crate) const PUT_RULE: u8 = 137; /* Like SET_RULE */
pub(crate) const NOP: u8 = 138;
pub(crate) const BOP: u8 = 139; /* Followed by 10 four byte count registers (signed?).  Last parameter points to */
/* previous BOP (backward linked, first BOP has -1).  BOP clears stack and resets current point. */
pub(crate) const EOP: u8 = 140;
pub(crate) const PUSH: u8 = 141; /* Pushes h,v,w,x,y,z */
pub(crate) const POP: u8 = 142; /* Opposite of push*/
pub(crate) const RIGHT1: u8 = 143; /* Move right by one byte signed operand */
pub(crate) const RIGHT2: u8 = 144; /* Move right by two byte signed operand */
pub(crate) const RIGHT3: u8 = 145; /* Move right by three byte signed operand */
pub(crate) const RIGHT4: u8 = 146; /* Move right by four byte signed operand */
pub(crate) const W0: u8 = 147; /* Move right w */
pub(crate) const W1: u8 = 148; /* w <- single byte signed operand.  Move right by same amount */
pub(crate) const W2: u8 = 149; /* Same as W1 with two byte signed operand */
pub(crate) const W3: u8 = 150; /* Three byte signed operand */
pub(crate) const W4: u8 = 151; /* Four byte signed operand */
pub(crate) const X0: u8 = 152; /* Move right x */
pub(crate) const X1: u8 = 153; /* Like W1 */
pub(crate) const X2: u8 = 154; /* Like W2 */
pub(crate) const X3: u8 = 155; /* Like W3 */
pub(crate) const X4: u8 = 156; /* Like W4 */
pub(crate) const DOWN1: u8 = 157; /* Move down by one byte signed operand */
pub(crate) const DOWN2: u8 = 158; /* Two byte signed operand */
pub(crate) const DOWN3: u8 = 159; /* Three byte signed operand */
pub(crate) const DOWN4: u8 = 160; /* Four byte signed operand */
pub(crate) const Y0: u8 = 161; /* Move down by y */
pub(crate) const Y1: u8 = 162; /* Move down by one byte signed operand, which replaces Y */
pub(crate) const Y2: u8 = 163; /* Two byte signed operand */
pub(crate) const Y3: u8 = 164; /* Three byte signed operand */
pub(crate) const Y4: u8 = 165; /* Four byte signed operand */
pub(crate) const Z0: u8 = 166; /* Like Y0, but use z */
pub(crate) const Z1: u8 = 167; /* Like Y1 */
pub(crate) const Z2: u8 = 168; /* Like Y2 */
pub(crate) const Z3: u8 = 169; /* Like Y3 */
pub(crate) const Z4: u8 = 170; /* Like Y4 */
pub(crate) const FNT_NUM_0: u8 = 171; /* Switch to font 0 */
pub(crate) const FNT_NUM_1: u8 = 172; /* Switch to font 1 */
/* etc. */
pub(crate) const FNT_NUM_63: u8 = 234; /* Switch to font 63 */
pub(crate) const FNT1: u8 = 235; /* Switch to font described by single byte unsigned operand */
pub(crate) const FNT2: u8 = 236; /* Switch to font described by two byte unsigned operand */
pub(crate) const FNT3: u8 = 237; /* Three byte font descriptor */
pub(crate) const FNT4: u8 = 238; /* Four byte operator (Knuth says signed, but what would be the point? */
pub(crate) const XXX1: u8 = 239; /* Special.  Operand is one byte length.  Special follows immediately */
pub(crate) const XXX2: u8 = 240; /* Two byte operand */
pub(crate) const XXX3: u8 = 241; /* Three byte operand */
pub(crate) const XXX4: u8 = 242; /* Four byte operand (Knuth says TeX uses only XXX1 and XXX4 */
pub(crate) const FNT_DEF1: u8 = 243; /* One byte font number, four byte checksum, four byte magnified size (DVI units),
                                     four byte designed size, single byte directory length, single byte name length,
                                     followed by complete name (area+name) */
pub(crate) const FNT_DEF2: u8 = 244; /* Same for two byte font number */
pub(crate) const FNT_DEF3: u8 = 245; /* Same for three byte font number */
pub(crate) const FNT_DEF4: u8 = 246; /* Four byte font number (Knuth says signed) */
pub(crate) const PRE: u8 = 247; /* Preamble:
                                one byte DVI version (should be 2)
                                four byte unsigned numerator
                                four byte unsigned denominator -- one DVI unit = den/num*10^(-7) m
                                four byte magnification (multiplied by 1000)
                                one byte unsigned comment length followed by comment. */
pub(crate) const DVI_ID: u8 = 2; /* ID Byte for current DVI file */
pub(crate) const DVIV_ID: u8 = 3; /* with Ascii pTeX VW mode extension */
pub(crate) const XDV_ID_OLD: u8 = 6; /* older XeTeX ".xdv" output that does not have XDV_TEXT_AND_GLYPHS */
pub(crate) const XDV_ID: u8 = 7; /* XeTeX ".xdv" output that uses XDV opcodes below */
pub(crate) const POST: u8 = 248; /* Postamble- -- similar to preamble
                                 four byte pointer to final bop
                                 four byte numerator
                                 four byte denominator
                                 four byte mag
                                 four byte maximum height (signed?)
                                 four byte maximum width
                                 two byte max stack depth required to process file
                                 two byte number of pages */
pub(crate) const POST_POST: u8 = 249; /* End of postamble
                                      four byte pointer to POST command
                                      Version byte (same as preamble)
                                      Padded by four or more 223's to the end of the file. */
pub(crate) const PADDING: u8 = 223;

pub(crate) const BEGIN_REFLECT: u8 = 250; /* TeX-XeT begin_reflect */
pub(crate) const END_REFLECT: u8 = 251; /* TeX-XeT end_reflect */

/* XeTeX ".xdv" codes */
pub(crate) const XDV_NATIVE_FONT_DEF: u8 = 252; /* fontdef for native platform font */
pub(crate) const XDV_GLYPHS: u8 = 253; /* string of glyph IDs with X and Y positions */
pub(crate) const XDV_TEXT_AND_GLYPHS: u8 = 254; /* like XDV_GLYPHS plus original Unicode text */

pub(crate) const PTEXDIR: u8 = 255; /* Ascii pTeX DIR command */
