use bridge::{ttstub_input_close, ttstub_input_getc, TTInputFormat};

use crate::help;
use std::ffi::CString;

use crate::xetex_ini::b16x4;
use crate::xetex_ini::nine_bits;
use crate::xetex_ini::packed_UTF16_code;
use crate::xetex_ini::str_number;
use crate::xetex_ini::UTF16_code;

use crate::xetex_xetexd::print_c_str;
use crate::xetex_xetexd::TeXInt;

use crate::xetex_ini::cur_ext;
use crate::xetex_ini::file_line_error_style_p;
use crate::xetex_ini::file_name_quote_char;
use crate::xetex_ini::fmem_ptr;
use crate::xetex_ini::font_used;
use crate::xetex_ini::loaded_font_design_size;
use crate::xetex_ini::loaded_font_flags;
use crate::xetex_ini::loaded_font_letter_space;
use crate::xetex_ini::loaded_font_mapping;
use crate::xetex_ini::native_font_type_flag;
use crate::xetex_ini::pool_ptr;
use crate::xetex_ini::quoted_filename;
use crate::xetex_ini::str_ptr;
use crate::xetex_ini::BCHAR_LABEL;
use crate::xetex_ini::DEPTH_BASE;
use crate::xetex_ini::FONT_AREA;
use crate::xetex_ini::FONT_BCHAR;
use crate::xetex_ini::FONT_CHECK;
use crate::xetex_ini::FONT_DSIZE;
use crate::xetex_ini::FONT_EC;
use crate::xetex_ini::FONT_FALSE_BCHAR;
use crate::xetex_ini::FONT_FLAGS;
use crate::xetex_ini::FONT_GLUE;
use crate::xetex_ini::FONT_LAYOUT_ENGINE;
use crate::xetex_ini::FONT_LETTER_SPACE;
use crate::xetex_ini::FONT_MAPPING;
use crate::xetex_ini::FONT_MAX;
use crate::xetex_ini::FONT_MEM_SIZE;
use crate::xetex_ini::FONT_PARAMS;
use crate::xetex_ini::FONT_PTR;
use crate::xetex_ini::FONT_SIZE;
use crate::xetex_ini::HEIGHT_BASE;
use crate::xetex_ini::HYPHEN_CHAR;
use crate::xetex_ini::KERN_BASE;
use crate::xetex_ini::LIG_KERN_BASE;
use crate::xetex_ini::PARAM_BASE;
use crate::xetex_ini::SKEW_CHAR;
use crate::xetex_ini::{
    CHAR_BASE, EXTEN_BASE, FONT_BC, FONT_INFO, FONT_NAME, ITALIC_BASE, WIDTH_BASE,
};

use crate::xetex_ini::init_pool_ptr;
use crate::xetex_ini::name_of_file;
use crate::xetex_ini::pool_size;
use crate::xetex_ini::str_pool;
use crate::xetex_ini::str_start;

use crate::xetex_ext::find_native_font;
use crate::xetex_ext::ot_get_font_metrics;
use crate::xetex_ext::release_font_engine;
use crate::xetex_ext::{check_for_tfm_font_mapping, load_tfm_font_mapping};
use crate::xetex_ext::{AAT_FONT_FLAG, OTGR_FONT_FLAG};

use super::xetex_io::tt_xetex_open_input;
use crate::xetex_consts::IntPar;
use crate::xetex_consts::FONT_BASE;
use crate::xetex_consts::INTPAR;
use crate::xetex_consts::LIST_TAG;
use crate::xetex_consts::NON_ADDRESS;
use crate::xetex_consts::TOO_BIG_CHAR;
use crate::xetex_errors::error;
use crate::xetex_errors::overflow;
use crate::xetex_output::print;
use crate::xetex_output::print_file_line;
use crate::xetex_output::print_file_name;
use crate::xetex_output::sprint_cs;
use crate::xetex_output::{
    print_char, print_chr, print_cstr, print_int, print_nl_cstr, print_scaled,
};
use crate::xetex_stringpool::length;
use crate::xetex_stringpool::make_string;
use crate::xetex_stringpool::str_eq_str;
use crate::xetex_stringpool::EMPTY_STRING;
use crate::xetex_xetex0::new_native_character;
use crate::xetex_xetex0::pack_file_name;
use crate::xetex_xetex0::{begin_diagnostic, end_diagnostic};

use crate::xetex_layout_interface::get_ot_math_constant;
use crate::xetex_layout_interface::isOpenTypeMathFont;
use crate::xetex_layout_interface::XeTeXLayoutEngine;
use crate::xetex_scaledmath::xn_over_d;

use bridge::InputHandleWrapper;
pub(crate) unsafe fn read_font_info(
    mut u: i32,
    mut nom: str_number,
    mut aire: str_number,
    mut s: i32,
) -> usize {
    let mut sw: i32 = 0;
    let mut bch_label: i32 = 0;
    let mut bchar_0: i16 = 0;
    let mut alpha: i32 = 0;
    let mut beta: u8 = 0;

    let mut g = FONT_BASE;

    pack_file_name(nom, aire, cur_ext);

    if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
        begin_diagnostic();
        print_nl_cstr("Requested font \"");
        print_c_str(&name_of_file);
        print('\"' as i32);
        if s < 0 {
            print_cstr(" scaled ");
            print_int(-s);
        } else {
            print_cstr(" at ");
            print_scaled(s);
            print_cstr("pt");
        }
        end_diagnostic(false);
    }

    if quoted_filename {
        g = load_native_font(u, nom, aire, s);
        if g != FONT_BASE {
            return done(None, g);
        }
    }

    let name_too_long = length(nom) > 255 || length(aire) > 255;
    if name_too_long {
        return bad_tfm(None, g, u, nom, aire, s, name_too_long);
    }
    pack_file_name(nom, aire, EMPTY_STRING as str_number);
    check_for_tfm_font_mapping();

    let mut tfm_file_owner = tt_xetex_open_input(TTInputFormat::TFM);
    if tfm_file_owner.is_none() {
        if !quoted_filename {
            g = load_native_font(u, nom, aire, s);
            if g != FONT_BASE {
                return done(None, g);
            }
        }
        return bad_tfm(None, g, u, nom, aire, s, name_too_long);
    }

    let tfm_file = tfm_file_owner.as_mut().unwrap();

    /* We are a bit cavalier about EOF-checking since we can't very
     * conveniently implement feof() in the Rust layer, and it only ever is
     * used in this one place. */

    macro_rules! READFIFTEEN (
        ($x:expr) => {
            $x = ttstub_input_getc(tfm_file);
            if $x > 127 || $x == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            $x *= 256;
            $x += ttstub_input_getc(tfm_file);

        };
    );

    let mut lf;
    let mut lh;
    let mut bc;
    let mut ec;
    READFIFTEEN!(lf);
    READFIFTEEN!(lh);
    READFIFTEEN!(bc);
    READFIFTEEN!(ec);

    if bc > ec + 1 || ec > 255 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if bc > 255 {
        bc = 1;
        ec = 0
    }

    let mut nw;
    let mut nh;
    let mut nd;
    let mut ni;
    let mut nl;
    let mut nk;
    let mut ne;
    let mut np;
    READFIFTEEN!(nw);
    READFIFTEEN!(nh);
    READFIFTEEN!(nd);
    READFIFTEEN!(ni);
    READFIFTEEN!(nl);
    READFIFTEEN!(nk);
    READFIFTEEN!(ne);
    READFIFTEEN!(np);

    if lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    } else if nw == 0 || nh == 0 || nd == 0 || ni == 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }

    lf = lf - 6 - lh;
    if np < 7 {
        lf = lf + 7 - np
    }
    assert!(
        !(FONT_PTR == FONT_MAX || fmem_ptr + lf > FONT_MEM_SIZE as i32),
        "not enough memory to load another font"
    );

    let f = FONT_PTR + 1;
    CHAR_BASE[f] = fmem_ptr - bc;
    WIDTH_BASE[f] = CHAR_BASE[f] + ec + 1;
    HEIGHT_BASE[f] = WIDTH_BASE[f] + nw;
    DEPTH_BASE[f] = HEIGHT_BASE[f] + nh;
    ITALIC_BASE[f] = DEPTH_BASE[f] + nd;
    LIG_KERN_BASE[f] = ITALIC_BASE[f] + ni;
    KERN_BASE[f] = LIG_KERN_BASE[f] + nl - 256 * 128;
    EXTEN_BASE[f] = KERN_BASE[f] + 256 * 128 + nk;
    PARAM_BASE[f] = EXTEN_BASE[f] + ne;
    if lh < 2 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    let a = ttstub_input_getc(tfm_file);
    let b = ttstub_input_getc(tfm_file);
    let c = ttstub_input_getc(tfm_file);
    let d = ttstub_input_getc(tfm_file);
    let qw = b16x4 {
        s0: d as u16,
        s1: c as u16,
        s2: b as u16,
        s3: a as u16,
    };
    if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    FONT_CHECK[f as usize] = qw;

    let mut z;
    READFIFTEEN!(z);
    z = z * 256 + ttstub_input_getc(tfm_file);
    z = z * 16 + ttstub_input_getc(tfm_file) / 16;
    if z < 65536 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    while lh > 2 {
        ttstub_input_getc(tfm_file);
        ttstub_input_getc(tfm_file);
        ttstub_input_getc(tfm_file);
        ttstub_input_getc(tfm_file);
        lh -= 1
    }
    FONT_DSIZE[f] = z;
    if s != -1000 {
        if s >= 0 {
            z = s
        } else {
            z = xn_over_d(z, -s, 1000)
        }
    }
    FONT_SIZE[f] = z;

    for k in fmem_ptr..=(WIDTH_BASE[f] - 1) {
        let a = ttstub_input_getc(tfm_file);
        let b = ttstub_input_getc(tfm_file);
        let c = ttstub_input_getc(tfm_file);
        let mut d = ttstub_input_getc(tfm_file);
        let qw = b16x4 {
            s0: d as u16,
            s1: c as u16,
            s2: b as u16,
            s3: a as u16,
        };
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        FONT_INFO[k as usize].b16 = qw;

        if a >= nw || b / 16 >= nh || b % 16 >= nd || c / 4 >= ni {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }

        match c % 4 {
            1 => {
                if d >= nl {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
            3 => {
                if d >= ne {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
            2 => {
                if d < bc || d > ec {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
                loop {
                    if !(d < k + bc - fmem_ptr) {
                        break;
                    }
                    let qw = FONT_INFO[(CHAR_BASE[f] + d) as usize].b16;
                    if qw.s1 as i32 % 4 != LIST_TAG {
                        break;
                    }
                    d = qw.s0 as i32
                }
                if d == k + bc - fmem_ptr {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
            _ => {}
        }
    }

    alpha = 16;
    while z >= 0x800000 {
        z = z / 2;
        alpha = alpha + alpha
    }
    beta = (256 / alpha) as u8;
    alpha = alpha * z;

    for k in WIDTH_BASE[f]..=LIG_KERN_BASE[f] - 1 {
        let a = ttstub_input_getc(tfm_file);
        let b = ttstub_input_getc(tfm_file);
        let c = ttstub_input_getc(tfm_file);
        let d = ttstub_input_getc(tfm_file);
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        sw = ((d * z / 256 + c * z) / 256 + b * z) / beta as i32;

        if a == 0 {
            FONT_INFO[k as usize].b32.s1 = sw
        } else if a == 255 {
            FONT_INFO[k as usize].b32.s1 = sw - alpha
        } else {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
    }

    if FONT_INFO[WIDTH_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if FONT_INFO[HEIGHT_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if FONT_INFO[DEPTH_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }
    if FONT_INFO[ITALIC_BASE[f] as usize].b32.s1 != 0 {
        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
    }

    bch_label = 32767;
    bchar_0 = 256;
    if nl > 0 {
        let mut a = 0;
        let mut c = 0;
        let mut d = 0;
        for k in LIG_KERN_BASE[f]..KERN_BASE[f] + 256 * 128 {
            a = ttstub_input_getc(tfm_file);
            let b = ttstub_input_getc(tfm_file);
            c = ttstub_input_getc(tfm_file);
            d = ttstub_input_getc(tfm_file);
            let qw = b16x4 {
                s0: d as u16,
                s1: c as u16,
                s2: b as u16,
                s3: a as u16,
            };
            if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            FONT_INFO[k as usize].b16 = qw;

            if a > 128 {
                if 256 * c + d >= nl {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
                if a == 255 && k == LIG_KERN_BASE[f] {
                    bchar_0 = b as i16
                }
            } else {
                if b != bchar_0 as i32 {
                    if b < bc || b > ec {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }

                    let qw = FONT_INFO[(CHAR_BASE[f] + b) as usize].b16;
                    if !(qw.s3 > 0) {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }
                }

                if c < 128 {
                    if d < bc || d > ec {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }
                    let qw = FONT_INFO[(CHAR_BASE[f] + d) as usize].b16;
                    if !(qw.s3 > 0) {
                        return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                    }
                } else if 256 * (c - 128) + d >= nk {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
                if a < 128 && k - LIG_KERN_BASE[f] + a + 1i32 >= nl {
                    return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
                }
            }
        }
        if a == 255 {
            bch_label = 256 * c + d
        }
    }

    for k in KERN_BASE[f] + 256 * 128..EXTEN_BASE[f] {
        let a = ttstub_input_getc(tfm_file);
        let b = ttstub_input_getc(tfm_file);
        let c = ttstub_input_getc(tfm_file);
        let d = ttstub_input_getc(tfm_file);
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        sw = ((d * z / 256i32 + c * z) / 256i32 + b * z) / beta as i32;
        if a == 0 {
            FONT_INFO[k as usize].b32.s1 = sw
        } else if a == 255 {
            FONT_INFO[k as usize].b32.s1 = sw - alpha
        } else {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
    }

    for k in EXTEN_BASE[f]..PARAM_BASE[f] {
        let a = ttstub_input_getc(tfm_file);
        let b = ttstub_input_getc(tfm_file);
        let c = ttstub_input_getc(tfm_file);
        let d = ttstub_input_getc(tfm_file);
        let qw = b16x4 {
            s0: d as u16,
            s1: c as u16,
            s2: b as u16,
            s3: a as u16,
        };
        if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        FONT_INFO[k as usize].b16 = qw;

        if a != 0 {
            if a < bc || a > ec {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            let qw = FONT_INFO[(CHAR_BASE[f] + a) as usize].b16;
            if !(qw.s3 as i32 > 0i32) {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }

        if b != 0 {
            if b < bc || b > ec {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            let qw = FONT_INFO[(CHAR_BASE[f] + b) as usize].b16;
            if !(qw.s3 > 0) {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }

        if c != 0 {
            if c < bc || c > ec {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            let qw = FONT_INFO[(CHAR_BASE[f] + c) as usize].b16;
            if !(qw.s3 > 0) {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }

        if d < bc || d > ec {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
        let qw = FONT_INFO[(CHAR_BASE[f] + d) as usize].b16;
        if !(qw.s3 > 0) {
            return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
        }
    }

    for k in 1..=np {
        if k == 1 {
            sw = ttstub_input_getc(tfm_file);
            if sw == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            if sw > 127 {
                sw = sw - 256
            }

            sw = sw * 256 + ttstub_input_getc(tfm_file);
            sw = sw * 256 + ttstub_input_getc(tfm_file);
            FONT_INFO[PARAM_BASE[f] as usize].b32.s1 = sw * 16 + ttstub_input_getc(tfm_file) / 16
        } else {
            let a = ttstub_input_getc(tfm_file);
            let b = ttstub_input_getc(tfm_file);
            let c = ttstub_input_getc(tfm_file);
            let d = ttstub_input_getc(tfm_file);
            if a == libc::EOF || b == libc::EOF || c == libc::EOF || d == libc::EOF {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
            sw = ((d * z / 256 + c * z) / 256 + b * z) / beta as i32;
            if a == 0 {
                FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = sw
            } else if a == 255 {
                FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = sw - alpha
            } else {
                return bad_tfm(tfm_file_owner, g, u, nom, aire, s, name_too_long);
            }
        }
    }

    for k in np + 1..8 {
        FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = 0;
    }

    if np >= 7 {
        FONT_PARAMS[f] = np
    } else {
        FONT_PARAMS[f] = 7
    }

    HYPHEN_CHAR[f] = *INTPAR(IntPar::default_hyphen_char);
    SKEW_CHAR[f] = *INTPAR(IntPar::default_skew_char);
    if bch_label < nl {
        BCHAR_LABEL[f] = bch_label + LIG_KERN_BASE[f]
    } else {
        BCHAR_LABEL[f] = NON_ADDRESS;
    }
    FONT_BCHAR[f] = bchar_0 as _;
    FONT_FALSE_BCHAR[f] = bchar_0 as nine_bits;

    if bchar_0 as i32 <= ec {
        if bchar_0 as i32 >= bc {
            let qw = FONT_INFO[(CHAR_BASE[f] + bchar_0 as i32) as usize].b16;
            if qw.s3 as i32 > 0 {
                FONT_FALSE_BCHAR[f] = TOO_BIG_CHAR;
            }
        }
    }

    FONT_NAME[f] = nom;
    FONT_AREA[f] = aire;
    FONT_BC[f] = bc as UTF16_code;
    FONT_EC[f] = ec as UTF16_code;
    FONT_GLUE[f] = None.tex_int();
    PARAM_BASE[f] -= 1;
    fmem_ptr = fmem_ptr + lf;
    FONT_PTR = f;
    g = f;
    FONT_MAPPING[f] = load_tfm_font_mapping();

    return done(tfm_file_owner, g);

    /// Called on error
    unsafe fn bad_tfm(
        tfm_file: Option<InputHandleWrapper>,
        g: usize,
        u: i32,
        nom: i32,
        aire: i32,
        s: i32,
        name_too_long: bool,
    ) -> usize {
        if *INTPAR(IntPar::suppress_fontnotfound_error) == 0 {
            /* NOTE: must preserve this path to keep passing the TRIP tests */
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Font ");
            sprint_cs(u);
            print_chr('=');
            if let Some(qc) = file_name_quote_char {
                print_char(qc as i32);
            }
            print_file_name(nom, aire, cur_ext);
            if let Some(qc) = file_name_quote_char {
                print_char(qc as i32);
            }
            if s >= 0 {
                print_cstr(" at ");
                print_scaled(s);
                print_cstr("pt");
            } else if s != -1000 {
                print_cstr(" scaled ");
                print_int(-s);
            }
            if tfm_file.is_some() {
                print_cstr(" not loadable: Bad metric (TFM) file");
            } else if name_too_long {
                print_cstr(" not loadable: Metric (TFM) file name too long");
            } else {
                print_cstr(" not loadable: Metric (TFM) file or installed font not found");
            }
            help!(
                "I wasn\'t able to read the size data for this font,",
                "so I will ignore the font specification.",
                "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
                "You might try inserting a different font spec;",
                "e.g., type `I\\font<same font id>=<substitute font name>\'."
            );
            error();
        }
        return done(tfm_file, g);
    }

    unsafe fn done(tfm_file: Option<InputHandleWrapper>, g: usize) -> usize {
        let file_opened = tfm_file.is_some();
        if let Some(handle) = tfm_file {
            ttstub_input_close(handle);
        }

        if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
            if g == FONT_BASE {
                begin_diagnostic();
                print_nl_cstr(" -> font not found, using \"nullfont\"");
                end_diagnostic(false);
            } else if file_opened {
                begin_diagnostic();
                print_nl_cstr(" -> ");
                print_c_str(&name_of_file);
                end_diagnostic(false);
            }
        }
        g
    }
}

pub(crate) unsafe fn load_native_font(
    mut u: i32,
    mut nom: str_number,
    mut aire: str_number,
    mut s: i32,
) -> usize {
    let mut font_engine =
        find_native_font(CString::new(name_of_file.as_str()).unwrap().as_ptr(), s);
    if font_engine.is_null() {
        return FONT_BASE;
    }
    let actual_size = if s >= 0 {
        s
    } else if s != -1000 {
        xn_over_d(loaded_font_design_size, -s, 1000)
    } else {
        loaded_font_design_size
    };
    if (pool_ptr as usize) + name_of_file.as_bytes().len() > (pool_size as usize) {
        overflow("pool size", (pool_size - init_pool_ptr) as usize);
    }
    for b in name_of_file.bytes() {
        str_pool[pool_ptr as usize] = b as packed_UTF16_code;
        pool_ptr = pool_ptr + 1;
    }

    let full_name = make_string();

    for f in (0 + 1)..=FONT_PTR {
        if FONT_AREA[f] == native_font_type_flag
            && str_eq_str(FONT_NAME[f], full_name)
            && FONT_SIZE[f] == actual_size
        {
            release_font_engine(font_engine, native_font_type_flag);
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
            return f;
        }
    }

    let num_font_dimens = if native_font_type_flag as u32 == OTGR_FONT_FLAG
        && isOpenTypeMathFont(font_engine as XeTeXLayoutEngine)
    {
        65 // = first_math_fontdimen (=10) + lastMathConstant (= radicalDegreeBottomRaisePercent = 55)
    } else {
        8
    };
    if FONT_PTR == FONT_MAX || fmem_ptr + num_font_dimens > FONT_MEM_SIZE as i32 {
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("Font ");
        sprint_cs(u);
        print_chr('=');
        if let Some(qc) = file_name_quote_char {
            print_char(qc as i32);
        }
        print_file_name(nom, aire, cur_ext);
        if let Some(qc) = file_name_quote_char {
            print_char(qc as i32);
        }
        if s >= 0 {
            print_cstr(" at ");
            print_scaled(s);
            print_cstr("pt");
        } else if s != -1000 {
            print_cstr(" scaled ");
            print_int(-s);
        }
        print_cstr(" not loaded: Not enough room left");
        help!(
            "I\'m afraid I won\'t be able to make use of this font,",
            "because my memory for character-size data is too small.",
            "If you\'re really stuck, ask a wizard to enlarge me.",
            "Or maybe try `I\\font<same font id>=<name of loaded font>\'."
        );
        error();
        return FONT_BASE;
    }
    FONT_PTR += 1;
    FONT_AREA[FONT_PTR] = native_font_type_flag;
    FONT_NAME[FONT_PTR] = full_name;
    FONT_CHECK[FONT_PTR] = b16x4 {
        s3: 0,
        s2: 0,
        s1: 0,
        s0: 0,
    };
    FONT_GLUE[FONT_PTR] = None.tex_int();
    FONT_DSIZE[FONT_PTR] = loaded_font_design_size;
    FONT_SIZE[FONT_PTR] = actual_size;
    
    let (ascent, descent, font_slant, x_ht, cap_ht) =
    match native_font_type_flag as u32 {
        #[cfg(target_os = "macos")]
        AAT_FONT_FLAG => {
            aat::aat_get_font_metrics(
                font_engine as _,
            )
        }
        #[cfg(not(target_os = "macos"))]
        AAT_FONT_FLAG => {
            unreachable!()
        }
        _ => {
            ot_get_font_metrics(
                font_engine
            )
        }
    };
    HEIGHT_BASE[FONT_PTR] = ascent;
    DEPTH_BASE[FONT_PTR] = -descent;
    FONT_PARAMS[FONT_PTR] = num_font_dimens;
    FONT_BC[FONT_PTR] = 0 as UTF16_code;
    FONT_EC[FONT_PTR] = 65535 as UTF16_code;
    font_used[FONT_PTR] = false;
    HYPHEN_CHAR[FONT_PTR] = *INTPAR(IntPar::default_hyphen_char);
    SKEW_CHAR[FONT_PTR] = *INTPAR(IntPar::default_skew_char);
    PARAM_BASE[FONT_PTR] = fmem_ptr - 1;
    FONT_LAYOUT_ENGINE[FONT_PTR] = font_engine;
    FONT_MAPPING[FONT_PTR] = 0 as *mut libc::c_void;
    FONT_LETTER_SPACE[FONT_PTR] = loaded_font_letter_space;
    /* "measure the width of the space character and set up font parameters" */
    let p = new_native_character(FONT_PTR, ' ' as i32);
    s = p.width() + loaded_font_letter_space;
    p.free();

    FONT_INFO[fmem_ptr as usize].b32.s1 = font_slant;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s / 2; // space_stretch
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s / 3; // space_shrink
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = x_ht;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = FONT_SIZE[FONT_PTR]; // quad
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s / 3; // extra_space
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = cap_ht;
    fmem_ptr += 1;
    if num_font_dimens == 65 {
        FONT_INFO[fmem_ptr as usize].b32.s1 = num_font_dimens;
        fmem_ptr += 1;
        for k in 0..=55 {
            /* 55 = lastMathConstant */
            /*:582*/
            FONT_INFO[fmem_ptr as usize].b32.s1 = get_ot_math_constant(FONT_PTR, k);
            fmem_ptr += 1;
        }
    }
    FONT_MAPPING[FONT_PTR] = loaded_font_mapping;
    FONT_FLAGS[FONT_PTR] = loaded_font_flags;
    FONT_PTR
}
