use bridge::TTInputFormat;

use crate::help;

use std::io::Read;

use crate::xetex_ini::b16x4;
use crate::xetex_ini::nine_bits;
use crate::xetex_ini::packed_UTF16_code;
use crate::xetex_ini::str_number;
use crate::xetex_ini::UTF16_code;
use crate::xetex_scaledmath::Scaled;

use crate::xetex_xetexd::print_c_str;
use crate::xetex_xetexd::TeXInt;

use crate::xetex_ini::cur_ext;
use crate::xetex_ini::file_line_error_style_p;
use crate::xetex_ini::fmem_ptr;
use crate::xetex_ini::font_used;
use crate::xetex_ini::loaded_font_design_size;
use crate::xetex_ini::loaded_font_flags;
use crate::xetex_ini::loaded_font_letter_space;
use crate::xetex_ini::loaded_font_mapping;
use crate::xetex_ini::pool_ptr;
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

use crate::xetex_ext::ot_get_font_metrics;
use crate::xetex_ext::{check_for_tfm_font_mapping, load_tfm_font_mapping};
use crate::xetex_ext::{find_native_font, NativeFont::*};

use super::xetex_io::tt_xetex_open_input;
use crate::xetex_consts::IntPar;
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
use crate::xetex_stringpool::PoolString;
use crate::xetex_stringpool::EMPTY_STRING;
use crate::xetex_xetex0::diagnostic;
use crate::xetex_xetex0::new_native_character;
use crate::xetex_xetex0::pack_file_name;

use crate::xetex_layout_interface::get_ot_math_constant;
use crate::xetex_scaledmath::xn_over_d;

#[derive(Clone, Copy, Debug)]
pub(crate) enum TfmError {
    LongName,
    NotFound,
    BadMetric,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum NativeFontError {
    NotFound,
    NotEnoughMemory,
}

pub(crate) unsafe fn read_font_info(
    u: i32,
    nom: str_number,
    aire: str_number,
    s: Scaled,
    quoted_filename: bool,
    file_name_quote_char: Option<u16>,
) -> Result<(bool, usize), TfmError> {
    pack_file_name(nom, aire, cur_ext);

    if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
        diagnostic(false, || {
            print_nl_cstr("Requested font \"");
            print_c_str(&name_of_file);
            print('\"' as i32);
            if s < Scaled::ZERO {
                print_cstr(" scaled ");
                print_int(-s.0);
            } else {
                print_cstr(" at ");
                print_scaled(s);
                print_cstr("pt");
            }
        });
    }

    if quoted_filename {
        if let Ok(g) =
            load_native_font(s).map_err(|e| nf_error(e, u, nom, aire, s, file_name_quote_char))
        {
            return Ok((false, g));
        }
    }

    let name_too_long = length(nom) > 255 || length(aire) > 255;
    if name_too_long {
        return Err(TfmError::LongName);
    }
    pack_file_name(nom, aire, EMPTY_STRING as str_number);
    check_for_tfm_font_mapping();

    let mut tfm_file_owner = tt_xetex_open_input(TTInputFormat::TFM);
    if tfm_file_owner.is_none() {
        if !quoted_filename {
            if let Ok(g) =
                load_native_font(s).map_err(|e| nf_error(e, u, nom, aire, s, file_name_quote_char))
            {
                return Ok((false, g));
            }
        }
        return Err(TfmError::NotFound);
    }

    let tfm_file = tfm_file_owner.as_mut().unwrap();

    /* We are a bit cavalier about EOF-checking since we can't very
     * conveniently implement feof() in the Rust layer, and it only ever is
     * used in this one place. */

    let mut buf16 = [0_u8; 2];

    macro_rules! READFIFTEEN (
        ($x:expr) => {
            tfm_file.read_exact(&mut buf16[..]).map_err(|_| TfmError::BadMetric)?;
            $x = {
                let b16 = i16::from_be_bytes(buf16);
                if b16 < 0 {
                    return Err(TfmError::BadMetric);
                }
                b16 as i32
            };
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
        return Err(TfmError::BadMetric);
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
        return Err(TfmError::BadMetric);
    } else if nw == 0 || nh == 0 || nd == 0 || ni == 0 {
        return Err(TfmError::BadMetric);
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
        return Err(TfmError::BadMetric);
    }
    let mut buf = [0_u8; 4];
    tfm_file
        .read_exact(&mut buf[..])
        .map_err(|_| TfmError::BadMetric)?;
    let [a, b, c, d] = buf;
    FONT_CHECK[f as usize] = b16x4 {
        s0: d as u16,
        s1: c as u16,
        s2: b as u16,
        s3: a as u16,
    };

    tfm_file
        .read_exact(&mut buf[..])
        .map_err(|_| TfmError::BadMetric)?;
    let mut z = Scaled(i32::from_be_bytes(buf) >> 4);
    if z < Scaled(65536) {
        return Err(TfmError::BadMetric);
    }
    while lh > 2 {
        tfm_file.read_exact(&mut buf[..]).ok();
        lh -= 1
    }
    FONT_DSIZE[f] = z;
    if s != Scaled(-1000) {
        z = if s >= Scaled::ZERO {
            s
        } else {
            xn_over_d(z, -s, 1000).0
        };
    }
    FONT_SIZE[f] = z;

    for k in fmem_ptr..WIDTH_BASE[f] {
        tfm_file
            .read_exact(&mut buf[..])
            .map_err(|_| TfmError::BadMetric)?;
        let [a, b, c, d] = buf;
        FONT_INFO[k as usize].b16 = b16x4 {
            s0: d as u16,
            s1: c as u16,
            s2: b as u16,
            s3: a as u16,
        };
        let a = a as i32;
        let b = b as i32;
        let c = c as i32;
        let mut d = d as i32;

        if a >= nw || b / 16 >= nh || b % 16 >= nd || c / 4 >= ni {
            return Err(TfmError::BadMetric);
        }

        match c % 4 {
            1 => {
                if d >= nl {
                    return Err(TfmError::BadMetric);
                }
            }
            3 => {
                if d >= ne {
                    return Err(TfmError::BadMetric);
                }
            }
            2 => {
                if d < bc || d > ec {
                    return Err(TfmError::BadMetric);
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
                    return Err(TfmError::BadMetric);
                }
            }
            _ => {}
        }
    }

    let mut alpha = 16;
    while z >= Scaled(0x800000) {
        z = z / 2;
        alpha *= 2;
    }
    let beta = (256 / alpha) as u8;
    let alpha = z * alpha;

    for k in WIDTH_BASE[f]..LIG_KERN_BASE[f] {
        tfm_file
            .read_exact(&mut buf[..])
            .map_err(|_| TfmError::BadMetric)?;
        let [a, b, c, d] = buf;
        let sw = Scaled(
            (((d as i32) * z.0 / 256 + (c as i32) * z.0) / 256 + (b as i32) * z.0) / beta as i32,
        );

        if a == 0 {
            FONT_INFO[k as usize].b32.s1 = sw.0;
        } else if a == 255 {
            FONT_INFO[k as usize].b32.s1 = (sw - alpha).0;
        } else {
            return Err(TfmError::BadMetric);
        }
    }

    if FONT_INFO[WIDTH_BASE[f] as usize].b32.s1 != 0 {
        return Err(TfmError::BadMetric);
    }
    if FONT_INFO[HEIGHT_BASE[f] as usize].b32.s1 != 0 {
        return Err(TfmError::BadMetric);
    }
    if FONT_INFO[DEPTH_BASE[f] as usize].b32.s1 != 0 {
        return Err(TfmError::BadMetric);
    }
    if FONT_INFO[ITALIC_BASE[f] as usize].b32.s1 != 0 {
        return Err(TfmError::BadMetric);
    }

    let mut bch_label = 32767;
    let mut bchar_0 = 256;
    if nl > 0 {
        let mut a = 0;
        let mut c = 0;
        let mut d = 0;
        for k in LIG_KERN_BASE[f]..KERN_BASE[f] + 256 * 128 {
            tfm_file
                .read_exact(&mut buf[..])
                .map_err(|_| TfmError::BadMetric)?;
            let [a_, b, c_, d_] = buf;
            FONT_INFO[k as usize].b16 = b16x4 {
                s0: d_ as u16,
                s1: c_ as u16,
                s2: b as u16,
                s3: a_ as u16,
            };
            a = a_ as i32;
            let b = b as i32;
            c = c_ as i32;
            d = d_ as i32;

            if a > 128 {
                if 256 * c + d >= nl {
                    return Err(TfmError::BadMetric);
                }
                if a == 255 && k == LIG_KERN_BASE[f] {
                    bchar_0 = b as i16
                }
            } else {
                if b != bchar_0 as i32 {
                    if b < bc || b > ec {
                        return Err(TfmError::BadMetric);
                    }

                    if FONT_INFO[(CHAR_BASE[f] + b) as usize].b16.s3 == 0 {
                        return Err(TfmError::BadMetric);
                    }
                }

                if c < 128 {
                    if d < bc || d > ec {
                        return Err(TfmError::BadMetric);
                    }
                    if FONT_INFO[(CHAR_BASE[f] + d) as usize].b16.s3 == 0 {
                        return Err(TfmError::BadMetric);
                    }
                } else if 256 * (c - 128) + d >= nk {
                    return Err(TfmError::BadMetric);
                }
                if a < 128 && k - LIG_KERN_BASE[f] + a + 1 >= nl {
                    return Err(TfmError::BadMetric);
                }
            }
        }
        if a == 255 {
            bch_label = 256 * c + d
        }
    }

    for k in KERN_BASE[f] + 256 * 128..EXTEN_BASE[f] {
        tfm_file
            .read_exact(&mut buf[..])
            .map_err(|_| TfmError::BadMetric)?;
        let [a, b, c, d] = buf;

        let sw = Scaled(
            (((d as i32) * z.0 / 256 + (c as i32) * z.0) / 256 + (b as i32) * z.0) / beta as i32,
        );
        if a == 0 {
            FONT_INFO[k as usize].b32.s1 = sw.0;
        } else if a == 255 {
            FONT_INFO[k as usize].b32.s1 = (sw - alpha).0;
        } else {
            return Err(TfmError::BadMetric);
        }
    }

    for k in EXTEN_BASE[f]..PARAM_BASE[f] {
        tfm_file
            .read_exact(&mut buf[..])
            .map_err(|_| TfmError::BadMetric)?;
        let [a, b, c, d] = buf;
        FONT_INFO[k as usize].b16 = b16x4 {
            s0: d as u16,
            s1: c as u16,
            s2: b as u16,
            s3: a as u16,
        };
        let a = a as i32;
        let b = b as i32;
        let c = c as i32;
        let d = d as i32;

        if a != 0 {
            if a < bc || a > ec {
                return Err(TfmError::BadMetric);
            }
            if FONT_INFO[(CHAR_BASE[f] + a) as usize].b16.s3 == 0 {
                return Err(TfmError::BadMetric);
            }
        }

        if b != 0 {
            if b < bc || b > ec {
                return Err(TfmError::BadMetric);
            }
            if FONT_INFO[(CHAR_BASE[f] + b) as usize].b16.s3 == 0 {
                return Err(TfmError::BadMetric);
            }
        }

        if c != 0 {
            if c < bc || c > ec {
                return Err(TfmError::BadMetric);
            }
            if FONT_INFO[(CHAR_BASE[f] + c) as usize].b16.s3 == 0 {
                return Err(TfmError::BadMetric);
            }
        }

        if d < bc || d > ec {
            return Err(TfmError::BadMetric);
        }
        if FONT_INFO[(CHAR_BASE[f] + d) as usize].b16.s3 == 0 {
            return Err(TfmError::BadMetric);
        }
    }

    for k in 1..=np {
        if k == 1 {
            tfm_file
                .read_exact(&mut buf[..])
                .map_err(|_| TfmError::BadMetric)?;
            FONT_INFO[PARAM_BASE[f] as usize].b32.s1 = i32::from_be_bytes(buf) >> 4;
        } else {
            tfm_file
                .read_exact(&mut buf[..])
                .map_err(|_| TfmError::BadMetric)?;
            let [a, b, c, d] = buf;

            let sw = Scaled(
                (((d as i32) * z.0 / 256 + (c as i32) * z.0) / 256 + (b as i32) * z.0)
                    / beta as i32,
            );
            if a == 0 {
                FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = sw.0;
            } else if a == 255 {
                FONT_INFO[(PARAM_BASE[f] + k - 1) as usize].b32.s1 = (sw - alpha).0;
            } else {
                return Err(TfmError::BadMetric);
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
            if FONT_INFO[(CHAR_BASE[f] + bchar_0 as i32) as usize].b16.s3 > 0 {
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
    FONT_MAPPING[f] = load_tfm_font_mapping();

    return Ok((true, f));
}

/// Called on error
pub(crate) unsafe fn bad_tfm(
    err: TfmError,
    u: i32,
    nom: i32,
    aire: i32,
    s: Scaled,
    file_name_quote_char: Option<u16>,
) {
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
        if s >= Scaled::ZERO {
            print_cstr(" at ");
            print_scaled(s);
            print_cstr("pt");
        } else if s != Scaled(-1000) {
            print_cstr(" scaled ");
            print_int(-s.0);
        }
        match err {
            TfmError::BadMetric => print_cstr(" not loadable: Bad metric (TFM) file"),
            TfmError::LongName => print_cstr(" not loadable: Metric (TFM) file name too long"),
            TfmError::NotFound => {
                print_cstr(" not loadable: Metric (TFM) file or installed font not found")
            }
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
    if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
        diagnostic(false, || {
            print_nl_cstr(" -> font not found, using \"nullfont\"")
        });
    }
}

pub(crate) fn good_tfm(ok: (bool, usize)) -> usize {
    unsafe {
        if *INTPAR(IntPar::xetex_tracing_fonts) > 0 {
            if ok.0 {
                diagnostic(false, || {
                    print_nl_cstr(" -> ");
                    print_c_str(&name_of_file);
                });
            }
        }
    }
    ok.1
}

pub(crate) unsafe fn load_native_font(mut s: Scaled) -> Result<usize, NativeFontError> {
    let font_engine = find_native_font(&name_of_file, s);
    if font_engine.is_none() {
        return Err(NativeFontError::NotFound);
    }
    let font_engine = font_engine.unwrap();
    let actual_size = if s >= Scaled::ZERO {
        s
    } else if s != Scaled(-1000) {
        xn_over_d(loaded_font_design_size, -s, 1000).0
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

    for f in 1..FONT_PTR + 1 {
        if FONT_AREA[f] == font_engine.flag() as i32
            && PoolString::from(FONT_NAME[f]) == PoolString::from(full_name)
            && FONT_SIZE[f] == actual_size
        {
            str_ptr -= 1;
            pool_ptr = str_start[(str_ptr - TOO_BIG_CHAR) as usize];
            return Ok(f);
        }
    }

    let num_font_dimens = match &font_engine {
        Otgr(e) if e.is_open_type_math_font() => 65,
        // = first_math_fontdimen (=10) + lastMathConstant (= radicalDegreeBottomRaisePercent = 55)
        _ => 8,
    };
    if FONT_PTR == FONT_MAX || fmem_ptr + num_font_dimens > FONT_MEM_SIZE as i32 {
        return Err(NativeFontError::NotEnoughMemory);
    }
    FONT_PTR += 1;
    FONT_AREA[FONT_PTR] = font_engine.flag() as i32;
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

    let (ascent, descent, x_ht, cap_ht, font_slant) = match &font_engine {
        #[cfg(target_os = "macos")]
        Aat(fe) => crate::xetex_aatfont::aat_get_font_metrics(*fe),
        Otgr(fe) => ot_get_font_metrics(fe),
    };
    HEIGHT_BASE[FONT_PTR] = ascent.0;
    DEPTH_BASE[FONT_PTR] = -descent.0;
    FONT_PARAMS[FONT_PTR] = num_font_dimens;
    FONT_BC[FONT_PTR] = 0 as UTF16_code;
    FONT_EC[FONT_PTR] = 65535 as UTF16_code;
    font_used[FONT_PTR] = false;
    HYPHEN_CHAR[FONT_PTR] = *INTPAR(IntPar::default_hyphen_char);
    SKEW_CHAR[FONT_PTR] = *INTPAR(IntPar::default_skew_char);
    PARAM_BASE[FONT_PTR] = fmem_ptr - 1;
    FONT_LAYOUT_ENGINE[FONT_PTR] = crate::xetex_ext::Font::Native(font_engine);
    FONT_MAPPING[FONT_PTR] = 0 as *mut libc::c_void;
    FONT_LETTER_SPACE[FONT_PTR] = loaded_font_letter_space;
    /* "measure the width of the space character and set up font parameters" */
    let p = new_native_character(FONT_PTR, ' ' as i32);
    let s = p.width() + loaded_font_letter_space;
    p.free();

    FONT_INFO[fmem_ptr as usize].b32.s1 = font_slant.0;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = s.0;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = (s / 2).0; // space_stretch
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = (s / 3).0; // space_shrink
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = x_ht.0;
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = FONT_SIZE[FONT_PTR].0; // quad
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = (s / 3).0; // extra_space
    fmem_ptr += 1;
    FONT_INFO[fmem_ptr as usize].b32.s1 = cap_ht.0;
    fmem_ptr += 1;
    if num_font_dimens == 65 {
        FONT_INFO[fmem_ptr as usize].b32.s1 = num_font_dimens;
        fmem_ptr += 1;
        for k in 0..=55 {
            /* 55 = lastMathConstant */
            /*:582*/
            FONT_INFO[fmem_ptr as usize].b32.s1 = get_ot_math_constant(FONT_PTR, k).0;
            fmem_ptr += 1;
        }
    }
    FONT_MAPPING[FONT_PTR] = loaded_font_mapping;
    FONT_FLAGS[FONT_PTR] = loaded_font_flags;
    Ok(FONT_PTR)
}

unsafe fn nf_error(
    e: NativeFontError,
    u: i32,
    nom: str_number,
    aire: str_number,
    s: Scaled,
    file_name_quote_char: Option<u16>,
) {
    match e {
        NativeFontError::NotFound => {}
        NativeFontError::NotEnoughMemory => {
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
            if s >= Scaled::ZERO {
                print_cstr(" at ");
                print_scaled(s);
                print_cstr("pt");
            } else if s != Scaled(-1000) {
                print_cstr(" scaled ");
                print_int(-s.0);
            }
            print_cstr(" not loaded: Not enough room left");
            help!(
                "I\'m afraid I won\'t be able to make use of this font,",
                "because my memory for character-size data is too small.",
                "If you\'re really stuck, ask a wizard to enlarge me.",
                "Or maybe try `I\\font<same font id>=<name of loaded font>\'."
            );
            error();
        }
    }
}
