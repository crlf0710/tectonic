/// Write a floating-point number using "engineering" notation
///
/// Analogous to the %g format of the C printf function, this method switches
/// between naive and scientific notation for floating-point numbers when the
/// number being printed becomes so small that printing leading zeroes could end
/// up larger than the scientific notation, or so large that we would be forced
/// to print more significant digits than requested.
///
pub(crate) fn write_engineering(
    writer: &mut impl std::io::Write,
    x: f64,
    sig_digits: Option<usize>,
) -> std::io::Result<()> {
    let sig_digits = sig_digits.unwrap_or(1);
    let mut precision = sig_digits - 1;
    if x == 0. {
        // Zero is special because you can't take its log
        write!(writer, "0")
    } else {
        // Otherwise, use log to evaluate order of magnitude
        let log_x = x.abs().log10();
        if log_x >= -3. && log_x <= (sig_digits as f64) {
            // Print using naive notation
            //
            // Since Rust's precision controls number of digits after the
            // decimal point, we must adjust it depending on magnitude in order
            // to operate at a constant number of significant digits.
            precision = (precision as isize - log_x.trunc() as isize) as usize;

            // Numbers smaller than 1 must get one extra digit since the leading
            // zero does not count as a significant digit.
            if log_x < 0. {
                precision += 1
            }

            // People don't normally expect trailing zeros or decimal point in
            // naive notation, but be careful with integer numbers...
            let str_with_zeros = format!("{:.1$}", x, precision);
            if str_with_zeros.contains('.') {
                write!(
                    writer,
                    "{}",
                    str_with_zeros.trim_end_matches('0').trim_end_matches('.')
                )
            } else {
                write!(writer, "{}", str_with_zeros)
            }
        } else {
            // Print using scientific notation
            let s = format!("{:.1$e}", x, precision);
            write!(writer, "{}", replace_exp(s))
        }
    }
}

fn replace_exp(s: String) -> String {
    if let Some(pos) = s.bytes().position(|b| b == b'e') {
        if s.as_bytes()[pos + 1] == b'-' {
            if pos == s.len() - 3 {
                s.replace("e-", "e-0")
            } else {
                s
            }
        } else {
            if pos == s.len() - 2 {
                s.replace("e", "e+0")
            } else {
                s.replace("e", "e+")
            }
        }
    } else {
        s
    }
}
