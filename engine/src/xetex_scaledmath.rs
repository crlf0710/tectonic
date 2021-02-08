/* tectonic/xetex-scaledmath.c: low-level math functions
   Copyright 2017 The Tectonic Project
   Licensed under the MIT License.
*/
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use crate::xetex_ini::arith_error;

#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    derive_more::Add,
    derive_more::Sub,
    derive_more::Neg,
    derive_more::AddAssign,
    derive_more::SubAssign,
)]
#[repr(transparent)]
pub struct Scaled(pub i32);

impl Scaled {
    pub const ZERO: Self = Self(0);
    pub const ONE: Self = Self(65536);
    pub const INFINITY: Self = Self(0x7fff_ffff);
    pub const MAX_HALFWORD: Self = Self(0x3fff_ffff);
}
impl Scaled {
    pub fn min(self, other: Self) -> Self {
        if self.0 < other.0 {
            self
        } else {
            other
        }
    }
    pub fn max(self, other: Self) -> Self {
        if self.0 > other.0 {
            self
        } else {
            other
        }
    }
    pub fn abs(self) -> Self {
        if self.0 >= 0 {
            self
        } else {
            -self
        }
    }
    pub fn half(self) -> Self {
        let x = self.0;
        if x & 1 != 0 {
            Self((x + 1) / 2)
        } else {
            Self(x / 2)
        }
    }
    pub unsafe fn mul_add(mut self, mut n: i32, y: Self) -> Self {
        let max_answer = Self::MAX_HALFWORD;
        if n < 0 {
            self = -self;
            n = -n
        }
        if n == 0 {
            y
        } else if self <= (max_answer - y) / n && -self <= (max_answer + y) / n {
            self * n + y
        } else {
            arith_error = true;
            Self::ZERO
        }
    }

    pub(crate) unsafe fn add_or_sub(self, mut y: Self, negative: bool) -> Self {
        let max_answer = Self::MAX_HALFWORD;
        if negative {
            y = -y
        }
        if self >= Self::ZERO {
            if y <= max_answer - self {
                self + y
            } else {
                arith_error = true;
                Self::ZERO
            }
        } else if y >= -max_answer - self {
            self + y
        } else {
            arith_error = true;
            Self::ZERO
        }
    }

    pub(crate) unsafe fn quotient(self, d: i32) -> Self {
        Self(crate::xetex_xetex0::quotient(self.0, d))
    }
    pub(crate) unsafe fn fract(self, n: Self, d: Self) -> Self {
        Self(crate::xetex_xetex0::fract(
            self.0,
            n.0,
            d.0,
            Self::MAX_HALFWORD.0,
        ))
    }
}

impl std::fmt::Display for Scaled {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = self.0;
        if s < 0 {
            '-'.fmt(f)?;
            s = s.wrapping_neg(); // TODO: check
        }
        (s >> 16).fmt(f)?;
        '.'.fmt(f)?;
        s = 10 * (s & 0xffff) + 5;
        let mut delta = 10;
        loop {
            if delta > 0x10000 {
                s = s + 0x8000 - 50000
            }
            char::from(b'0' + (s >> 16) as u8).fmt(f)?;
            s = 10 * (s & 0xffff);
            delta *= 10;
            if s <= delta {
                break;
            }
        }
        Ok(())
    }
}

impl From<f64> for Scaled {
    fn from(d: f64) -> Self {
        Self((d * 65536. + 0.5) as i32)
    }
}
impl From<f32> for Scaled {
    fn from(f: f32) -> Self {
        (f as f64).into()
    }
}
impl From<Scaled> for f64 {
    fn from(s: Scaled) -> Self {
        s.0 as f64 / 65536.
    }
}
impl From<Scaled> for f32 {
    fn from(s: Scaled) -> Self {
        f64::from(s) as f32
    }
}

impl From<i32> for Scaled {
    fn from(v: i32) -> Self {
        Self(v * Self::ONE.0)
    }
}

impl core::ops::Mul<i32> for Scaled {
    type Output = Self;
    fn mul(self, i: i32) -> Self::Output {
        Self(self.0 * i)
    }
}
impl core::ops::Div<i32> for Scaled {
    type Output = Self;
    fn div(self, i: i32) -> Self::Output {
        Self(self.0 / i)
    }
}

impl core::ops::Div for Scaled {
    type Output = i32;
    fn div(self, i: Self) -> Self::Output {
        self.0 / i.0
    }
}

impl core::ops::Rem for Scaled {
    type Output = Self;
    fn rem(self, i: Self) -> Self::Output {
        Self(self.0 % i.0)
    }
}

pub(crate) unsafe fn tex_round(r: f64) -> Scaled {
    /* We must reproduce very particular rounding semantics to pass the TRIP
     * test. Specifically, values within the 32-bit range of TeX integers are
     * rounded to the nearest integer with half-integral values going away
     * from zero: 0.5 => 1, -0.5 => -1.
     *
     * `r` does not necessarily lie within the range of a 32-bit TeX integer;
     * if it doesn't, we clip. The following LaTeX document allegedly triggers
     * that codepath:
     *
     *   \documentstyle{article}
     *   \begin{document}
     *   \begin{flushleft}
     *   $\hbox{} $\hfill
     *   \filbreak
     *   \eject
     *
     */
    if r > 2147483647.0f64 {
        /* 0x7FFFFFFF */
        return Scaled(2147483647);
    }
    if r < -2147483648.0f64 {
        /* -0x80000000 */
        return Scaled(-2147483648);
    }
    /* ANSI defines the float-to-integer cast to truncate towards zero, so the
     * following code is all that's necessary to get the desired behavior. The
     * truncation technically causes an uncaught "inexact" floating-point
     * exception, but exception is virtually impossible to avoid in real
     * code. */
    if r >= 0.0f64 {
        return Scaled((r + 0.5) as i32);
    }
    Scaled((r - 0.5) as i32)
}
pub(crate) unsafe fn mult_and_add(mut n: i32, mut x: i32, y: i32, max_answer: i32) -> i32 {
    if n < 0 {
        x = -x;
        n = -n
    }
    if n == 0 {
        y
    } else if x <= (max_answer - y) / n && -x <= (max_answer + y) / n {
        x * n + y
    } else {
        arith_error = true;
        0
    }
}
pub(crate) unsafe fn x_over_n(x: Scaled, mut n: i32) -> (Scaled, Scaled) {
    let mut x = x.0;
    if n == 0 {
        arith_error = true;
        (Scaled::ZERO, Scaled(x))
    } else {
        if n < 0 {
            // negative
            x = -x;
            n = -n;
        }
        if x >= 0 {
            (Scaled(x / n), Scaled(x % n))
        } else {
            (Scaled(-(-x / n)), Scaled(-(-x % n)))
        }
    }
}
/* xetex-errors */
/* xetex-math */
/* xetex-output */
/* xetex-pagebuilder */
/* xetex-scaledmath */
pub(crate) unsafe fn xn_over_d(x: Scaled, n: Scaled, d: i32) -> (Scaled, i32) {
    let mut x = x.0;
    let n = n.0;
    let positive = if x >= 0 {
        true
    } else {
        x = x.wrapping_neg(); // TODO: check
        false
    };
    let t = (x as i64 % 32768 * n as i64) as i32;
    let mut u = (x as i64 / 32768 * n as i64 + t as i64 / 32768) as i32;
    let v = ((u % d) as i64 * 32768 + t as i64 % 32768) as i32;
    if (u / d) as i64 >= 32768 {
        arith_error = true
    } else {
        u = (32768 * (u / d) as i64 + (v / d) as i64) as i32
    }
    if positive {
        (Scaled(u), v % d)
    } else {
        (Scaled(-u), -(v % d))
    }
}
pub(crate) unsafe fn round_xn_over_d(x: Scaled, n: i32, d: i32) -> Scaled {
    let mut x = x.0;
    let positive = if x >= 0 {
        true
    } else {
        x = -x;
        false
    };
    let t = (x as i64 % 32768 * n as i64) as i32;
    let mut u = (x as i64 / 32768 * n as i64 + t as i64 / 32768) as i32;
    let v = ((u % d) as i64 * 32768 + t as i64 % 32768) as i32;
    if (u / d) as i64 >= 32768 {
        arith_error = true
    } else {
        u = (32768 * (u / d) as i64 + (v / d) as i64) as i32
    }
    let v = v % d;
    if 2 * v >= d {
        u += 1
    }
    if positive {
        Scaled(u)
    } else {
        Scaled(-u)
    }
}
