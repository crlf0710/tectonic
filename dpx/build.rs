// build.rs -- build helper script for Tectonic.
// Copyright 2016-2019 the Tectonic Project
// Licensed under the MIT License.

/// The Tectonic build script. Not only do we have internal C/C++ code, we
/// also depend on several external C/C++ libraries, so there's a lot to do
/// here. It would be great to streamline things.
///
/// TODO: this surely needs to become much smarter and more flexible.
use std::env;
use std::path::PathBuf;

fn main() {
    let target = env::var("TARGET").unwrap();
    let mut ccfg = cc::Build::new();
    let cflags = [
        "-Wall",
        "-Wcast-qual",
        "-Wdate-time",
        "-Wendif-labels",
        "-Wextra",
        "-Wextra-semi",
        "-Wformat=2",
        "-Winit-self",
        "-Wlogical-op",
        "-Wmissing-declarations",
        "-Wmissing-include-dirs",
        "-Wmissing-prototypes",
        "-Wmissing-variable-declarations",
        "-Wnested-externs",
        "-Wold-style-definition",
        "-Wpointer-arith",
        "-Wredundant-decls",
        "-Wstrict-prototypes",
        "-Wswitch-bool",
        "-Wundef",
        "-Wwrite-strings",
        // TODO: Fix existing warnings before enabling these:
        // "-Wbad-function-cast",
        // "-Wcast-align",
        // "-Wconversion",
        // "-Wdouble-promotion",
        // "-Wshadow",
        // "-Wsuggest-attribute=format",
        // "-Wsuggest-attribute=const",
        // "-Wsuggest-attribute=noreturn",
        // "-Wsuggest-attribute=pure",
        // "-Wunreachable-code-aggresive",
        "-Wno-unused-parameter",
        "-Wno-implicit-fallthrough",
        "-Wno-sign-compare",
        "-std=gnu11",
    ];

    for flag in &cflags {
        ccfg.flag_if_supported(flag);
    }

    ccfg.flag("-Wall").file("shims/dpx_shims.c").include(".");

    if target.contains("-msvc") {
        ccfg.flag("/EHsc");
    }

    // OK, back to generic build rules.
    ccfg.compile("dpx_shims.a");

    for file in PathBuf::from("shims").read_dir().unwrap() {
        let file = file.unwrap();
        println!("cargo:rerun-if-changed={}", file.path().display());
    }
}
