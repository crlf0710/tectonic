// Copyright 2018 the Tectonic Project
// Licensed under the MIT License.

//! Note: we need to store this code as `tests/util/mod.rs` rather than
//! `tests/util.rs` because otherwise Cargo thinks it is a test executable of
//! its own.

// An item is considered unused if at least one testing binary
// has no reference to it. This yields a lot of false-positives
// using this testing setup...
#![allow(dead_code)]


use ::flate2::read::GzDecoder;
use std::collections::{HashMap, HashSet};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use ::tectonic::errors::Result;
pub use ::tectonic::test_util::{test_path, TestBundle};

/// Set the magic environment variable that enables the testing infrastructure
/// embedded in the main Tectonic crate. This function is separated out from
/// the main crate because it embeds `CARGO_MANIFEST_DIR`, which is not
/// something we want to leak into the binary artifacts we produce.
pub fn set_test_root() {
    ::tectonic::test_util::set_test_root_augmented(env!("CARGO_MANIFEST_DIR"));
}

// Duplicated from Cargo's own testing code:
// https://github.com/rust-lang/cargo/blob/19fdb308/tests/cargotest/support/mod.rs#L305-L318
pub fn cargo_dir() -> PathBuf {
    env::var_os("CARGO_BIN_PATH")
        .map(PathBuf::from)
        .or_else(|| {
            env::current_exe().ok().map(|mut path| {
                path.pop();
                if path.ends_with("deps") {
                    path.pop();
                }
                path
            })
        })
        .unwrap_or_else(|| panic!("CARGO_BIN_PATH wasn't set. Cannot continue running test"))
}

/// Generate a plain.fmt file using local files only -- a variety of tests
/// need such a file to exist.
///
/// Note that because tests are run in parallel, this can get quite racy. At
/// the moment we just let everybody write and overwrite the file, but we
/// could use a locking scheme to get smarter about this.
pub fn ensure_plain_format() -> Result<PathBuf> {
    use ::tectonic::engines::NoopIoEventBackend;
    use ::tectonic::io::{
        try_open_file, FilesystemIo, FilesystemPrimaryInputIo, IoStack, MemoryIo,
    };
    use ::tectonic::status::NoopStatusBackend;
    use ::tectonic::TexEngine;

    let fmt_path = test_path(&["plain.fmt"]);

    if try_open_file(&fmt_path).is_not_available() {
        let mut mem = MemoryIo::new(true);

        let mut assets_dir = test_path(&["assets"]);
        let mut fs_support = FilesystemIo::new(&assets_dir, false, false, HashSet::new());

        assets_dir.push("plain");
        assets_dir.set_extension("tex");
        let mut fs_primary = FilesystemPrimaryInputIo::new(&assets_dir);

        {
            let mut io = IoStack::new(vec![&mut mem, &mut fs_primary, &mut fs_support]);

            TexEngine::new()
                .halt_on_error_mode(true)
                .initex_mode(true)
                .process(
                    &mut io,
                    &mut NoopIoEventBackend::new(),
                    &mut NoopStatusBackend::new(),
                    "UNUSED.fmt",
                    "plain.tex",
                )?;
        }

        let mut temp_fmt = tempfile::Builder::new()
            .prefix("plain_fmt")
            .rand_bytes(6)
            .tempfile_in(test_path(&[]))?;
        temp_fmt.write_all(mem.files.borrow().get(OsStr::new("plain.fmt")).unwrap())?;
        temp_fmt.persist(&fmt_path)?;
    }

    Ok(fmt_path)
}

/// Convenience structure for comparing expected and actual output in various
/// tests.
pub struct ExpectedInfo {
    name: OsString,
    contents: Vec<u8>,
    gzipped: bool,
}

impl ExpectedInfo {
    pub fn read<P: AsRef<Path>>(path: P) -> Self {
        let path = path.as_ref();
        let name = path
            .file_name()
            .unwrap_or_else(|| panic!("couldn't get file name of {:?}", path))
            .to_owned();

        let mut f = File::open(path).unwrap_or_else(|_| panic!("failed to open {:?}", path));
        let mut contents = Vec::new();
        f.read_to_end(&mut contents).unwrap();

        ExpectedInfo {
            name,
            contents,
            gzipped: false,
        }
    }

    pub fn read_with_extension(pbase: &mut PathBuf, extension: &str) -> Self {
        pbase.set_extension(extension);
        Self::read(pbase)
    }

    pub fn read_with_extension_gz(pbase: &mut PathBuf, extension: &str) -> Self {
        pbase.set_extension(extension);
        let name = pbase.file_name().unwrap().to_owned();

        let mut dec = GzDecoder::new(File::open(pbase).unwrap());
        let mut contents = Vec::new();
        dec.read_to_end(&mut contents).unwrap();

        ExpectedInfo {
            name,
            contents,
            gzipped: true,
        }
    }

    pub fn test_data(&self, observed: &[u8]) {
        if self.contents == observed {
            return;
        }

        // For nontrivial tests, it's really tough to figure out what
        // changed without being able to do diffs, etc. So, write out the
        // buffers.
        {
            let mut n = self.name.clone();
            n.push(".expected");
            let mut f = File::create(&n).unwrap_or_else(|_| {
                panic!(
                    "failed to create {} for test failure diagnosis",
                    n.to_string_lossy()
                )
            });
            f.write_all(&self.contents).unwrap_or_else(|_| {
                panic!(
                    "failed to write {} for test failure diagnosis",
                    n.to_string_lossy()
                )
            });
        }
        {
            let mut n = self.name.clone();
            n.push(".observed");
            let mut f = File::create(&n).unwrap_or_else(|_| {
                panic!(
                    "failed to create {} for test failure diagnosis",
                    n.to_string_lossy()
                )
            });
            f.write_all(observed).unwrap_or_else(|_| {
                panic!(
                    "failed to write {} for test failure diagnosis",
                    n.to_string_lossy()
                )
            });
        }
        panic!(
            "difference in {}; contents saved to disk",
            self.name.to_string_lossy()
        );
    }

    pub fn test_from_collection(&self, files: &HashMap<OsString, Vec<u8>>) {
        if !self.gzipped {
            if let Some(data) = files.get(&self.name) {
                self.test_data(data)
            } else {
                panic!(
                    "{:?} not in {:?}",
                    self.name,
                    files.keys().collect::<Vec<_>>()
                )
            }
        } else {
            let mut buf = Vec::new();
            let mut dec = GzDecoder::new(&files.get(&self.name).unwrap()[..]);
            dec.read_to_end(&mut buf).unwrap();
            self.test_data(&buf);
        }
    }
}

pub enum OutputEncoding {
    Binary,
    Utf8,
}

pub struct OutputSnapshotChecker {
    name: OsString,
    encoding: OutputEncoding,
    gzipped: bool,
}

impl OutputSnapshotChecker {
    fn create(path: &impl AsRef<Path>, encoding: OutputEncoding) -> Self{
        let path = path.as_ref();
        let name = path
            .file_name()
            .unwrap_or_else(|| panic!("couldn't get file name of {:?}", path))
            .to_owned();
        OutputSnapshotChecker {
            name,
            encoding,
            gzipped: false
        }
    }

    pub fn new(pbase: &mut PathBuf, ext: &str, enc: OutputEncoding) -> Self {
        pbase.set_extension(ext);
        OutputSnapshotChecker::create(pbase, enc)
    }

    pub fn new_gz(pbase: &mut PathBuf, ext: &str, enc: OutputEncoding) -> Self {
        let mut neu = OutputSnapshotChecker::new(pbase, ext, enc);
        neu.gzipped = true;
        neu
    }

    fn get_snapshot_string(&self, observed: &[u8]) -> String {
        match self.encoding {
            OutputEncoding::Utf8 => {
                let st = String::from_utf8(observed.to_vec()).unwrap();
                st
            }
            OutputEncoding::Binary => {
                let hex = pretty_hex::pretty_hex(&observed);
                hex
            }
        }
    }

    pub fn snapshot<'a>(&'a self, files: &HashMap<OsString, Vec<u8>>) -> (&'a str, String) {
        let name = self.name.to_str().unwrap();
        let string = if !self.gzipped {
            if let Some(data) = files.get(&self.name) {
                self.get_snapshot_string(data)
            } else {
                panic!(
                    "{:?} not in {:?}",
                    self.name,
                    files.keys().collect::<Vec<_>>()
                )
            }
        } else {
            let mut buf = Vec::new();
            let mut dec = GzDecoder::new(&files.get(&self.name).unwrap()[..]);
            dec.read_to_end(&mut buf).unwrap();
            self.get_snapshot_string(&buf)
        };
        (name, string)
    }
}
