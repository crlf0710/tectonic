// src/engines/xdvipdfmx.rs -- Rustic interface to the xdvipdfmx translator.
// Copyright 2017 the Tectonic Project
// Licensed under the MIT License.

use super::{ExecutionState, IoEventBackend, TectonicBridgeApi};
use crate::errors::{ErrorKind, Result};
use crate::io::IoStack;
use crate::status::StatusBackend;
use crate::unstable_opts::UnstableOptions;

#[no_mangle]
extern "C" {
    static mut paperspec: *const libc::c_char;
}

pub struct XdvipdfmxEngine {
    enable_compression: bool,
    deterministic_tags: bool,
}

impl XdvipdfmxEngine {
    pub fn new() -> XdvipdfmxEngine {
        XdvipdfmxEngine {
            enable_compression: true,
            deterministic_tags: false,
        }
    }

    pub fn with_compression(mut self, enable_compression: bool) -> Self {
        self.enable_compression = enable_compression;
        self
    }

    pub fn with_deterministic_tags(mut self, flag: bool) -> Self {
        self.deterministic_tags = flag;
        self
    }

    pub fn process(
        &mut self,
        io: &mut IoStack,
        events: &mut dyn IoEventBackend,
        status: &mut dyn StatusBackend,
        dvi: &str,
        pdf: &str,
        unstables: &UnstableOptions,
    ) -> Result<i32> {
        let _guard = super::ENGINE_LOCK.lock().unwrap(); // until we're thread-safe ...

        let paperspec_str = unstables
            .paper_size
            .as_ref()
            .and_then(|s| CString::new(s.clone()).ok());
        if let Some(cstr) = paperspec_str.as_ref() {
            unsafe {
                paperspec = cstr.as_ptr();
            }
        }

        let /*mut*/ state = ExecutionState::new(io, events, status);
        let bridge = TectonicBridgeApi::new(&state);

        unsafe {
            match super::dvipdfmx_simple_main(
                &*bridge,
                dvi,
                pdf,
                self.enable_compression,
                self.deterministic_tags,
            ) {
                99 => {
                    let msg = super::tt_get_error_message().to_string();
                    Err(ErrorKind::Msg(msg).into())
                }
                x => Ok(x as i32),
            }
        }
    }
}

impl Default for XdvipdfmxEngine {
    fn default() -> Self {
        XdvipdfmxEngine::new()
    }
}
