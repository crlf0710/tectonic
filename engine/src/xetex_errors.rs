/* tectonic/errors.c -- error handling
 * Copyright 2016 the Tectonic Project
 * Licensed under the MIT License.
*/
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use std::io::Write;

use crate::help;
use crate::{t_eprint, t_print, t_print_nl};

use crate::cmd::InteractionMode;
use crate::xetex_ini::tt_cleanup;
use crate::xetex_ini::{
    error_count, halt_on_error_p, help_line, help_ptr, history, interaction, job_name, log_opened,
    rust_stdout, selector, use_err_help,
};
use crate::xetex_output::print_ln;
use crate::xetex_xetex0::{close_files_and_terminate, give_err_help, open_log_file, show_context};

use bridge::TTHistory;

use crate::xetex_ini::{cur_input, Selector, INPUT_PTR, INPUT_STACK};

pub(crate) trait Confuse {
    type Output;
    fn confuse(self, message: &str) -> Self::Output;
}

impl<T> Confuse for Option<T> {
    type Output = T;
    fn confuse(self, message: &str) -> Self::Output {
        match self {
            Some(v) => v,
            None => unsafe { confusion(message) },
        }
    }
}

/* WEBby error-handling code: */
unsafe fn pre_error_message() {
    /* FKA normalize_selector(): */
    if log_opened {
        selector = Selector::TERM_AND_LOG
    } else {
        selector = Selector::TERM_ONLY
    }
    if job_name == 0 {
        open_log_file();
    }
    if interaction == InteractionMode::Batch {
        selector = match selector {
            Selector::TERM_ONLY => Selector::NO_PRINT,
            Selector::TERM_AND_LOG => Selector::LOG_ONLY,
            _ => unreachable!(),
        };
    }
}
/*82: */
unsafe fn post_error_message(need_to_print_it: i32) {
    if interaction == InteractionMode::ErrorStop {
        interaction = InteractionMode::Scroll;
    }
    if need_to_print_it != 0 && log_opened {
        error();
    }
    history = TTHistory::FATAL_ERROR;
    close_files_and_terminate();
    tt_cleanup();
    rust_stdout.as_mut().unwrap().flush().unwrap();
}
pub(crate) unsafe fn error() {
    if (history as u32) < (TTHistory::ERROR_ISSUED as u32) {
        history = TTHistory::ERROR_ISSUED
    }
    t_print!(".");
    INPUT_STACK[INPUT_PTR] = cur_input;
    show_context(&INPUT_STACK[..INPUT_PTR + 1]);
    if halt_on_error_p != 0 {
        history = TTHistory::FATAL_ERROR;
        post_error_message(0);
        abort!("halted on potentially-recoverable error as specified");
    }
    /* This used to be where there was a bunch of code if "interaction ==
     * error_stop_mode" that would let the use interactively try to solve the
     * error. */
    error_count += 1;
    if error_count as i32 == 100 {
        t_print_nl!("(That makes 100 errors; please try again.)");
        history = TTHistory::FATAL_ERROR;
        post_error_message(0);
        panic!("halted after 100 potentially-recoverable errors");
    }
    if interaction != InteractionMode::Batch {
        selector = match selector {
            Selector::TERM_ONLY => Selector::NO_PRINT,
            Selector::TERM_AND_LOG => Selector::LOG_ONLY,
            _ => unreachable!(),
        }
    }
    if use_err_help {
        print_ln();
        give_err_help();
    } else {
        while help_ptr > 0 {
            help_ptr -= 1;
            t_print_nl!("{}", help_line[help_ptr as usize]);
        }
    }
    print_ln();
    if interaction != InteractionMode::Batch {
        selector = match selector {
            Selector::NO_PRINT => Selector::TERM_ONLY,
            Selector::LOG_ONLY => Selector::TERM_AND_LOG,
            _ => unreachable!(),
        }
    }
    print_ln();
}
pub(crate) unsafe fn fatal_error(s: &str) -> ! {
    pre_error_message();
    t_eprint!("Emergency stop");
    t_print_nl!("{}", s);
    close_files_and_terminate();
    tt_cleanup();
    rust_stdout.as_mut().unwrap().flush().unwrap();
    abort!("{}", s);
}
pub(crate) unsafe fn overflow(s: &str, n: usize) -> ! {
    pre_error_message();
    t_eprint!("TeX capacity exceeded, sorry [{}={}]", s, n as i32);
    help!(
        "If you really absolutely need more capacity,",
        "you can ask a wizard to enlarge me."
    );
    post_error_message(1);
    panic!("halted on overflow()");
}
pub(crate) unsafe fn confusion(s: &str) -> ! {
    pre_error_message();
    if (history as u32) < (TTHistory::ERROR_ISSUED as u32) {
        t_eprint!("This can\'t happen ({})", s);
        help!("I\'m broken. Please show this to someone who can fix can fix");
    } else {
        t_eprint!("I can\'t go on meeting you like this");
        help!(
            "One of your faux pas seems to have wounded me deeply...",
            "in fact, I\'m barely conscious. Please fix it and try again."
        );
    }
    post_error_message(1);
    panic!("halted on confusion()");
}
/* xetex-errors */
pub(crate) unsafe fn pdf_error(t: &str, p: &str) -> ! {
    pre_error_message();
    t_eprint!("Error");
    if !t.is_empty() {
        t_print!(" ({})", t);
    }
    t_print!(": {}", p);
    post_error_message(1);
    panic!("halted on pdf_error()");
}
