// These functions are not exported directly on windows. So we use a shims to call them.
extern "C" {
    #[link_name = "dpx_sprintf"]
    pub(crate) fn sprintf(s: *mut libc::c_char, format: *const libc::c_char, ...) -> libc::c_int;
}
