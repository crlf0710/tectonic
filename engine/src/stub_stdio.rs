// These functions are not exported directly on windows. So we use a shims to call them.
extern "C" {
    #[link_name = "xetex_sprintf"]
    pub fn sprintf(s: *mut libc::c_char, format: *const libc::c_char, ...) -> libc::c_int;
    #[link_name = "xetex_strcasecmp"]
    pub fn strcasecmp(s1: *const libc::c_char, s2: *const libc::c_char) -> libc::c_int;
}
