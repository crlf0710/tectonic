// These functions are not exported directly on windows. So we use a shims to call them.
extern "C" {
    #[link_name = "dpx_snprintf"]
    pub fn snprintf(
        s: *mut libc::c_char,
        n: libc::size_t,
        format: *const libc::c_char,
        ...
    ) -> libc::c_int;
    #[link_name = "dpx_sprintf"]
    pub fn sprintf(s: *mut libc::c_char, format: *const libc::c_char, ...) -> libc::c_int;
    #[link_name = "dpx_sscanf"]
    pub fn sscanf(s: *const libc::c_char, format: *const libc::c_char, ...) -> libc::c_int;
}
