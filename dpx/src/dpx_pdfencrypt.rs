/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2018 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team.

    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/
#![allow(
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use std::ptr;
use std::slice::from_raw_parts;

use super::dpx_dpxcrypt::ARC4_CONTEXT;
use super::dpx_dpxcrypt::{AES_cbc_encrypt_tectonic, AES_ecb_encrypt, ARC4_set_key, ARC4};
use super::dpx_mem::new;
use super::dpx_pdfdoc::pdf_doc_mut;
use super::dpx_pdffont::get_unique_time_if_given;
use crate::dpx_pdfobj::{pdf_dict, pdf_get_version, pdf_obj, pdf_string, PushObj};
use crate::warn;
use chrono::prelude::*;
use libc::{free, memcpy, memset, srand, strcpy, strlen};
use md5::{Digest, Md5};
use rand::prelude::*;
use sha2::{Sha256, Sha384, Sha512};

/* Encryption support
 *
 * Supported: 40-128 bit RC4, 128 bit AES, 256 bit AES
 *
 * TODO: Convert password to PDFDocEncoding. SASLPrep stringpref for AESV3.
 */
/* PDF-2.0 is not published yet. */
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct pdf_sec {
    pub(crate) key: [u8; 32],
    pub(crate) key_size: i32,
    pub(crate) ID: [u8; 16],
    pub(crate) O: [u8; 48],
    pub(crate) U: [u8; 48],
    pub(crate) OE: [u8; 32],
    pub(crate) UE: [u8; 32],
    pub(crate) V: i32,
    pub(crate) R: i32,
    pub(crate) P: i32,
    pub(crate) setting: PdfSecSetting,
    pub(crate) label: PdfSecLabel,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct PdfSecLabel {
    pub(crate) objnum: u64,
    pub(crate) gennum: u16,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub(crate) struct PdfSecSetting {
    pub(crate) use_aes: i32,
    pub(crate) encrypt_metadata: i32,
}
/* Dummy routine for stringprep - NOT IMPLEMENTED YET
 *
 * Preprocessing of a user-provided password consists first of
 * normalizing its representation by applying the "SASLPrep" profile (RFC 4013)
 * of the "stringprep" algorithm (RFC 3454) to the supplied password using the
 * Normalize and BiDi options.
 */
pub(crate) type Stringprep_profile_flags = i32;
static mut sec_data: pdf_sec = pdf_sec {
    key: [0; 32],
    key_size: 0,
    ID: [0; 16],
    O: [0; 48],
    U: [0; 48],
    OE: [0; 32],
    UE: [0; 32],
    V: 0,
    R: 0,
    P: 0,
    setting: PdfSecSetting {
        use_aes: 0,
        encrypt_metadata: 0,
    },
    label: PdfSecLabel {
        objnum: 0,
        gennum: 0,
    },
};
static padding_bytes: [u8; 32] = [
    0x28, 0xbf, 0x4e, 0x5e, 0x4e, 0x75, 0x8a, 0x41, 0x64, 0, 0x4e, 0x56, 0xff, 0xfa, 0x1, 0x8,
    0x2e, 0x2e, 0, 0xb6, 0xd0, 0x68, 0x3e, 0x80, 0x2f, 0xc, 0xa9, 0xfe, 0x64, 0x53, 0x69, 0x7a,
];
static mut verbose: u8 = 0_u8;

pub(crate) unsafe fn pdf_enc_set_verbose(level: i32) {
    verbose = level as u8; /* For AES IV */
}

unsafe fn pdf_enc_init(use_aes: i32, encrypt_metadata: i32) {
    let p = &mut sec_data;
    let current_time = match get_unique_time_if_given() {
        Some(x) => x,
        None => std::time::SystemTime::now(),
    };
    let seconds_since_epoch = current_time
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap_or_else(|x| x.duration())
        .as_secs();
    // TODO: libc rand is not used in this module
    srand(seconds_since_epoch as _);
    p.setting.use_aes = use_aes;
    p.setting.encrypt_metadata = encrypt_metadata;
}

pub(crate) unsafe fn pdf_enc_compute_id_string(dviname: Option<&[u8]>, pdfname: Option<&[u8]>) {
    let p = &mut sec_data;
    /* FIXME: This should be placed in main() or somewhere. */
    pdf_enc_init(1, 1);

    let timeformat = "%Y%m%d%H%M%S";
    let current_time = match get_unique_time_if_given() {
        Some(x) => DateTime::<Utc>::from(x).format(timeformat),
        None => Local::now().format(timeformat),
    };

    let mut md5 = Md5::new();
    md5.input(&format!("{}", current_time));
    md5.input(&format!(
        "{}-{}, Copyright 2002-2015 by Jin-Hwan Cho, Matthias Franz, and Shunsaku Hirata",
        // TODO: Are these variables?
        "xdvipdfmx",
        "0.1"
    ));
    if let Some(dviname) = dviname {
        md5.input(dviname);
    }
    if let Some(pdfname) = pdfname {
        md5.input(pdfname);
    }
    p.ID = md5.result().into();
}
unsafe fn passwd_padding(src: *const i8, dst: *mut u8) {
    let len = (if 32 < strlen(src) { 32 } else { strlen(src) }) as i32;
    memcpy(
        dst as *mut libc::c_void,
        src as *const libc::c_void,
        len as _,
    );
    memcpy(
        dst.offset(len as isize) as *mut libc::c_void,
        padding_bytes.as_ptr() as *const libc::c_void,
        (32 - len) as _,
    );
}
unsafe fn compute_owner_password(p: &mut pdf_sec, opasswd: *const i8, upasswd: *const i8) {
    let mut padded: [u8; 32] = [0; 32];
    let mut arc4: ARC4_CONTEXT = ARC4_CONTEXT {
        idx_i: 0,
        idx_j: 0,
        sbox: [0; 256],
    };
    passwd_padding(
        if strlen(opasswd) > 0 {
            opasswd
        } else {
            upasswd
        },
        padded.as_mut_ptr(),
    );
    let mut md5 = Md5::new();
    md5.input(&padded);
    let mut hash = md5.result();
    if p.R >= 3 {
        for _ in 0..50 {
            /*
             * NOTE: We truncate each MD5 hash as in the following step.
             *       Otherwise Adobe Reader won't decrypt the PDF file.
             */
            let mut md5 = Md5::new();
            md5.input(&hash[..p.key_size as usize]);
            hash = md5.result();
        }
    }
    ARC4_set_key(&mut arc4, p.key_size as u32, hash.as_mut_ptr());
    passwd_padding(upasswd, padded.as_mut_ptr());
    let mut tmp1: [u8; 32] = [0; 32];
    let mut tmp2: [u8; 32] = [0; 32];
    let mut key: [u8; 16] = [0; 16];
    ARC4(&mut arc4, 32_u32, padded.as_mut_ptr(), tmp1.as_mut_ptr());
    if p.R >= 3 {
        for i in 1..=19 {
            memcpy(
                tmp2.as_mut_ptr() as *mut libc::c_void,
                tmp1.as_mut_ptr() as *const libc::c_void,
                32,
            );
            for j in 0..p.key_size as usize {
                key[j] = (hash[j] as i32 ^ i) as u8;
            }
            ARC4_set_key(&mut arc4, p.key_size as u32, key.as_mut_ptr());
            ARC4(&mut arc4, 32_u32, tmp2.as_mut_ptr(), tmp1.as_mut_ptr());
        }
    }
    memcpy(
        p.O.as_mut_ptr() as *mut libc::c_void,
        hash.as_mut_ptr() as *const libc::c_void,
        32,
    );
}

unsafe fn compute_encryption_key(p: &mut pdf_sec, passwd: *const i8) {
    let mut padded: [u8; 32] = [0; 32];
    passwd_padding(passwd, padded.as_mut_ptr());
    let mut md5 = Md5::new();
    md5.input(&padded);
    md5.input(&p.O[..32]);
    let mut tmp: [u8; 4] = [0; 4];
    tmp[0] = (p.P as u8 as i32 & 0xff) as u8;
    tmp[1] = ((p.P >> 8) as u8 as i32 & 0xff) as u8;
    tmp[2] = ((p.P >> 16) as u8 as i32 & 0xff) as u8;
    tmp[3] = ((p.P >> 24) as u8 as i32 & 0xff) as u8;
    md5.input(&tmp);
    md5.input(&p.ID);
    let mut hash = md5.result();
    if p.R >= 3 {
        for _ in 0..50 {
            /*
             * NOTE: We truncate each MD5 hash as in the following step.
             *       Otherwise Adobe Reader won't decrypt the PDF file.
             */
            let mut md5 = Md5::new();
            md5.input(&hash.as_slice()[..p.key_size as usize]);
            hash = md5.result();
        }
    }
    memcpy(
        p.key.as_mut_ptr() as *mut libc::c_void,
        hash.as_mut_ptr() as *const libc::c_void,
        p.key_size as _,
    );
}

unsafe fn compute_user_password(p: &mut pdf_sec, uplain: *const i8) {
    let mut arc4: ARC4_CONTEXT = ARC4_CONTEXT {
        idx_i: 0,
        idx_j: 0,
        sbox: [0; 256],
    };
    let mut upasswd: [u8; 32] = [0; 32];
    compute_encryption_key(p, uplain);
    match p.R {
        2 => {
            ARC4_set_key(&mut arc4, p.key_size as u32, p.key.as_mut_ptr());
            ARC4(
                &mut arc4,
                32_u32,
                padding_bytes.as_ptr(),
                upasswd.as_mut_ptr(),
            );
        }
        3 | 4 => {
            let mut tmp1: [u8; 32] = [0; 32];
            let mut tmp2: [u8; 32] = [0; 32];
            let mut md5 = Md5::new();
            md5.input(&padding_bytes);
            md5.input(&p.ID);
            let mut hash = md5.result();
            ARC4_set_key(&mut arc4, p.key_size as u32, p.key.as_mut_ptr());
            ARC4(&mut arc4, 16_u32, hash.as_mut_ptr(), tmp1.as_mut_ptr());
            for i in 1..=19 {
                let mut key: [u8; 16] = [0; 16];
                memcpy(
                    tmp2.as_mut_ptr() as *mut libc::c_void,
                    tmp1.as_mut_ptr() as *const libc::c_void,
                    16,
                );
                for j in 0..p.key_size as usize {
                    key[j] = (p.key[j] as i32 ^ i) as u8;
                }
                ARC4_set_key(&mut arc4, p.key_size as u32, key.as_mut_ptr());
                ARC4(&mut arc4, 16_u32, tmp2.as_mut_ptr(), tmp1.as_mut_ptr());
            }
            memcpy(
                upasswd.as_mut_ptr() as *mut libc::c_void,
                tmp1.as_mut_ptr() as *const libc::c_void,
                32,
            );
        }
        _ => {
            panic!("Invalid revision number.");
        }
    }
    memcpy(
        p.U.as_mut_ptr() as *mut libc::c_void,
        upasswd.as_mut_ptr() as *const libc::c_void,
        32,
    );
}
/* Algorithm 2.B from ISO 32000-1 chapter 7 */
unsafe fn compute_hash_V5(
    passwd: *const i8,
    salt: *const u8,
    user_key: *const u8,
    R: i32,
) -> [u8; 32]
/* revision */
{
    let mut sha = Sha256::new();
    let mut K: [u8; 64] = [0; 64];
    sha.input(from_raw_parts(passwd as *const u8, strlen(passwd)));
    sha.input(from_raw_parts(salt, 8));
    if !user_key.is_null() {
        sha.input(from_raw_parts(user_key, 48));
    }
    let mut hash: [u8; 32] = sha.result().into();
    assert!(R == 5 || R == 6);
    if R == 5 {
        return hash;
    }
    for (K_item, hash_item) in K.iter_mut().zip(hash.iter()) {
        *K_item = *hash_item;
    }
    let mut K_len = 32_usize;
    let mut nround = 1;
    loop
    /* Initial K count as nround 0. */
    {
        let mut K1: [u8; 256] = [0; 256];
        let mut E_mod3: i32 = 0;
        let K1_len = strlen(passwd)
            .wrapping_add(K_len as _)
            .wrapping_add(if !user_key.is_null() { 48 } else { 0 }) as u64;
        assert!(K1_len < 240 as u64);
        memcpy(
            K1.as_mut_ptr() as *mut libc::c_void,
            passwd as *const libc::c_void,
            strlen(passwd),
        );
        memcpy(
            K1.as_mut_ptr().offset(strlen(passwd) as isize) as *mut libc::c_void,
            K.as_mut_ptr() as *const libc::c_void,
            K_len as _,
        );
        if !user_key.is_null() {
            memcpy(
                K1.as_mut_ptr()
                    .offset(strlen(passwd) as isize)
                    .offset(K_len as isize) as *mut libc::c_void,
                user_key as *const libc::c_void,
                48,
            );
        }
        let mut Kr = Vec::<u8>::with_capacity(K1_len as usize * 64);
        for _ in 0..64 {
            Kr.extend(&K1[..K1_len as usize]);
        }
        let E = AES_cbc_encrypt_tectonic(&K[..16], K.as_ptr().offset(16), 0, &Kr);
        for i in 0..16 {
            E_mod3 += E[i as usize] as i32;
        }
        E_mod3 %= 3;
        match E_mod3 {
            0 => {
                let mut sha_0 = Sha256::new();
                sha_0.input(&E);
                for (K_item, result_item) in K.iter_mut().zip(sha_0.result()) {
                    *K_item = result_item;
                }
                K_len = 32;
            }
            1 => {
                let mut sha_1 = Sha384::new();
                sha_1.input(&E);
                for (K_item, result_item) in K.iter_mut().zip(sha_1.result()) {
                    *K_item = result_item;
                }
                K_len = 48;
            }
            2 => {
                let mut sha_2 = Sha512::new();
                sha_2.input(&E);
                for (K_item, result_item) in K.iter_mut().zip(sha_2.result()) {
                    *K_item = result_item;
                }
                K_len = 64;
            }
            _ => {}
        }
        let c = E[E.len() - 1] as i32;
        if nround >= 64 && c <= nround - 32 {
            break;
        }
        nround += 1
    }

    for (hash_item, K_item) in hash.iter_mut().zip(K.iter()) {
        *hash_item = *K_item;
    }
    hash
}
unsafe fn compute_owner_password_V5(p: &mut pdf_sec, oplain: *const i8) {
    let mut vsalt: [u8; 8] = random();
    let mut ksalt: [u8; 8] = random();
    let hash = compute_hash_V5(oplain, vsalt.as_mut_ptr(), p.U.as_mut_ptr(), p.R);
    p.O[..32].copy_from_slice(&hash);
    p.O[32..40].copy_from_slice(&vsalt);
    p.O[40..].copy_from_slice(&ksalt);
    let hash = compute_hash_V5(oplain, ksalt.as_mut_ptr(), p.U.as_mut_ptr(), p.R);
    let mut iv = [0_u8; 16];
    let OE = AES_cbc_encrypt_tectonic(&hash, iv.as_mut_ptr(), 0, &p.key[..p.key_size as usize]);
    p.OE.copy_from_slice(&OE[..32]);
}
unsafe fn compute_user_password_V5(p: &mut pdf_sec, uplain: *const i8) {
    let mut vsalt: [u8; 8] = random();
    let mut ksalt: [u8; 8] = random();
    let hash = compute_hash_V5(uplain, vsalt.as_mut_ptr(), ptr::null(), p.R);
    p.U[..32].copy_from_slice(&hash);
    p.U[32..40].copy_from_slice(&vsalt);
    p.U[40..].copy_from_slice(&ksalt);
    let hash = compute_hash_V5(uplain, ksalt.as_mut_ptr(), ptr::null(), p.R);
    let mut iv = [0_u8; 16];
    let UE = AES_cbc_encrypt_tectonic(&hash, iv.as_mut_ptr(), 0, &p.key[..p.key_size as usize]);
    p.UE.copy_from_slice(&UE[..32]);
}
unsafe fn check_version(p: &mut pdf_sec, version: i32) {
    if p.V > 2 && version < 4 {
        warn!("Current encryption setting requires PDF version >= 1.4.");
        p.V = 1;
        p.key_size = 5
    } else if p.V == 4 && version < 5 {
        warn!("Current encryption setting requires PDF version >= 1.5.");
        p.V = 2
    } else if p.V == 5 && version < 7 {
        warn!("Current encryption setting requires PDF version >= 1.7 (plus Adobe Extension Level 3).");
        p.V = 4
    };
}
unsafe fn stringprep_profile(
    input: *const i8,
    output: *mut *mut i8,
    mut _profile: *const i8,
    mut _flags: Stringprep_profile_flags,
) -> Result<(), std::str::Utf8Error> {
    let len = strlen(input);
    let _ = std::str::from_utf8(std::slice::from_raw_parts(input as *const u8, len as _))?;
    *output = new((len.wrapping_add(1)).wrapping_mul(::std::mem::size_of::<i8>()) as _) as *mut i8;
    strcpy(*output, input);
    Ok(())
}
unsafe fn preproc_password(passwd: *const i8, outbuf: *mut i8, V: i32) -> i32 {
    let mut error: i32 = 0;
    memset(outbuf as *mut libc::c_void, 0, 128);
    match V {
        1 | 2 | 3 | 4 => {
            /* Need to be converted to PDFDocEncoding - UNIMPLEMENTED */
            for i in 0..strlen(passwd) {
                if (*passwd.offset(i as isize) as i32) < 0x20
                    || *passwd.offset(i as isize) as i32 > 0x7e
                {
                    warn!("Non-ASCII-printable character found in password.");
                }
            }
            memcpy(
                outbuf as *mut libc::c_void,
                passwd as *const libc::c_void,
                if 127 < strlen(passwd) {
                    127
                } else {
                    strlen(passwd)
                },
            );
        }
        5 => {
            /* This is a dummy routine - not actually stringprep password... */
            let mut saslpwd: *mut i8 = ptr::null_mut();
            if stringprep_profile(
                passwd,
                &mut saslpwd,
                b"SASLprep\x00" as *const u8 as *const i8,
                0,
            )
            .is_err()
            {
                return -1;
            } else {
                if !saslpwd.is_null() {
                    memcpy(
                        outbuf as *mut libc::c_void,
                        saslpwd as *const libc::c_void,
                        if 127 < strlen(saslpwd) {
                            127
                        } else {
                            strlen(saslpwd)
                        },
                    );
                    free(saslpwd as *mut libc::c_void);
                }
            }
        }
        _ => error = -1,
    }
    error
}

pub(crate) unsafe fn pdf_enc_set_passwd(
    bits: u32,
    perm: u32,
    oplain: *const i8,
    uplain: *const i8,
) {
    let p = &mut sec_data;
    assert!(!oplain.is_null());
    assert!(!uplain.is_null());
    let version = pdf_get_version();
    p.key_size = bits.wrapping_div(8_u32) as i32;
    if p.key_size == 5 {
        /* 40bit */
        p.V = 1
    } else if p.key_size <= 16 {
        p.V = if p.setting.use_aes != 0 { 4 } else { 2 }
    } else if p.key_size == 32 {
        p.V = 5
    } else {
        warn!("Key length {} unsupported.", bits);
        p.key_size = 5;
        p.V = 2
    }
    check_version(p, version as i32);
    p.P = (perm | 0xc0u32) as i32;
    match p.V {
        1 => p.R = if (p.P as i64) < 0x100 { 2 } else { 3 },
        2 | 3 => p.R = 3,
        4 => p.R = 4,
        5 => p.R = 6,
        _ => p.R = 3,
    }
    /* Password must be preprocessed. */
    let mut opasswd: [i8; 128] = [0; 128];
    let mut upasswd: [i8; 128] = [0; 128];
    if preproc_password(oplain, opasswd.as_mut_ptr(), p.V) < 0 {
        warn!("Invaid UTF-8 string for password.");
    }
    if preproc_password(uplain, upasswd.as_mut_ptr(), p.V) < 0 {
        warn!("Invalid UTF-8 string for passowrd.");
    }
    if p.R >= 3 {
        p.P = (p.P as u32 | 0xfffff000u32) as i32
    }
    if p.V < 5 {
        compute_owner_password(p, opasswd.as_mut_ptr(), upasswd.as_mut_ptr());
        compute_user_password(p, upasswd.as_mut_ptr());
    } else if p.V == 5 {
        p.key = random();
        p.key_size = 32;
        /* uses p->U */
        compute_user_password_V5(p, upasswd.as_mut_ptr());
        compute_owner_password_V5(p, opasswd.as_mut_ptr());
    };
}

unsafe fn calculate_key(p: &mut pdf_sec) -> [u8; 16] {
    let mut len = p.key_size as usize + 5;
    let mut tmp = [0u8; 25];
    memcpy(
        tmp.as_mut_ptr() as *mut libc::c_void,
        p.key.as_mut_ptr() as *const libc::c_void,
        p.key_size as _,
    );
    tmp[p.key_size as usize] = (p.label.objnum as u8 as i32 & 0xff) as u8;
    tmp[(p.key_size + 1) as usize] = ((p.label.objnum >> 8) as u8 as i32 & 0xff) as u8;
    tmp[(p.key_size + 2) as usize] = ((p.label.objnum >> 16) as u8 as i32 & 0xff) as u8;
    tmp[(p.key_size + 3) as usize] = (p.label.gennum as u8 as i32 & 0xff) as u8;
    tmp[(p.key_size + 4) as usize] = ((p.label.gennum as i32 >> 8) as u8 as i32 & 0xff) as u8;
    if p.V >= 4 {
        tmp[(p.key_size + 5) as usize] = 0x73_u8;
        tmp[(p.key_size + 6) as usize] = 0x41_u8;
        tmp[(p.key_size + 7) as usize] = 0x6c_u8;
        tmp[(p.key_size + 8) as usize] = 0x54_u8;
        len += 4;
    }
    let mut md5 = Md5::new();
    md5.input(&tmp[..len]);
    md5.result().into()
}

pub(crate) unsafe fn pdf_encrypt_data(plain: &[u8]) -> Vec<u8> {
    let p = &mut sec_data;
    match p.V {
        1 | 2 => {
            let mut key = calculate_key(p);
            let mut arc4: ARC4_CONTEXT = ARC4_CONTEXT {
                idx_i: 0,
                idx_j: 0,
                sbox: [0; 256],
            };
            let mut cipher = vec![0_u8; plain.len()];
            ARC4_set_key(
                &mut arc4,
                (if 16 < p.key_size + 5 {
                    16
                } else {
                    p.key_size + 5
                }) as u32,
                key.as_mut_ptr(),
            );
            ARC4(
                &mut arc4,
                plain.len() as u32,
                plain.as_ptr(),
                cipher.as_mut_ptr(),
            );
            cipher
        }
        4 => {
            let key = calculate_key(p);
            AES_cbc_encrypt_tectonic(
                &key[..(if 16 < p.key_size + 5 {
                    16
                } else {
                    p.key_size + 5
                }) as usize],
                ptr::null(),
                1,
                plain,
            )
        }
        5 => AES_cbc_encrypt_tectonic(&p.key[..p.key_size as usize], ptr::null(), 1, plain),
        _ => {
            panic!("pdfencrypt: Unexpected V value: {}", p.V);
        }
    }
}

pub(crate) unsafe fn pdf_encrypt_obj() -> pdf_dict {
    let p = &mut sec_data;
    let mut doc_encrypt = pdf_dict::new();
    doc_encrypt.set("Filter", "Standard");
    doc_encrypt.set("V", p.V as f64);
    doc_encrypt.set("Length", (p.key_size * 8) as f64);
    if p.V >= 4 {
        let mut CF = pdf_dict::new();
        let mut StdCF = pdf_dict::new();
        StdCF.set("CFM", if p.V == 4 { "AESV2" } else { "AESV3" });
        StdCF.set("AuthEvent", "DocOpen");
        StdCF.set("Length", p.key_size as f64);
        CF.set("StdCF", StdCF);
        doc_encrypt.set("CF", CF);
        doc_encrypt.set("StmF", "StdCF");
        doc_encrypt.set("StrF", "StdCF");
    }
    doc_encrypt.set("R", p.R as f64);
    if p.V < 5 {
        doc_encrypt.set("O", pdf_string::new(&p.O[..32]));
        doc_encrypt.set("U", pdf_string::new(&p.U[..32]));
    } else if p.V == 5 {
        doc_encrypt.set("O", pdf_string::new(p.O.as_ref()));
        doc_encrypt.set("U", pdf_string::new(p.U.as_ref()));
    }
    doc_encrypt.set("P", p.P as f64);
    if p.V == 5 {
        let mut perms: [u8; 16] = [0; 16];
        doc_encrypt.set("OE", pdf_string::new(p.OE.as_ref()));
        doc_encrypt.set("UE", pdf_string::new(p.UE.as_ref()));
        perms[0] = (p.P & 0xff) as u8;
        perms[1] = (p.P >> 8 & 0xff) as u8;
        perms[2] = (p.P >> 16 & 0xff) as u8;
        perms[3] = (p.P >> 24 & 0xff) as u8;
        perms[4] = 0xff_u8;
        perms[5] = 0xff_u8;
        perms[6] = 0xff_u8;
        perms[7] = 0xff_u8;
        perms[8] = if p.setting.encrypt_metadata != 0 {
            b'T'
        } else {
            b'F'
        };
        perms[9] = b'a';
        perms[10] = b'd';
        perms[11] = b'b';
        perms[12] = 0_u8;
        perms[13] = 0_u8;
        perms[14] = 0_u8;
        perms[15] = 0_u8;
        let cipher = AES_ecb_encrypt(&p.key[..p.key_size as usize], &perms);
        doc_encrypt.set("Perms", pdf_string::new(&cipher));
    }
    if p.R > 5 {
        let catalog = pdf_doc_mut().get_dictionary("Catalog");
        let mut ext = pdf_dict::new();
        let mut adbe = pdf_dict::new();
        adbe.set("BaseVersion", "1.7");
        adbe.set("ExtensionLevel", (if p.R == 5 { 3 } else { 8 }) as f64);
        ext.set("ADBE", adbe);
        (*catalog).as_dict_mut().set("Extensions", ext);
    }
    doc_encrypt
}

pub(crate) unsafe fn pdf_enc_id_array() -> Vec<*mut pdf_obj> {
    let p = &mut sec_data;
    let mut id = vec![];
    id.push_obj(pdf_string::new(p.ID.as_ref()));
    id.push_obj(pdf_string::new(p.ID.as_ref()));
    id
}

pub(crate) unsafe fn pdf_enc_set_label(label: u32) {
    let p = &mut sec_data;
    p.label.objnum = label as u64;
}

pub(crate) unsafe fn pdf_enc_set_generation(generation: u32) {
    let p = &mut sec_data;
    p.label.gennum = generation as u16;
}
/* Order is important here */
