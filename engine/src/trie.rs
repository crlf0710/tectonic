use crate::xetex_consts::BIGGEST_LANG;
use crate::xetex_errors::overflow;
use crate::xetex_ini::cur_lang;
use crate::xetex_ini::{packed_UTF16_code, UTF16_code};
use std::ptr;

pub(crate) type trie_pointer = i32;
pub(crate) type trie_opcode = u16;

pub(crate) const MIN_TRIE_OP: u16 = 0;
pub(crate) const MAX_TRIE_OP: u16 = 65535;
pub(crate) const TRIE_OP_SIZE: i32 = 35111;
const NEG_TRIE_OP_SIZE: i32 = -35111;

pub(crate) static mut _trie_op_hash_array: [usize; 70223] = [0; 70223];

pub(crate) static mut trie_size: i32 = 0;
pub(crate) static mut hyf_distance: [i16; 35112] = [0; 35112];
pub(crate) static mut hyf_num: [i16; 35112] = [0; 35112];
pub(crate) static mut hyf_next: [trie_opcode; 35112] = [0; 35112];
pub(crate) static mut op_start: [i32; 256] = [0; 256];
pub(crate) static mut hyph_start: trie_pointer = 0;

pub(crate) static mut max_hyph_char: i32 = 0;
pub(crate) static mut trie_trl: *mut trie_pointer = ptr::null_mut();
pub(crate) static mut trie_tro: *mut trie_pointer = ptr::null_mut();
pub(crate) static mut trie_trc: *mut u16 = ptr::null_mut();

pub(crate) static mut trie_used: [trie_opcode; 256] = [0; 256];
pub(crate) static mut trie_op_lang: [u8; 35112] = [0; 35112];
pub(crate) static mut trie_op_val: [trie_opcode; 35112] = [0; 35112];
pub(crate) static mut trie_op_ptr: i32 = 0;
pub(crate) static mut max_op_used: trie_opcode = 0;
pub(crate) static mut trie_c: *mut packed_UTF16_code = ptr::null_mut();
pub(crate) static mut trie_o: *mut trie_opcode = ptr::null_mut();
pub(crate) static mut trie_l: *mut trie_pointer = ptr::null_mut();
pub(crate) static mut trie_r: *mut trie_pointer = ptr::null_mut();
pub(crate) static mut trie_ptr: trie_pointer = 0;
pub(crate) static mut trie_hash: *mut trie_pointer = ptr::null_mut();
pub(crate) static mut trie_taken: *mut bool = ptr::null_mut();
pub(crate) static mut trie_min: [trie_pointer; 65536] = [0; 65536];
pub(crate) static mut trie_max: trie_pointer = 0;
pub(crate) static mut trie_not_ready: bool = false;

pub(crate) unsafe fn new_trie_op(mut d: i16, mut n: i16, mut v: trie_opcode) -> trie_opcode {
    let mut h: i32 = 0;
    let mut u: trie_opcode = 0;
    h = ((n as i32 + 313 * d as i32 + 361 * v as i32 + 1009 * cur_lang as i32).abs() as i64
        % (TRIE_OP_SIZE as i64 - NEG_TRIE_OP_SIZE as i64)
        + NEG_TRIE_OP_SIZE as i64) as i32;
    loop {
        let l = _trie_op_hash_array[(h as i64 - -35111) as usize];
        if l == 0 {
            if trie_op_ptr as i64 == 35111 {
                overflow("pattern memory ops", 35111);
            }
            u = trie_used[cur_lang as usize];
            if u == MAX_TRIE_OP {
                overflow("pattern memory ops per language", 65535 - 0);
            }
            trie_op_ptr += 1;
            u += 1;
            trie_used[cur_lang as usize] = u;
            if u as i32 > max_op_used as i32 {
                max_op_used = u
            }
            hyf_distance[trie_op_ptr as usize] = d;
            hyf_num[trie_op_ptr as usize] = n;
            hyf_next[trie_op_ptr as usize] = v;
            trie_op_lang[trie_op_ptr as usize] = cur_lang;
            _trie_op_hash_array[(h as i64 - -35111) as usize] = trie_op_ptr as usize;
            trie_op_val[trie_op_ptr as usize] = u;
            return u;
        }
        if hyf_distance[l] as i32 == d as i32
            && hyf_num[l] as i32 == n as i32
            && hyf_next[l] as i32 == v as i32
            && trie_op_lang[l] as i32 == cur_lang as i32
        {
            return trie_op_val[l];
        }
        if h > -(TRIE_OP_SIZE as i32) {
            h -= 1
        } else {
            h = TRIE_OP_SIZE as i32
        }
    }
}
pub(crate) unsafe fn trie_node(mut p: trie_pointer) -> trie_pointer {
    let mut h: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    h = ((*trie_c.offset(p as isize) as u32
        + 1009 * *trie_o.offset(p as isize) as u32
        + 2718 * *trie_l.offset(p as isize) as u32
        + 3142 * *trie_r.offset(p as isize) as u32)
        % trie_size as u32) as i32;
    loop {
        q = *trie_hash.offset(h as isize);
        if q == 0 {
            *trie_hash.offset(h as isize) = p;
            return p;
        }
        if *trie_c.offset(q as isize) as i32 == *trie_c.offset(p as isize) as i32
            && *trie_o.offset(q as isize) as i32 == *trie_o.offset(p as isize) as i32
            && *trie_l.offset(q as isize) == *trie_l.offset(p as isize)
            && *trie_r.offset(q as isize) == *trie_r.offset(p as isize)
        {
            return q;
        }
        if h > 0 {
            h -= 1;
        } else {
            h = trie_size
        }
    }
}
pub(crate) unsafe fn compress_trie(mut p: trie_pointer) -> trie_pointer {
    if p == 0 {
        0
    } else {
        *trie_l.offset(p as isize) = compress_trie(*trie_l.offset(p as isize));
        *trie_r.offset(p as isize) = compress_trie(*trie_r.offset(p as isize));
        trie_node(p)
    }
}
pub(crate) unsafe fn first_fit(mut p: trie_pointer) {
    let mut h: trie_pointer = 0;
    let mut z: trie_pointer = 0;
    let mut q: trie_pointer = 0;
    let mut c: UTF16_code = 0;
    let mut l: trie_pointer = 0;
    let mut r: trie_pointer = 0;
    let mut ll: i32 = 0;
    c = *trie_c.offset(p as isize);
    z = trie_min[c as usize];
    's_31: loop {
        h = z - c as i32;
        if trie_max < h + max_hyph_char {
            if trie_size <= h + max_hyph_char {
                overflow("pattern memory", trie_size as usize);
            }
            loop {
                trie_max += 1;
                *trie_taken.offset(trie_max as isize) = false;
                *trie_trl.offset(trie_max as isize) = trie_max + 1;
                *trie_tro.offset(trie_max as isize) = trie_max - 1;
                if trie_max == h + max_hyph_char {
                    break;
                }
            }
        }
        if !*trie_taken.offset(h as isize) {
            q = *trie_r.offset(p as isize);
            loop {
                if !(q > 0) {
                    break 's_31;
                }
                if *trie_trl.offset((h + *trie_c.offset(q as isize) as i32) as isize) == 0i32 {
                    break;
                }
                q = *trie_r.offset(q as isize)
            }
        }
        /*not_found */
        z = *trie_trl.offset(z as isize)
    }
    /*found *//*991: */
    *trie_taken.offset(h as isize) = true;
    *trie_hash.offset(p as isize) = h;
    q = p;
    loop {
        z = h + *trie_c.offset(q as isize) as i32;
        l = *trie_tro.offset(z as isize);
        r = *trie_trl.offset(z as isize);
        *trie_tro.offset(r as isize) = l;
        *trie_trl.offset(l as isize) = r;
        *trie_trl.offset(z as isize) = 0i32;
        if l < max_hyph_char {
            if z < max_hyph_char {
                ll = z
            } else {
                ll = max_hyph_char
            }
            loop {
                trie_min[l as usize] = r;
                l += 1;
                if l == ll {
                    break;
                }
            }
        }
        q = *trie_r.offset(q as isize);
        if q == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn trie_pack(mut p: trie_pointer) {
    let mut q: trie_pointer = 0;
    loop {
        q = *trie_l.offset(p as isize);
        if q > 0 && *trie_hash.offset(q as isize) == 0 {
            first_fit(q);
            trie_pack(q);
        }
        p = *trie_r.offset(p as isize);
        if p == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn trie_fix(mut p: trie_pointer) {
    let mut q: trie_pointer = 0;
    let mut c: UTF16_code = 0;
    let mut z: trie_pointer = 0;
    z = *trie_hash.offset(p as isize);
    loop {
        q = *trie_l.offset(p as isize);
        c = *trie_c.offset(p as isize);
        *trie_trl.offset((z + c as i32) as isize) = *trie_hash.offset(q as isize);
        *trie_trc.offset((z + c as i32) as isize) = c;
        *trie_tro.offset((z + c as i32) as isize) = *trie_o.offset(p as isize) as trie_pointer;
        if q > 0 {
            trie_fix(q);
        }
        p = *trie_r.offset(p as isize);
        if p == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn init_trie() {
    let mut t: i32 = 0;
    let mut s: trie_pointer = 0;
    max_hyph_char += 1;
    op_start[0] = -(MIN_TRIE_OP as i32);
    for j in 1..=BIGGEST_LANG {
        op_start[j as usize] = op_start[(j - 1) as usize] + trie_used[(j - 1) as usize] as i32;
    }
    for j in 1..=trie_op_ptr {
        _trie_op_hash_array[(j as i64 - -35111) as usize] =
            (op_start[trie_op_lang[j as usize] as usize] + trie_op_val[j as usize] as i32) as usize;
    }
    for j in 1..=trie_op_ptr {
        while _trie_op_hash_array[(j as i64 - -35111) as usize] as i32 > j {
            let k = _trie_op_hash_array[(j as i64 - -35111) as usize] as usize;
            t = hyf_distance[k] as i32;
            hyf_distance[k] = hyf_distance[j as usize];
            hyf_distance[j as usize] = t as i16;
            t = hyf_num[k] as i32;
            hyf_num[k] = hyf_num[j as usize];
            hyf_num[j as usize] = t as i16;
            t = hyf_next[k] as i32;
            hyf_next[k] = hyf_next[j as usize];
            hyf_next[j as usize] = t as trie_opcode;
            _trie_op_hash_array[(j as i64 - -35111) as usize] =
                _trie_op_hash_array[(k as i64 - -35111) as usize];
            _trie_op_hash_array[(k as i64 - -35111) as usize] = k;
        }
    }
    for p in 0..=trie_size {
        *trie_hash.offset(p as isize) = 0;
    }
    *trie_r.offset(0) = compress_trie(*trie_r.offset(0));
    *trie_l.offset(0) = compress_trie(*trie_l.offset(0));
    for p in 0..=trie_ptr {
        *trie_hash.offset(p as isize) = 0;
    }
    for p in 0..=0xffff {
        trie_min[p as usize] = p + 1;
    }
    *trie_trl.offset(0) = 1i32;
    trie_max = 0i32;
    if *trie_l.offset(0) != 0i32 {
        first_fit(*trie_l.offset(0));
        trie_pack(*trie_l.offset(0));
    }
    if *trie_r.offset(0) != 0i32 {
        /*1645: */
        if *trie_l.offset(0) == 0i32 {
            for p in 0..256 {
                trie_min[p as usize] = p + 2;
            }
        }
        first_fit(*trie_r.offset(0));
        trie_pack(*trie_r.offset(0));
        hyph_start = *trie_hash.offset(*trie_r.offset(0) as isize)
    }
    if trie_max == 0i32 {
        for r in 0..=max_hyph_char {
            *trie_trl.offset(r as isize) = 0;
            *trie_tro.offset(r as isize) = 0;
            *trie_trc.offset(r as isize) = 0;
        }
        trie_max = max_hyph_char
    } else {
        if *trie_r.offset(0) > 0i32 {
            trie_fix(*trie_r.offset(0));
        }
        if *trie_l.offset(0) > 0i32 {
            trie_fix(*trie_l.offset(0));
        }
        let mut r = 0;
        loop {
            s = *trie_trl.offset(r as isize);
            *trie_trl.offset(r as isize) = 0;
            *trie_tro.offset(r as isize) = 0;
            *trie_trc.offset(r as isize) = 0;
            r = s;
            if r > trie_max {
                break;
            }
        }
    }
    *trie_trc.offset(0) = '?' as i32 as u16;
    trie_not_ready = false;
}
