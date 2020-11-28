use crate::xetex_consts::BIGGEST_LANG;
use crate::xetex_errors::overflow;
use crate::xetex_ini::cur_lang;
use crate::xetex_ini::packed_UTF16_code;

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
pub(crate) static mut trie_trl: Vec<trie_pointer> = Vec::new();
pub(crate) static mut trie_tro: Vec<trie_pointer> = Vec::new();
pub(crate) static mut trie_trc: Vec<u16> = Vec::new();

pub(crate) static mut trie_used: [trie_opcode; 256] = [0; 256];
pub(crate) static mut trie_op_lang: [u8; 35112] = [0; 35112];
pub(crate) static mut trie_op_val: [trie_opcode; 35112] = [0; 35112];
pub(crate) static mut trie_op_ptr: i32 = 0;
pub(crate) static mut max_op_used: trie_opcode = 0;
pub(crate) static mut trie_c: Vec<packed_UTF16_code> = Vec::new();
pub(crate) static mut trie_o: Vec<trie_opcode> = Vec::new();
pub(crate) static mut trie_l: Vec<trie_pointer> = Vec::new();
pub(crate) static mut trie_r: Vec<trie_pointer> = Vec::new();
pub(crate) static mut trie_ptr: trie_pointer = 0;
pub(crate) static mut trie_hash: Vec<trie_pointer> = Vec::new();
pub(crate) static mut trie_taken: Vec<bool> = Vec::new();
pub(crate) static mut trie_min: [trie_pointer; 65536] = [0; 65536];
pub(crate) static mut trie_max: trie_pointer = 0;
pub(crate) static mut trie_not_ready: bool = false;

pub(crate) unsafe fn new_trie_op(d: i16, n: i16, v: trie_opcode) -> trie_opcode {
    let mut h = ((n as i32 + 313 * d as i32 + 361 * v as i32 + 1009 * cur_lang as i32).abs() as i64
        % (TRIE_OP_SIZE as i64 - NEG_TRIE_OP_SIZE as i64)
        + NEG_TRIE_OP_SIZE as i64) as i32;
    loop {
        let l = _trie_op_hash_array[(h as i64 - -35111) as usize];
        if l == 0 {
            if trie_op_ptr as i64 == 35111 {
                overflow("pattern memory ops", 35111);
            }
            let mut u = trie_used[cur_lang as usize];
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
pub(crate) unsafe fn trie_node(p: trie_pointer) -> trie_pointer {
    let mut h = ((trie_c[p as usize] as u32
        + 1009 * trie_o[p as usize] as u32
        + 2718 * trie_l[p as usize] as u32
        + 3142 * trie_r[p as usize] as u32)
        % trie_size as u32) as i32;
    loop {
        let q = trie_hash[h as usize];
        if q == 0 {
            trie_hash[h as usize] = p;
            return p;
        }
        if trie_c[q as usize] as i32 == trie_c[p as usize] as i32
            && trie_o[q as usize] as i32 == trie_o[p as usize] as i32
            && trie_l[q as usize] == trie_l[p as usize]
            && trie_r[q as usize] == trie_r[p as usize]
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
pub(crate) unsafe fn compress_trie(p: trie_pointer) -> trie_pointer {
    if p == 0 {
        0
    } else {
        trie_l[p as usize] = compress_trie(trie_l[p as usize]);
        trie_r[p as usize] = compress_trie(trie_r[p as usize]);
        trie_node(p)
    }
}
pub(crate) unsafe fn first_fit(p: trie_pointer) {
    let mut h;
    let c = trie_c[p as usize];
    let mut z = trie_min[c as usize];
    's_31: loop {
        h = z - c as i32;
        if trie_max < h + max_hyph_char {
            if trie_size <= h + max_hyph_char {
                overflow("pattern memory", trie_size as usize);
            }
            loop {
                trie_max += 1;
                trie_taken[trie_max as usize] = false;
                trie_trl[trie_max as usize] = trie_max + 1;
                trie_tro[trie_max as usize] = trie_max - 1;
                if trie_max == h + max_hyph_char {
                    break;
                }
            }
        }
        if !trie_taken[h as usize] {
            let mut q = trie_r[p as usize];
            loop {
                if q <= 0 {
                    break 's_31;
                }
                if trie_trl[(h + trie_c[q as usize] as i32) as usize] == 0 {
                    break;
                }
                q = trie_r[q as usize];
            }
        }
        /*not_found */
        z = trie_trl[z as usize];
    }
    /*found *//*991: */
    trie_taken[h as usize] = true;
    trie_hash[p as usize] = h;
    let mut q = p;
    loop {
        z = h + trie_c[q as usize] as i32;
        let mut l = trie_tro[z as usize];
        let r = trie_trl[z as usize];
        trie_tro[r as usize] = l;
        trie_trl[l as usize] = r;
        trie_trl[z as usize] = 0;
        if l < max_hyph_char {
            let ll = if z < max_hyph_char { z } else { max_hyph_char };
            loop {
                trie_min[l as usize] = r;
                l += 1;
                if l == ll {
                    break;
                }
            }
        }
        q = trie_r[q as usize];
        if q == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn trie_pack(mut p: trie_pointer) {
    loop {
        let q = trie_l[p as usize];
        if q > 0 && trie_hash[q as usize] == 0 {
            first_fit(q);
            trie_pack(q);
        }
        p = trie_r[p as usize];
        if p == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn trie_fix(mut p: trie_pointer) {
    let z = trie_hash[p as usize];
    loop {
        let q = trie_l[p as usize];
        let c = trie_c[p as usize];
        trie_trl[(z + c as i32) as usize] = trie_hash[q as usize];
        trie_trc[(z + c as i32) as usize] = c;
        trie_tro[(z + c as i32) as usize] = trie_o[p as usize] as trie_pointer;
        if q > 0 {
            trie_fix(q);
        }
        p = trie_r[p as usize];
        if p == 0 {
            break;
        }
    }
}
pub(crate) unsafe fn init_trie() {
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
            let t = hyf_distance[k] as i32;
            hyf_distance[k] = hyf_distance[j as usize];
            hyf_distance[j as usize] = t as i16;
            let t = hyf_num[k] as i32;
            hyf_num[k] = hyf_num[j as usize];
            hyf_num[j as usize] = t as i16;
            let t = hyf_next[k] as i32;
            hyf_next[k] = hyf_next[j as usize];
            hyf_next[j as usize] = t as trie_opcode;
            _trie_op_hash_array[(j as i64 - -35111) as usize] =
                _trie_op_hash_array[(k as i64 - -35111) as usize];
            _trie_op_hash_array[(k as i64 - -35111) as usize] = k;
        }
    }
    for p in 0..=trie_size {
        trie_hash[p as usize] = 0;
    }
    trie_r[0] = compress_trie(trie_r[0]);
    trie_l[0] = compress_trie(trie_l[0]);
    for p in 0..=trie_ptr {
        trie_hash[p as usize] = 0;
    }
    for p in 0..=0xffff {
        trie_min[p as usize] = p + 1;
    }
    trie_trl[0] = 1;
    trie_max = 0;
    if trie_l[0] != 0 {
        first_fit(trie_l[0]);
        trie_pack(trie_l[0]);
    }
    if trie_r[0] != 0 {
        /*1645: */
        if trie_l[0] == 0 {
            for p in 0..256 {
                trie_min[p as usize] = p + 2;
            }
        }
        first_fit(trie_r[0]);
        trie_pack(trie_r[0]);
        hyph_start = trie_hash[trie_r[0] as usize];
    }
    if trie_max == 0i32 {
        for r in 0..=max_hyph_char {
            trie_trl[r as usize] = 0;
            trie_tro[r as usize] = 0;
            trie_trc[r as usize] = 0;
        }
        trie_max = max_hyph_char
    } else {
        if trie_r[0] > 0 {
            trie_fix(trie_r[0]);
        }
        if trie_l[0] > 0 {
            trie_fix(trie_l[0]);
        }
        let mut r = 0;
        loop {
            let s = trie_trl[r as usize];
            trie_trl[r as usize] = 0;
            trie_tro[r as usize] = 0;
            trie_trc[r as usize] = 0;
            r = s;
            if r > trie_max {
                break;
            }
        }
    }
    trie_trc[0] = '?' as i32 as u16;
    trie_not_ready = false;
}
