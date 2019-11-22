#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error};
use crate::xetex_ini::{
    best_height_plus_depth, cur_list, cur_mark, cur_ptr, dead_cycles, disc_ptr, eqtb,
    file_line_error_style_p, help_line, help_ptr, insert_penalties, last_glue, last_kern,
    last_node_type, last_penalty, line, mem, nest, nest_ptr, output_active, page_contents,
    page_so_far, page_tail, sa_root, semantic_pagination_enabled, temp_ptr,
};
use crate::xetex_output::{print_cstr, print_esc_cstr, print_file_line, print_int, print_nl_cstr};
use crate::xetex_scaledmath::x_over_n;
use crate::xetex_shipout::ship_out;
use crate::xetex_xetex0::{
    badness, begin_token_list, box_error, delete_glue_ref, delete_token_ref, do_marks,
    find_sa_element, flush_node_list, free_node, geq_word_define, get_node, new_null_box,
    new_save_level, new_skip_param, new_spec, normal_paragraph, prune_page_top, push_nest,
    scan_left_brace, vert_break, vpackage, BOX_depth, BOX_height, BOX_width, GLUE_NODE_glue_ptr,
    GLUE_SPEC_shrink, GLUE_SPEC_shrink_order, GLUE_SPEC_stretch, GLUE_SPEC_stretch_order,
    LLIST_link, NODE_subtype, NODE_type, PENALTY_NODE_penalty,
};
use crate::xetex_xetexd::is_non_discardable_node;

pub type scaled_t = i32;
pub type eight_bits = u8;
pub type small_number = i16;
/* tectonic/xetex-pagebuilder.c: the page builder
   Copyright 2017-2018 The Tectonic Project
   Licensed under the MIT License.
*/
/* Customizations for Tectonic:
 *
 * In semantic pagination mode, we don't run the pagebuilder routine. We just
 * directly invoke the shipout code, which in turn writes out the output vlist
 * without worrying about pages. We also behave as if holding_inserts is
 * always true: inserts are kept in the page vlist rather than being
 * processed.
 */
static mut best_page_break: i32 = 0;
static mut best_size: scaled_t = 0;
static mut least_page_cost: i32 = 0;
static mut page_max_depth: scaled_t = 0;
/* XXX other variables belong here but pop up all over the code */

#[no_mangle]
pub unsafe extern "C" fn initialize_pagebuilder_variables() {
    page_max_depth = 0;
}

unsafe extern "C" fn freeze_page_specs(mut s: small_number) {
    page_contents = s as _;
    page_so_far[0] = DIMENPAR(DIMEN_PAR__vsize);
    page_max_depth = DIMENPAR(DIMEN_PAR__max_depth);
    page_so_far[7] = 0;
    page_so_far[1] = 0;
    page_so_far[2] = 0;
    page_so_far[3] = 0;
    page_so_far[4] = 0;
    page_so_far[5] = 0;
    page_so_far[6] = 0;
    least_page_cost = MAX_HALFWORD;
}

unsafe extern "C" fn ensure_vbox(mut n: eight_bits) {
    let p = BOX_REG(n as _);
    if p == TEX_NULL {
        return;
    }
    if *NODE_type(p as isize) != HLIST_NODE {
        return;
    }
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr(b"! \x00" as *const u8 as *const i8);
    }
    print_cstr(b"Insertions can only be added to a vbox\x00" as *const u8 as *const i8);
    help_ptr = 3;
    help_line[2] = b"Tut tut: You\'re trying to \\insert into a\x00" as *const u8 as *const i8;
    help_line[1] = b"\\box register that now contains an \\hbox.\x00" as *const u8 as *const i8;
    help_line[0] =
        b"Proceed, and I\'ll discard its present contents.\x00" as *const u8 as *const i8;
    box_error(n);
}

/*1047: "The fire_up subroutine prepares to output the curent page at the best
 * place; then it fires up the user's output routine, if there is one, or it
 * simple ships out the page. There is one parameter, `c`, which represents
 * the node that was being contributed to the page when the decision to force
 * an output was made." */
unsafe extern "C" fn fire_up(mut c: i32) {
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut prev_p: i32 = 0;
    let mut n: u8 = 0;
    let mut wait: bool = false;
    let mut save_vbadness: i32 = 0;
    let mut save_vfuzz: scaled_t = 0;
    let mut save_split_top_skip: i32 = 0;
    /*1048: "Set the value of output_penalty" */
    if *NODE_type(best_page_break as isize) == PENALTY_NODE {
        geq_word_define(
            INT_BASE + INT_PAR__output_penalty,
            *PENALTY_NODE_penalty(best_page_break as isize),
        );
        *PENALTY_NODE_penalty(best_page_break as isize) = INF_PENALTY;
    } else {
        geq_word_define(INT_BASE + INT_PAR__output_penalty, INF_PENALTY);
    }

    /* ... resuming 1047 ... "We set the values of top_mark, first_mark, and
     * bot_mark. The program uses the fact that `bot_mark != null` implies
     * `first_mark != null`; it also knows that `bot_mark == null` implies
     * `top_mark = first_mark = null`." The do_marks() call basically does the
     * same thing as the code immediately below it, but for all "mark classes"
     * beyond the default one -- a "mark class" being a concept introduced in
     * e-TeX. */

    if sa_root[MARK_VAL as usize] != TEX_NULL {
        if do_marks(FIRE_UP_INIT as _, 0, sa_root[MARK_VAL as usize]) {
            sa_root[7] = TEX_NULL;
        }
    }
    if cur_mark[BOT_MARK_CODE as usize] != TEX_NULL {
        if cur_mark[TOP_MARK_CODE as usize] != TEX_NULL {
            delete_token_ref(cur_mark[TOP_MARK_CODE as usize]);
        }
        cur_mark[TOP_MARK_CODE as usize] = cur_mark[BOT_MARK_CODE as usize];
        (*mem.offset(cur_mark[0] as isize)).b32.s0 += 1;
        delete_token_ref(cur_mark[FIRST_MARK_CODE as usize]);
        cur_mark[FIRST_MARK_CODE as usize] = TEX_NULL;
    }

    /*1049: "Put the optimal current page into box 255, update first_mark and
     * bot_mark, append insertions to their boxes, and put the remaining nodes
     * back on the contribution list." */

    if c == best_page_break {
        best_page_break = TEX_NULL; /* "c not yet linked in" */
    }
    if BOX_REG(255) != TEX_NULL {
        /*1050:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! \x00" as *const u8 as *const i8);
        }
        print_cstr(b"\x00" as *const u8 as *const i8);
        print_esc_cstr(b"box\x00" as *const u8 as *const i8);
        print_cstr(b"255 is not void\x00" as *const u8 as *const i8);
        help_ptr = 2;
        help_line[1] = b"You shouldn\'t use \\box255 except in \\output routines.\x00" as *const u8
            as *const i8;
        help_line[0] =
            b"Proceed, and I\'ll discard its present contents.\x00" as *const u8 as *const i8;
        box_error(255);
    }
    insert_penalties = 0; /* "this will count the number of insertions held over" */
    save_split_top_skip = *GLUEPAR(GLUE_PAR__split_top_skip);

    /* Tectonic: in semantic pagination mode, we act as if holding_inserts is
     * always active. */

    let process_inserts = INTPAR(INT_PAR__holding_inserts) <= 0 && !semantic_pagination_enabled;

    if process_inserts {
        /*1053: "Prepare all the boxes involved in insertions to act as
         * queues". Namely: for each insert being tracked, set the
         * `last_ins_ptr` field of its data structure to the last node in its
         * associated vlist. If holding_inserts is positive, the inserts are
         * just kept in the page vlist without any processing, I believe with
         * the expectation that the output routine will do something clever
         * with them. */
        r = *LLIST_link(PAGE_INS_HEAD as isize);
        while r != PAGE_INS_HEAD {
            if (*mem.offset((r + 2i32) as isize)).b32.s0 != TEX_NULL {
                n = *NODE_subtype(r as _) as _;
                ensure_vbox(n);

                if BOX_REG(n as _) == TEX_NULL {
                    BOX_REG_set(n as _, new_null_box());
                }

                p = BOX_REG(n as _) + 5; /* 5 = list_offset, "position of the list inside the box" */
                while *LLIST_link(p as isize) != TEX_NULL {
                    p = *LLIST_link(p as isize);
                }

                (*mem.offset((r + 2i32) as isize)).b32.s1 = p
            }
            r = *LLIST_link(r as isize);
        }
    }
    q = HOLD_HEAD;
    *LLIST_link(q as isize) = TEX_NULL;
    prev_p = PAGE_HEAD;
    p = *LLIST_link(prev_p as isize);

    while p != best_page_break {
        if *NODE_type(p as isize) == INS_NODE {
            if process_inserts {
                /*1055: "Either insert the material specified by node p into
                 * the appropriate box, or hold it for the next page; also
                 * delete node p from the current page." */
                r = *LLIST_link(PAGE_INS_HEAD as isize);

                while *NODE_subtype(r as isize) != *NODE_subtype(p as isize) {
                    r = *LLIST_link(r as isize);
                }
                if (*mem.offset((r + 2i32) as isize)).b32.s0 == TEX_NULL {
                    wait = true
                } else {
                    wait = false;

                    s = (*mem.offset((r + 2i32) as isize)).b32.s1;
                    *LLIST_link(s as isize) = (*mem.offset((p + 4i32) as isize)).b32.s0;
                    if (*mem.offset((r + 2i32) as isize)).b32.s0 == p {
                        /*1056: "Wrap up the box specified by node r,
                         * splitting node p if called for; set wait = true if
                         * node p holds a remainder after splitting" */
                        if *NODE_type(r as isize) == SPLIT_UP as _ {
                            if (*mem.offset((r + 1i32) as isize)).b32.s0 == p
                                && (*mem.offset((r + 1i32) as isize)).b32.s1 != -0xfffffffi32
                            {
                                while *LLIST_link(s as isize)
                                    != (*mem.offset((r + 1i32) as isize)).b32.s1
                                {
                                    s = *LLIST_link(s as isize);
                                }
                                *LLIST_link(s as isize) = TEX_NULL;
                                *GLUEPAR(GLUE_PAR__split_top_skip) =
                                    (*mem.offset((p + 4i32) as isize)).b32.s1;
                                (*mem.offset((p + 4i32) as isize)).b32.s0 = prune_page_top(
                                    (*mem.offset((r + 1i32) as isize)).b32.s1,
                                    false,
                                );
                                if (*mem.offset((p + 4i32) as isize)).b32.s0 != TEX_NULL {
                                    temp_ptr = vpackage(
                                        (*mem.offset((p + 4i32) as isize)).b32.s0,
                                        0,
                                        ADDITIONAL as _,
                                        MAX_HALFWORD,
                                    );
                                    (*mem.offset((p + 3i32) as isize)).b32.s1 =
                                        (*mem.offset((temp_ptr + 3i32) as isize)).b32.s1
                                            + (*mem.offset((temp_ptr + 2i32) as isize)).b32.s1;
                                    free_node(temp_ptr, BOX_NODE_SIZE);
                                    wait = true
                                }
                            }
                        }
                        (*mem.offset((r + 2i32) as isize)).b32.s0 = TEX_NULL;
                        n = *NODE_subtype(r as isize) as _;
                        temp_ptr = (*mem.offset(
                            ((*eqtb.offset(
                                (1i32
                                    + (0x10ffffi32 + 1i32)
                                    + (0x10ffffi32 + 1i32)
                                    + 1i32
                                    + 15000i32
                                    + 12i32
                                    + 9000i32
                                    + 1i32
                                    + 1i32
                                    + 19i32
                                    + 256i32
                                    + 256i32
                                    + 13i32
                                    + 256i32
                                    + 4i32
                                    + n as i32) as isize,
                            ))
                            .b32
                            .s1 + 5i32) as isize,
                        ))
                        .b32
                        .s1;
                        free_node(BOX_REG(n as _), BOX_NODE_SIZE);
                        BOX_REG_set(
                            n as _,
                            vpackage(temp_ptr, 0i32, 1i32 as small_number, 0x3fffffffi32),
                        );
                    } else {
                        while *LLIST_link(s as isize) != TEX_NULL {
                            s = *LLIST_link(s as isize);
                        }
                        (*mem.offset((r + 2i32) as isize)).b32.s1 = s
                    }
                }

                /*1057: "Either append the insertion node p after node q, and
                 * remove it from the current page, or delete node(p)" */

                *LLIST_link(prev_p as isize) = *LLIST_link(p as isize);
                *LLIST_link(p as isize) = TEX_NULL;

                if wait {
                    *LLIST_link(q as isize) = p;
                    q = p;
                    insert_penalties += 1
                } else {
                    delete_glue_ref((*mem.offset((p + 4i32) as isize)).b32.s1);
                    free_node(p, INS_NODE_SIZE);
                }
                p = prev_p /*:1057 */
            }
        } else if *NODE_type(p as isize) == MARK_NODE {
            if (*mem.offset((p + 1i32) as isize)).b32.s0 != 0 {
                /*1618: "Update the current marks" */
                find_sa_element(
                    MARK_VAL as _,
                    (*mem.offset((p + 1i32) as isize)).b32.s0,
                    true,
                );
                if (*mem.offset((cur_ptr + 1i32) as isize)).b32.s1 == TEX_NULL {
                    (*mem.offset((cur_ptr + 1i32) as isize)).b32.s1 =
                        (*mem.offset((p + 1i32) as isize)).b32.s1;
                    let ref mut fresh1 = (*mem
                        .offset((*mem.offset((p + 1i32) as isize)).b32.s1 as isize))
                    .b32
                    .s0;
                    *fresh1 += 1
                }
                if (*mem.offset((cur_ptr + 2i32) as isize)).b32.s0 != TEX_NULL {
                    delete_token_ref((*mem.offset((cur_ptr + 2i32) as isize)).b32.s0);
                }
                (*mem.offset((cur_ptr + 2i32) as isize)).b32.s0 =
                    (*mem.offset((p + 1i32) as isize)).b32.s1;
                let ref mut fresh2 = (*mem
                    .offset((*mem.offset((p + 1i32) as isize)).b32.s1 as isize))
                .b32
                .s0;
                *fresh2 += 1
            } else {
                /*1051: "Update the values of first_mark and bot_mark" */
                if cur_mark[FIRST_MARK_CODE as usize] == TEX_NULL {
                    cur_mark[FIRST_MARK_CODE as usize] = (*mem.offset((p + 1i32) as isize)).b32.s1;
                    let ref mut fresh3 = (*mem.offset(cur_mark[1] as isize)).b32.s0;
                    *fresh3 += 1
                }
                if cur_mark[2] != TEX_NULL {
                    delete_token_ref(cur_mark[BOT_MARK_CODE as usize]);
                }
                cur_mark[BOT_MARK_CODE as usize] = (*mem.offset((p + 1i32) as isize)).b32.s1;
                let ref mut fresh4 = (*mem.offset(cur_mark[2] as isize)).b32.s0;
                *fresh4 += 1
            }
        }

        prev_p = p;
        p = *LLIST_link(prev_p as isize);
    }
    *GLUEPAR(GLUE_PAR__split_top_skip) = save_split_top_skip;

    /*1052: "Break the current page at node p, put it in box 255, and put the
     * remaining nodes on the contribution list". */
    if p != TEX_NULL {
        if *LLIST_link(CONTRIB_HEAD as isize) == TEX_NULL {
            if nest_ptr == 0 {
                cur_list.tail = page_tail
            } else {
                (*nest.offset(0)).tail = page_tail
            }
        }
        *LLIST_link(page_tail as isize) = *LLIST_link(CONTRIB_HEAD as isize);
        *LLIST_link(CONTRIB_HEAD as isize) = p;
        *LLIST_link(prev_p as isize) = TEX_NULL;
    }

    /* Temporarily futz some variables to inhibit error messages */
    save_vbadness = INTPAR(INT_PAR__vbadness);
    INTPAR_set(INT_PAR__vbadness, INF_BAD);
    save_vfuzz = DIMENPAR(DIMEN_PAR__vfuzz);
    DIMENPAR_set(DIMEN_PAR__vfuzz, MAX_HALFWORD);
    BOX_REG_set(
        255,
        vpackage(
            *LLIST_link(PAGE_HEAD as isize),
            best_size,
            EXACTLY as _,
            page_max_depth,
        ),
    );
    INTPAR_set(INT_PAR__vbadness, save_vbadness);
    DIMENPAR_set(DIMEN_PAR__vfuzz, save_vfuzz);

    if last_glue != MAX_HALFWORD {
        delete_glue_ref(last_glue);
    }

    /*1026: "Start a new current page" */
    page_contents = EMPTY as _;
    page_tail = PAGE_HEAD;
    *LLIST_link(PAGE_HEAD as isize) = TEX_NULL;
    last_glue = MAX_HALFWORD;
    last_penalty = 0;
    last_kern = 0;
    last_node_type = -1;
    page_so_far[7] = 0;
    page_max_depth = 0;

    if q != HOLD_HEAD {
        *LLIST_link(PAGE_HEAD as isize) = *LLIST_link(HOLD_HEAD as isize);
        page_tail = q
    }

    /*1054: "Delete the page-insertion nodes" */
    r = *LLIST_link(PAGE_INS_HEAD as isize);
    while r != PAGE_INS_HEAD {
        q = *LLIST_link(r as isize);
        free_node(r, PAGE_INS_NODE_SIZE);
        r = q
    }

    *LLIST_link(PAGE_INS_HEAD as isize) = PAGE_INS_HEAD;

    /* ... resuming 1047 ... */

    if sa_root[MARK_VAL as usize] != TEX_NULL {
        if do_marks(FIRE_UP_DONE as _, 0, sa_root[MARK_VAL as usize]) {
            sa_root[MARK_VAL as usize] = TEX_NULL;
        }
    }
    if cur_mark[TOP_MARK_CODE as usize] != TEX_NULL
        && cur_mark[FIRST_MARK_CODE as usize] == TEX_NULL
    {
        cur_mark[FIRST_MARK_CODE as usize] = cur_mark[TOP_MARK_CODE as usize];
        (*mem.offset(cur_mark[0] as isize)).b32.s0 += 1;
    }

    /* Tectonic: in semantic pagination mode, ignore the output routine. */

    if LOCAL(LOCAL__output_routine) != TEX_NULL && !semantic_pagination_enabled {
        if dead_cycles >= INTPAR(INT_PAR__max_dead_cycles) {
            /*1059: "Explain that too many dead cycles have happened in a row." */
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! \x00" as *const u8 as *const i8);
            }
            print_cstr(b"Output loop---\x00" as *const u8 as *const i8);
            print_int(dead_cycles);
            print_cstr(b" consecutive dead cycles\x00" as *const u8 as *const i8);
            help_ptr = 3;
            help_line[2] = b"I\'ve concluded that your \\output is awry; it never does a\x00"
                as *const u8 as *const i8;
            help_line[1] = b"\\shipout, so I\'m shipping \\box255 out myself. Next time\x00"
                as *const u8 as *const i8;
            help_line[0] = b"increase \\maxdeadcycles if you want me to be more patient!\x00"
                as *const u8 as *const i8;
            error();
        } else {
            /*1060: "Fire up the user's output routine and return" */
            output_active = true;
            dead_cycles += 1;
            push_nest();
            cur_list.mode = -VMODE as _;
            cur_list.aux.b32.s1 = IGNORE_DEPTH; /* this is `prev_depth` */
            cur_list.mode_line = -line;
            begin_token_list(LOCAL(LOCAL__output_routine), OUTPUT_TEXT);
            new_save_level(OUTPUT_GROUP as _);
            normal_paragraph();
            scan_left_brace();
            return;
        }
    }

    /*1058: "Perform the default output routine." */
    if *LLIST_link(PAGE_HEAD as isize) != TEX_NULL {
        if *LLIST_link(CONTRIB_HEAD as isize) == TEX_NULL {
            if nest_ptr == 0 {
                cur_list.tail = page_tail
            } else {
                (*nest.offset(0)).tail = page_tail
            }
        } else {
            *LLIST_link(page_tail as isize) = *LLIST_link(CONTRIB_HEAD as isize);
        }

        *LLIST_link(CONTRIB_HEAD as isize) = *LLIST_link(PAGE_HEAD as isize);
        *LLIST_link(PAGE_HEAD as isize) = TEX_NULL;
        page_tail = PAGE_HEAD;
    }

    flush_node_list(disc_ptr[LAST_BOX_CODE as usize]);
    disc_ptr[LAST_BOX_CODE as usize] = TEX_NULL;
    ship_out(BOX_REG(255));
    BOX_REG_set(255, TEX_NULL);
}

/*1029: "When TeX has appended new material in vertical mode, it calls the
 * procedure build_page, which tries to catch up by moving nodes from the
 * contribution list to the current page. This procedure will succeed in its
 * goal of emptying the contribution list, unless a page break is discovered,
 * i.e., unless the current page has grown to the point where the optimum next
 * page break has been determined. In the latter case, the nodes after the
 * optimum break will go back onto the contribution list, and control will
 * effectively pass to the user's output routine." ... "TeX is not always in
 * vertical mode at the time build_page is called; the current mode reflects
 * what TeX should return to, after the contribution list has been emptied. A
 * call on build_page should be immediate followed by `goto big_switch`, which
 * is TeX's central control point." */

const AWFUL_BAD: i32 = MAX_HALFWORD; /* XXX redundant with xetex-linebreak.c */

#[no_mangle]
pub unsafe extern "C" fn build_page() {
    let mut current_block: u64;
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut b: i32 = 0;
    let mut c: i32 = 0;
    let mut pi: i32 = 0;
    let mut n: u8 = 0;
    let mut delta: scaled_t = 0;
    let mut h: scaled_t = 0;
    let mut w: scaled_t = 0;

    if *LLIST_link(CONTRIB_HEAD as isize) == TEX_NULL || output_active as i32 != 0 {
        return;
    }
    loop  {
        p = *LLIST_link(CONTRIB_HEAD as isize);

        /*1031: "Update the values of last_glue, last_penalty, and last_kern" */
        if last_glue != MAX_HALFWORD { delete_glue_ref(last_glue); }

        last_penalty = 0;
        last_kern = 0;
        last_node_type = *NODE_type(p as isize) as i32 + 1;

        if *NODE_type(p as isize) == GLUE_NODE {
            last_glue = *GLUE_NODE_glue_ptr(p as isize);
            (*mem.offset(last_glue as isize)).b32.s1 += 1;
        } else {
            last_glue = MAX_HALFWORD;

            if *NODE_type(p as isize) == PENALTY_NODE {
                last_penalty = (*mem.offset((p + 1i32) as isize)).b32.s1
            } else if *NODE_type(p as isize) == KERN_NODE {
                last_kern = *BOX_width(p as isize);
            }
        }
        /*1032: "Move node p to the current page; if it is time for a page
         * break, put the nodes following the break back onto the contribution
         * list, and return to the user's output routine if there is one" */

        /* "The code here is an example of a many-way switch into routines
         * that merge together in different places. Some people call this
         * unstructured programming, but the author doesn't see much wrong
         * with it, as long as the various labels have a well-understood
         * meaning." */

        /* 1035: "If the current page is empty and node p is to be deleted,
         * goto done1; otherwise use node p to update the state of the current
         * page; if this node is an insertion, goto contribute; otherwise if
         * this node is not a legal breakpoint, goto contribute or
         * update_heights; otherwise set `pi` to the penalty associated with
         * this breakpoint." ... "The title of this section is already so
         * long, it seems best to avoid making it more accurate but still
         * longer, by mentioning the fact that a kern node at the end of the
         * contribution list will not be contributed until we know its
         * successor." */

        match *NODE_type(p as isize) {
            HLIST_NODE | VLIST_NODE | RULE_NODE => {
                if page_contents < BOX_THERE as _ {
                    /*1036: "Initialize the current page, insert the \topskip glue
                     * ahead of p, and goto continue." */
                    if page_contents == EMPTY as _{
                        freeze_page_specs(BOX_THERE as _);
                    } else { page_contents = BOX_THERE as _}

                    q = new_skip_param(GLUE_PAR__top_skip as _); /* "now temp_ptr = glue_ptr(q) */

                    if *BOX_width(temp_ptr as isize) > *BOX_height(p as isize) {
                        *BOX_width(temp_ptr as isize) -= *BOX_height(p as isize);
                    } else {
                        *BOX_width(temp_ptr as isize) = 0;
                    }

                    *LLIST_link(q as isize) = p;
                    *LLIST_link(CONTRIB_HEAD as isize) = q;
                    current_block = 15427931788582360902;
                } else {
                    /*1037: "Prepare to move a box or rule node to the current
                 * page, then goto contribute." */
                    page_so_far[1] += page_so_far[7] + *BOX_height(p as isize);
                    page_so_far[7] = *BOX_depth(p as isize);
                    current_block = 11918621130838443904;
                }
            }
            WHATSIT_NODE => {
                /*1401: "Prepare to move whatsit p to the current page, then goto contribute" */
                if *NODE_subtype(p as isize) == PIC_NODE || *NODE_subtype(p as isize) == PDF_NODE {
                    page_so_far[1] += page_so_far[7] + *BOX_height(p as isize);
                    page_so_far[7] = *BOX_depth(p as isize);
                }
                current_block = 11918621130838443904;
            }
            GLUE_NODE => {
                if page_contents < BOX_THERE as _ {
                    current_block = 15559656170992153795;
                } else if is_non_discardable_node(page_tail) {
                    pi = 0;
                    current_block = 13253659531982233645;
                } else { current_block = 5579886686420104461; }
            }
            KERN_NODE => {
                if page_contents  < BOX_THERE as _ {
                    current_block = 15559656170992153795;
                } else if *LLIST_link(p as isize) == TEX_NULL {
                    return
                } else if *NODE_type(*LLIST_link(p as isize) as isize) == GLUE_NODE {
                    pi = 0;
                    current_block = 13253659531982233645;
                } else { current_block = 5579886686420104461; }
            }
            PENALTY_NODE => {
                if page_contents < BOX_THERE as _ {
                    current_block = 15559656170992153795;
                } else {
                    pi = (*mem.offset((p + 1i32) as isize)).b32.s1;
                    current_block = 13253659531982233645;
                }
            }
            MARK_NODE => { current_block = 11918621130838443904; }
            INS_NODE => {
                /*1043: "Append an insertion to the current page and goto contribute" */
                if page_contents == EMPTY as _ {
                    freeze_page_specs(INSERTS_ONLY as _);
                }

                n = *NODE_subtype(p as isize) as _;
                r = PAGE_INS_HEAD;

                while n as u16 >= *NODE_subtype(*LLIST_link(r as isize) as isize) {
                    r = *LLIST_link(r as isize);
                }

                if *NODE_subtype(r as isize) != n as _{
                    /*1044: "Create a page insertion node with subtype(r) = n, and
                 * include the glue correction for box `n` in the current page
                 * state" */
                    q = get_node(PAGE_INS_NODE_SIZE);
                    *LLIST_link(q as isize) = *LLIST_link(r as isize);
                    *LLIST_link(r as isize) = q;
                    r = q;

                    *NODE_subtype(r as isize) = n as _;
                    *NODE_type(r as isize) = INSERTING as _;
                    ensure_vbox(n);

                    if BOX_REG(n as _) == TEX_NULL {
                        *BOX_height(r as isize) = 0;
                    } else {
                        *BOX_height(r as isize) = *BOX_height(BOX_REG(n as _) as isize) + *BOX_depth(BOX_REG(n as _) as isize);
                    }

                    (*mem.offset((r + 2i32) as isize)).b32.s0 = TEX_NULL;
                    q = SKIP_REG(n as _);

                    if COUNT_REG(n as _) == 1000 {
                        h = *BOX_height(r as isize);
                    } else {
                        h = x_over_n(*BOX_height(r as isize), 1000) * COUNT_REG(n as _);
                    }

                    page_so_far[0] -= h + *BOX_width(q as isize);
                    page_so_far[2 + *GLUE_SPEC_stretch_order(q as isize) as usize] += *GLUE_SPEC_stretch(q as isize);
                    page_so_far[6] += *GLUE_SPEC_shrink(q as isize);

                    if *GLUE_SPEC_shrink_order(q as isize) != NORMAL as _ && *GLUE_SPEC_shrink(q as isize) != 0 {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr(b"! \x00" as *const u8 as
                                              *const i8);
                        }
                        print_cstr(b"Infinite glue shrinkage inserted from \x00"
                                       as *const u8 as *const i8);
                        print_esc_cstr(b"skip\x00" as *const u8 as
                                           *const i8);
                        print_int(n as i32);
                        help_ptr = 3;
                        help_line[2] =
                            b"The correction glue for page breaking with insertions\x00"
                                as *const u8 as *const i8;
                        help_line[1] =
                            b"must have finite shrinkability. But you may proceed,\x00"
                                as *const u8 as *const i8;
                        help_line[0] =
                            b"since the offensive shrinkability has been made finite.\x00"
                                as *const u8 as *const i8;
                        error();
                    }
                }

                if *NODE_type(r as isize) == SPLIT_UP as _ {
                    insert_penalties +=
                        (*mem.offset((p + 1i32) as isize)).b32.s1
                } else {
                    (*mem.offset((r + 2i32) as isize)).b32.s1 = p;
                    delta =
                        page_so_far[0] - page_so_far[1] - page_so_far[7] +
                            page_so_far[6];

                    if COUNT_REG(n as _) == 1000 {
                        h = *BOX_height(p as isize);
                    } else {
                        h = x_over_n(*BOX_height(p as isize), 1000) * COUNT_REG(n as _);
                    }

                    if (h <= 0 || h <= delta) &&
                        *BOX_height(p as isize) + *BOX_height(r as isize) <= SCALED_REG(n as _) {
                        page_so_far[0] -= h;
                        *BOX_height(r as isize) += *BOX_height(p as isize);
                    } else {
                        /*1045: "Find the best way to split the insertion, and
                     * change type(r) to split_up." ... "Here is code that
                     * will split a long footnote between pages, in an
                     * emergency ... Node `p` is an insertion into box `n`;
                     * the insertion will not fit, in its entirety, either
                     * because it would make the total contents of box `n`
                     * greater then `\dimen n`, or because it would make the
                     * incremental amount of growth `h` greater than the
                     * available space `delta`, or both. (This amount `h` has
                     * been weighted by the insertion scaling factor, i.e., by
                     * `\count n` over 1000.) Now we will choose the best way
                     * to break the vlist of the insertion, using the same
                     * criteria as in the `\vsplit` operation." */
                        if COUNT_REG(n as _) <= 0 {
                            w = MAX_HALFWORD;
                        } else {
                            w =
                                page_so_far[0] - page_so_far[1] -
                                    page_so_far[7];
                            if COUNT_REG(n as _) != 1000 {
                                w = x_over_n(w, COUNT_REG(n as _))* 1000;
                            }
                        }

                        if w > SCALED_REG(n as _) - *BOX_height(r as isize) {
                            w = SCALED_REG(n as _) - *BOX_height(r as isize);
                        }

                        q =
                            vert_break((*mem.offset((p + 4i32) as
                                                        isize)).b32.s0, w,
                                       (*mem.offset((p + 2i32) as
                                                        isize)).b32.s1);
                        let ref mut fresh9 =
                            (*mem.offset((r + 3i32) as isize)).b32.s1;
                        *fresh9 += best_height_plus_depth;

                        if COUNT_REG(n as _) != 1000 {
                            best_height_plus_depth =
                                x_over_n(best_height_plus_depth, 1000i32) * COUNT_REG(n as _);
                        }
                        page_so_far[0] -= best_height_plus_depth;
                        *NODE_type(r as isize) = SPLIT_UP as _;
                        (*mem.offset((r + 1i32) as isize)).b32.s1 = q;
                        (*mem.offset((r + 1i32) as isize)).b32.s0 = p;

                        if q == TEX_NULL {
                            insert_penalties += EJECT_PENALTY;
                        } else if *NODE_type(q as isize) == PENALTY_NODE {
                            insert_penalties +=
                                (*mem.offset((q + 1i32) as isize)).b32.s1
                        }
                    }
                }
                current_block = 11918621130838443904;
            }
            _ => {
                confusion(b"page\x00" as *const u8 as *const i8);
            }
        }
        match current_block {
            13253659531982233645 =>
            /*1040: "Check if node p is the new champion breakpoint; then if it is
         * time for a page break, prepare for output, and either fire up the
         * user's output routine and return or ship out the page and goto
         * done." We reach this point when p is a glue, kern, or penalty, and
         * there's already content on the page -- so this might be a place to
         * break the page. */
            {
                if pi < INF_PENALTY {
                    /*1042: "Compute the badness b of the current page, using
             * awful_bad if the box is too full." */
                    if page_so_far[1] < page_so_far[0] {
                        if page_so_far[3] != 0 || page_so_far[4] != 0 ||
                               page_so_far[5] != 0 {
                            b = 0;
                        } else {
                            b =
                                badness(page_so_far[0] - page_so_far[1],
                                        page_so_far[2])
                        }
                    } else if page_so_far[1] - page_so_far[0] > page_so_far[6]
                     {
                        b = AWFUL_BAD;
                    } else {
                        b =
                            badness(page_so_far[1] - page_so_far[0],
                                    page_so_far[6])
                    }
                    if b < AWFUL_BAD {
                        if pi <= EJECT_PENALTY {
                            c = pi
                        } else if b < INF_BAD {
                            c = b + pi + insert_penalties
                        } else { c = 100000 }
                        /* DEPLORABLE */
                    } else { c = b }

                    if insert_penalties >= 10000 { c = MAX_HALFWORD }

                    if c <= least_page_cost {
                        best_page_break = p;
                        best_size = page_so_far[0];
                        least_page_cost = c;
                        r = *LLIST_link(PAGE_INS_HEAD as isize);

                        while r != PAGE_INS_HEAD {
                            (*mem.offset((r + 2i32) as isize)).b32.s0 =
                                (*mem.offset((r + 2i32) as isize)).b32.s1;
                            r = *LLIST_link(r as isize);
                        }
                    }
                    if c == AWFUL_BAD || pi <= EJECT_PENALTY {
                        fire_up(p);
                        if output_active {
                            /* "the page has been shipped out by the default output routine" */
                            /* "user's output routine will act" */
                            return
                        }
                        current_block = 15427931788582360902;
                    } else { current_block = 433373112845341403; }
                } else { current_block = 433373112845341403; }
                match current_block {
                    15427931788582360902 => { }
                    _ =>
                    /* ... resuming 1032 ... I believe the "goto" here can only be
         * triggered if p is a penalty node, and we decided not to break. */
                    {
                        if ((*mem.offset(p as isize)).b16.s1 as i32) <
                               10i32 ||
                               (*mem.offset(p as isize)).b16.s1 as i32
                                   > 11i32 {
                            current_block = 11918621130838443904;
                        } else { current_block = 5579886686420104461; }
                    }
                }
            }
            15559656170992153795 => {
                /*1034: "Recycle node p". This codepath is triggered if we encountered
         * something nonprinting (glue, kern, penalty) and there aren't any
         * yes-printing boxes at the top of the page yet. When that happens,
         * we just discard the nonprinting node. */
                (*mem.offset((4999999i32 - 1i32) as isize)).b32.s1 =
                    (*mem.offset(p as isize)).b32.s1;
                (*mem.offset(p as isize)).b32.s1 = -0xfffffffi32;
                if (*eqtb.offset((1i32 + (0x10ffffi32 + 1i32) +
                                      (0x10ffffi32 + 1i32) + 1i32 + 15000i32 +
                                      12i32 + 9000i32 + 1i32 + 1i32 + 19i32 +
                                      256i32 + 256i32 + 13i32 + 256i32 + 4i32
                                      + 256i32 + 1i32 + 3i32 * 256i32 +
                                      (0x10ffffi32 + 1i32) +
                                      (0x10ffffi32 + 1i32) +
                                      (0x10ffffi32 + 1i32) +
                                      (0x10ffffi32 + 1i32) +
                                      (0x10ffffi32 + 1i32) +
                                      (0x10ffffi32 + 1i32) + 65i32) as
                                     isize)).b32.s1 <= 0i32 {
                    flush_node_list(p);
                } else {
                    /* `disc_ptr[LAST_BOX_CODE]` is `tail_page_disc`, the last item
             * removed by the page builder. `disc_ptr[LAST_BOX_CODE]` is
             * `page_disc`, the first item removed by the page builder.
             * `disc_ptr[VSPLIT_CODE]` is `split_disc`, the first item removed
             * by \vsplit. */
                    if disc_ptr[2] == -0xfffffffi32 {
                        disc_ptr[2] = p
                    } else { (*mem.offset(disc_ptr[1] as isize)).b32.s1 = p }
                    disc_ptr[1] = p
                }
                current_block = 15427931788582360902;
            }
            _ => { }
        }
        match current_block {
            5579886686420104461 => {
                /*1039: "Update the current page measurements with respect to the glue or kern
         * specified by node p" */
                if (*mem.offset(p as isize)).b16.s1 as i32 == 11i32 {
                    q = p
                } else {
                    q = (*mem.offset((p + 1i32) as isize)).b32.s0;
                    page_so_far[(2i32 +
                                     (*mem.offset(q as isize)).b16.s1 as
                                         i32) as usize] +=
                        (*mem.offset((q + 2i32) as isize)).b32.s1;
                    page_so_far[6] +=
                        (*mem.offset((q + 3i32) as isize)).b32.s1;
                    if (*mem.offset(q as isize)).b16.s0 as i32 != 0i32
                           &&
                           (*mem.offset((q + 3i32) as isize)).b32.s1 != 0i32 {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr(b"! \x00" as *const u8 as
                                              *const i8);
                        }
                        print_cstr(b"Infinite glue shrinkage found on current page\x00"
                                       as *const u8 as *const i8);
                        help_ptr = 4_u8;
                        help_line[3] =
                            b"The page about to be output contains some infinitely\x00"
                                as *const u8 as *const i8;
                        help_line[2] =
                            b"shrinkable glue, e.g., `\\vss\' or `\\vskip 0pt minus 1fil\'.\x00"
                                as *const u8 as *const i8;
                        help_line[1] =
                            b"Such glue doesn\'t belong there; but you can safely proceed,\x00"
                                as *const u8 as *const i8;
                        help_line[0] =
                            b"since the offensive shrinkability has been made finite.\x00"
                                as *const u8 as *const i8;
                        error();
                        r = new_spec(q);
                        (*mem.offset(r as isize)).b16.s0 = 0_u16;
                        delete_glue_ref(q);
                        (*mem.offset((p + 1i32) as isize)).b32.s0 = r;
                        q = r
                    }
                }
                page_so_far[1] +=
                    page_so_far[7] +
                        (*mem.offset((q + 1i32) as isize)).b32.s1;
                page_so_far[7] = 0i32;
                current_block = 11918621130838443904;
            }
            _ => { }
        }
        match current_block {
            11918621130838443904 => {
                /*1038: "Make sure that page_max_depth is not exceeded." */
                if page_so_far[7] > page_max_depth {
                    page_so_far[1] += page_so_far[7] - page_max_depth;
                    page_so_far[7] = page_max_depth
                }
                /*1033: "Link node p into the current page and goto done." */
                (*mem.offset(page_tail as isize)).b32.s1 =
                    p; /* "vertical mode" */
                page_tail = p;
                (*mem.offset((4999999i32 - 1i32) as isize)).b32.s1 =
                    (*mem.offset(p as isize)).b32.s1;
                (*mem.offset(p as isize)).b32.s1 = -0xfffffffi32
            }
            _ => { }
        }
        if !((*mem.offset((4999999i32 - 1i32) as isize)).b32.s1 !=
                 -0xfffffffi32) {
            break ;
        }
    }
    if nest_ptr == 0i32 {
        cur_list.tail = 4999999i32 - 1i32
    } else {
        (*nest.offset(0)).tail = 4999999i32 - 1i32
    };
    /* "other modes" */
}
