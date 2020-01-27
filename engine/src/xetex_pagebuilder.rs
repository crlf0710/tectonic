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
    best_height_plus_depth, cur_list, cur_mark, cur_ptr, dead_cycles, disc_ptr,
    file_line_error_style_p, help_line, help_ptr, insert_penalties, last_glue, last_kern,
    last_node_type, last_penalty, line, nest, nest_ptr, output_active, page_contents, page_so_far,
    page_tail, sa_root, semantic_pagination_enabled, temp_ptr, MEM,
};
use crate::xetex_output::{print_cstr, print_esc_cstr, print_file_line, print_int, print_nl_cstr};
use crate::xetex_scaledmath::x_over_n;
use crate::xetex_shipout::ship_out;
use crate::xetex_xetex0::{
    badness, begin_token_list, box_error, delete_glue_ref, delete_token_ref, do_marks,
    find_sa_element, flush_node_list, free_node, geq_word_define, get_node, new_null_box,
    new_save_level, new_skip_param, new_spec, normal_paragraph, prune_page_top, push_nest,
    scan_left_brace, vert_break, vpackage,
};
use crate::xetex_xetexd::{
    is_non_discardable_node, BOX_depth, BOX_height, BOX_width, GLUE_NODE_glue_ptr,
    GLUE_SPEC_shrink, GLUE_SPEC_shrink_order, GLUE_SPEC_stretch, GLUE_SPEC_stretch_order,
    LLIST_link, NODE_subtype, NODE_type, PENALTY_NODE_penalty,
};

pub(crate) type scaled_t = i32;
pub(crate) type eight_bits = u8;
pub(crate) type small_number = i16;
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

pub(crate) unsafe fn initialize_pagebuilder_variables() {
    page_max_depth = 0;
}

unsafe fn freeze_page_specs(mut s: small_number) {
    page_contents = s as _;
    page_so_far[0] = *DIMENPAR(DimenPar::vsize);
    page_max_depth = *DIMENPAR(DimenPar::max_depth);
    page_so_far[7] = 0;
    page_so_far[1] = 0;
    page_so_far[2] = 0;
    page_so_far[3] = 0;
    page_so_far[4] = 0;
    page_so_far[5] = 0;
    page_so_far[6] = 0;
    least_page_cost = MAX_HALFWORD;
}

unsafe fn ensure_vbox(mut n: eight_bits) {
    let p = *BOX_REG(n as _);
    if p.is_texnull() {
        return;
    }
    if *NODE_type(p as isize) != HLIST_NODE {
        return;
    }
    if file_line_error_style_p != 0 {
        print_file_line();
    } else {
        print_nl_cstr(b"! ");
    }
    print_cstr(b"Insertions can only be added to a vbox");
    help_ptr = 3;
    help_line[2] = b"Tut tut: You\'re trying to \\insert into a";
    help_line[1] = b"\\box register that now contains an \\hbox.";
    help_line[0] = b"Proceed, and I\'ll discard its present contents.";
    box_error(n);
}

/*1047: "The fire_up subroutine prepares to output the curent page at the best
 * place; then it fires up the user's output routine, if there is one, or it
 * simple ships out the page. There is one parameter, `c`, which represents
 * the node that was being contributed to the page when the decision to force
 * an output was made." */
unsafe fn fire_up(mut c: i32) {
    let mut p: i32 = 0;
    let mut q: i32 = 0;
    let mut r: i32 = 0;
    let mut s: i32 = 0;
    let mut prev_p: i32 = 0;
    let mut n: u8 = 0;
    let mut wait = false;
    let mut save_vbadness: i32 = 0;
    let mut save_vfuzz: scaled_t = 0;
    let mut save_split_top_skip: i32 = 0;
    /*1048: "Set the value of output_penalty" */
    if *NODE_type(best_page_break as isize) == PENALTY_NODE {
        geq_word_define(
            INT_BASE + IntPar::output_penalty as i32,
            *PENALTY_NODE_penalty(best_page_break as isize),
        );
        *PENALTY_NODE_penalty(best_page_break as isize) = INF_PENALTY;
    } else {
        geq_word_define(INT_BASE + IntPar::output_penalty as i32, INF_PENALTY);
    }

    /* ... resuming 1047 ... "We set the values of top_mark, first_mark, and
     * bot_mark. The program uses the fact that `bot_mark != null` implies
     * `first_mark != null`; it also knows that `bot_mark == null` implies
     * `top_mark = first_mark = null`." The do_marks() call basically does the
     * same thing as the code immediately below it, but for all "mark classes"
     * beyond the default one -- a "mark class" being a concept introduced in
     * e-TeX. */

    if !sa_root[MARK_VAL as usize].is_texnull() {
        if do_marks(FIRE_UP_INIT as _, 0, sa_root[MARK_VAL as usize]) {
            sa_root[7] = TEX_NULL;
        }
    }
    if !cur_mark[BOT_MARK_CODE as usize].is_texnull() {
        if !cur_mark[TOP_MARK_CODE as usize].is_texnull() {
            delete_token_ref(cur_mark[TOP_MARK_CODE as usize]);
        }
        cur_mark[TOP_MARK_CODE as usize] = cur_mark[BOT_MARK_CODE as usize];
        MEM[cur_mark[0] as usize].b32.s0 += 1;
        delete_token_ref(cur_mark[FIRST_MARK_CODE as usize]);
        cur_mark[FIRST_MARK_CODE as usize] = TEX_NULL;
    }

    /*1049: "Put the optimal current page into box 255, update first_mark and
     * bot_mark, append insertions to their boxes, and put the remaining nodes
     * back on the contribution list." */

    if c == best_page_break {
        best_page_break = TEX_NULL; /* "c not yet linked in" */
    }
    if !BOX_REG(255).is_texnull() {
        /*1050:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr(b"! ");
        }
        print_cstr(b"");
        print_esc_cstr(b"box");
        print_cstr(b"255 is not void");
        help_ptr = 2;
        help_line[1] = b"You shouldn\'t use \\box255 except in \\output routines.";
        help_line[0] = b"Proceed, and I\'ll discard its present contents.";
        box_error(255);
    }
    insert_penalties = 0; /* "this will count the number of insertions held over" */
    save_split_top_skip = *GLUEPAR(GluePar::split_top_skip);

    /* Tectonic: in semantic pagination mode, we act as if holding_inserts is
     * always active. */

    let process_inserts = *INTPAR(IntPar::holding_inserts) <= 0 && !semantic_pagination_enabled;

    if process_inserts {
        /*1053: "Prepare all the boxes involved in insertions to act as
         * queues". Namely: for each insert being tracked, set the
         * `last_ins_ptr` field of its data structure to the last node in its
         * associated vlist. If holding_inserts is positive, the inserts are
         * just kept in the page vlist without any processing, I believe with
         * the expectation that the output routine will do something clever
         * with them. */
        r = *LLIST_link(PAGE_INS_HEAD as isize);
        while r != PAGE_INS_HEAD as i32 {
            if !MEM[(r + 2) as usize].b32.s0.is_texnull() {
                n = *NODE_subtype(r as _) as _;
                ensure_vbox(n);

                if BOX_REG(n as _).is_texnull() {
                    *BOX_REG(n as _) = new_null_box();
                }

                p = *BOX_REG(n as _) + 5; /* 5 = list_offset, "position of the list inside the box" */
                while !LLIST_link(p as isize).is_texnull() {
                    p = *LLIST_link(p as isize);
                }

                MEM[(r + 2) as usize].b32.s1 = p
            }
            r = *LLIST_link(r as isize);
        }
    }
    q = HOLD_HEAD as i32;
    *LLIST_link(q as isize) = TEX_NULL;
    prev_p = PAGE_HEAD as i32;
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
                if MEM[(r + 2) as usize].b32.s0.is_texnull() {
                    wait = true
                } else {
                    wait = false;

                    s = MEM[(r + 2) as usize].b32.s1;
                    *LLIST_link(s as isize) = MEM[(p + 4) as usize].b32.s0;
                    if MEM[(r + 2) as usize].b32.s0 == p {
                        /*1056: "Wrap up the box specified by node r,
                         * splitting node p if called for; set wait = true if
                         * node p holds a remainder after splitting" */
                        if *NODE_type(r as isize) == SPLIT_UP as _ {
                            if MEM[(r + 1) as usize].b32.s0 == p
                                && MEM[(r + 1) as usize].b32.s1 != -0xfffffff
                            {
                                while *LLIST_link(s as isize) != MEM[(r + 1) as usize].b32.s1 {
                                    s = *LLIST_link(s as isize);
                                }
                                *LLIST_link(s as isize) = TEX_NULL;
                                *GLUEPAR(GluePar::split_top_skip) = MEM[(p + 4) as usize].b32.s1;
                                MEM[(p + 4) as usize].b32.s0 =
                                    prune_page_top(MEM[(r + 1) as usize].b32.s1, false);
                                if !MEM[(p + 4) as usize].b32.s0.is_texnull() {
                                    temp_ptr = vpackage(
                                        MEM[(p + 4) as usize].b32.s0,
                                        0,
                                        ADDITIONAL as _,
                                        MAX_HALFWORD,
                                    );
                                    MEM[(p + 3) as usize].b32.s1 =
                                        MEM[(temp_ptr + 3) as usize].b32.s1
                                            + MEM[(temp_ptr + 2) as usize].b32.s1;
                                    free_node(temp_ptr, BOX_NODE_SIZE);
                                    wait = true
                                }
                            }
                        }
                        MEM[(r + 2) as usize].b32.s0 = TEX_NULL;
                        n = *NODE_subtype(r as isize) as _;
                        temp_ptr = MEM[(*BOX_REG(n as usize) + 5) as usize].b32.s1;
                        free_node(*BOX_REG(n as _), BOX_NODE_SIZE);
                        *BOX_REG(n as _) =
                            vpackage(temp_ptr, 0i32, 1i32 as small_number, 0x3fffffffi32);
                    } else {
                        while !LLIST_link(s as isize).is_texnull() {
                            s = *LLIST_link(s as isize);
                        }
                        MEM[(r + 2) as usize].b32.s1 = s
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
                    delete_glue_ref(MEM[(p + 4) as usize].b32.s1);
                    free_node(p, INS_NODE_SIZE);
                }
                p = prev_p /*:1057 */
            }
        } else if *NODE_type(p as isize) == MARK_NODE {
            if MEM[(p + 1) as usize].b32.s0 != 0 {
                /*1618: "Update the current marks" */
                find_sa_element(MARK_VAL as _, MEM[(p + 1) as usize].b32.s0, true);
                if MEM[(cur_ptr + 1) as usize].b32.s1.is_texnull() {
                    MEM[(cur_ptr + 1) as usize].b32.s1 = MEM[(p + 1) as usize].b32.s1;
                    MEM[MEM[(p + 1) as usize].b32.s1 as usize].b32.s0 += 1;
                }
                if !MEM[(cur_ptr + 2) as usize].b32.s0.is_texnull() {
                    delete_token_ref(MEM[(cur_ptr + 2) as usize].b32.s0);
                }
                MEM[(cur_ptr + 2) as usize].b32.s0 = MEM[(p + 1) as usize].b32.s1;
                MEM[MEM[(p + 1) as usize].b32.s1 as usize].b32.s0 += 1;
            } else {
                /*1051: "Update the values of first_mark and bot_mark" */
                if cur_mark[FIRST_MARK_CODE as usize].is_texnull() {
                    cur_mark[FIRST_MARK_CODE as usize] = MEM[(p + 1) as usize].b32.s1;
                    MEM[cur_mark[1] as usize].b32.s0 += 1;
                }
                if !cur_mark[2].is_texnull() {
                    delete_token_ref(cur_mark[BOT_MARK_CODE as usize]);
                }
                cur_mark[BOT_MARK_CODE as usize] = MEM[(p + 1) as usize].b32.s1;
                MEM[cur_mark[2] as usize].b32.s0 += 1;
            }
        }

        prev_p = p;
        p = *LLIST_link(prev_p as isize);
    }
    *GLUEPAR(GluePar::split_top_skip) = save_split_top_skip;

    /*1052: "Break the current page at node p, put it in box 255, and put the
     * remaining nodes on the contribution list". */
    if !p.is_texnull() {
        if LLIST_link(CONTRIB_HEAD as isize).is_texnull() {
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
    save_vbadness = *INTPAR(IntPar::vbadness);
    *INTPAR(IntPar::vbadness) = INF_BAD;
    save_vfuzz = *DIMENPAR(DimenPar::vfuzz);
    *DIMENPAR(DimenPar::vfuzz) = MAX_HALFWORD;
    *BOX_REG(255) = vpackage(
        *LLIST_link(PAGE_HEAD as isize),
        best_size,
        EXACTLY as _,
        page_max_depth,
    );
    *INTPAR(IntPar::vbadness) = save_vbadness;
    *DIMENPAR(DimenPar::vfuzz) = save_vfuzz;

    if last_glue != MAX_HALFWORD {
        delete_glue_ref(last_glue);
    }

    /*1026: "Start a new current page" */
    page_contents = EMPTY as _;
    page_tail = PAGE_HEAD as i32;
    *LLIST_link(PAGE_HEAD as isize) = TEX_NULL;
    last_glue = MAX_HALFWORD;
    last_penalty = 0;
    last_kern = 0;
    last_node_type = -1;
    page_so_far[7] = 0;
    page_max_depth = 0;

    if q != HOLD_HEAD as i32 {
        *LLIST_link(PAGE_HEAD as isize) = *LLIST_link(HOLD_HEAD as isize);
        page_tail = q
    }

    /*1054: "Delete the page-insertion nodes" */
    r = *LLIST_link(PAGE_INS_HEAD as isize);
    while r != PAGE_INS_HEAD as i32 {
        q = *LLIST_link(r as isize);
        free_node(r, PAGE_INS_NODE_SIZE);
        r = q
    }

    *LLIST_link(PAGE_INS_HEAD as isize) = PAGE_INS_HEAD as i32;

    /* ... resuming 1047 ... */

    if !sa_root[MARK_VAL as usize].is_texnull() {
        if do_marks(FIRE_UP_DONE as _, 0, sa_root[MARK_VAL as usize]) {
            sa_root[MARK_VAL as usize] = TEX_NULL;
        }
    }
    if !cur_mark[TOP_MARK_CODE as usize].is_texnull()
        && cur_mark[FIRST_MARK_CODE as usize].is_texnull()
    {
        cur_mark[FIRST_MARK_CODE as usize] = cur_mark[TOP_MARK_CODE as usize];
        MEM[cur_mark[0] as usize].b32.s0 += 1;
    }

    /* Tectonic: in semantic pagination mode, ignore the output routine. */

    if !LOCAL(Local::output_routine).is_texnull() && !semantic_pagination_enabled {
        if dead_cycles >= *INTPAR(IntPar::max_dead_cycles) {
            /*1059: "Explain that too many dead cycles have happened in a row." */
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr(b"! ");
            }
            print_cstr(b"Output loop---");
            print_int(dead_cycles);
            print_cstr(b" consecutive dead cycles");
            help_ptr = 3;
            help_line[2] = b"I\'ve concluded that your \\output is awry; it never does a";
            help_line[1] = b"\\shipout, so I\'m shipping \\box255 out myself. Next time";
            help_line[0] = b"increase \\maxdeadcycles if you want me to be more patient!";
            error();
        } else {
            /*1060: "Fire up the user's output routine and return" */
            output_active = true;
            dead_cycles += 1;
            push_nest();
            cur_list.mode = -VMODE as _;
            cur_list.aux.b32.s1 = IGNORE_DEPTH; /* this is `prev_depth` */
            cur_list.mode_line = -line;
            begin_token_list(*LOCAL(Local::output_routine), OUTPUT_TEXT);
            new_save_level(OUTPUT_GROUP as _);
            normal_paragraph();
            scan_left_brace();
            return;
        }
    }

    /*1058: "Perform the default output routine." */
    if !LLIST_link(PAGE_HEAD as isize).is_texnull() {
        if LLIST_link(CONTRIB_HEAD as isize).is_texnull() {
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
        page_tail = PAGE_HEAD as i32;
    }

    flush_node_list(disc_ptr[LAST_BOX_CODE as usize]);
    disc_ptr[LAST_BOX_CODE as usize] = TEX_NULL;
    ship_out(*BOX_REG(255));
    *BOX_REG(255) = TEX_NULL;
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

pub(crate) unsafe fn build_page() {
    #[derive(Default)]
    struct Args {
        p: i32,
        q: i32,
        r: i32,
        pi: i32,
    }

    unsafe fn do_smth(mut slf: Args) -> (Args, bool) {
        slf.p = *LLIST_link(CONTRIB_HEAD as isize);

        /*1031: "Update the values of last_glue, last_penalty, and last_kern" */
        if last_glue != MAX_HALFWORD {
            delete_glue_ref(last_glue);
        }

        last_penalty = 0;
        last_kern = 0;
        last_node_type = *NODE_type(slf.p as isize) as i32 + 1;

        if *NODE_type(slf.p as isize) == GLUE_NODE {
            last_glue = *GLUE_NODE_glue_ptr(slf.p as isize);
            MEM[last_glue as usize].b32.s1 += 1;
        } else {
            last_glue = MAX_HALFWORD;

            if *NODE_type(slf.p as isize) == PENALTY_NODE {
                last_penalty = MEM[(slf.p + 1) as usize].b32.s1
            } else if *NODE_type(slf.p as isize) == KERN_NODE {
                last_kern = *BOX_width(slf.p as isize);
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

        match *NODE_type(slf.p as isize) {
            HLIST_NODE | VLIST_NODE | RULE_NODE => {
                if page_contents < BOX_THERE as _ {
                    /*1036: "Initialize the current page, insert the \topskip glue
                     * ahead of p, and goto continue." */
                    if page_contents == EMPTY as _{
                        freeze_page_specs(BOX_THERE as _);
                    } else { page_contents = BOX_THERE as _}

                    slf.q = new_skip_param(GluePar::top_skip as _); /* "now temp_ptr = glue_ptr(q) */

                    if *BOX_width(temp_ptr as isize) > *BOX_height(slf.p as isize) {
                        *BOX_width(temp_ptr as isize) -= *BOX_height(slf.p as isize);
                    } else {
                        *BOX_width(temp_ptr as isize) = 0;
                    }

                    *LLIST_link(slf.q as isize) = slf.p;
                    *LLIST_link(CONTRIB_HEAD as isize) = slf.q;
                    return (slf, false); // TODO: check
                } else {
                    /*1037: "Prepare to move a box or rule node to the current
                 * page, then goto contribute." */
                    page_so_far[1] += page_so_far[7] + *BOX_height(slf.p as isize);
                    page_so_far[7] = *BOX_depth(slf.p as isize);
                    return contribute(slf);
                }
            }
            WHATSIT_NODE => {
                /*1401: "Prepare to move whatsit p to the current page, then goto contribute" */
                if *NODE_subtype(slf.p as isize) == PIC_NODE || *NODE_subtype(slf.p as isize) == PDF_NODE {
                    page_so_far[1] += page_so_far[7] + *BOX_height(slf.p as isize);
                    page_so_far[7] = *BOX_depth(slf.p as isize);
                }
                return contribute(slf);
            }
            GLUE_NODE => {
                if page_contents < BOX_THERE as _ {
                    return done1(slf);
                } else if is_non_discardable_node(page_tail) {
                    slf.pi = 0;
                } else { return update_heights(slf); }
            }
            KERN_NODE => {
                if page_contents  < BOX_THERE as _ {
                    return done1(slf);
                } else if LLIST_link(slf.p as isize).is_texnull() {
                    return (slf, true)
                } else if *NODE_type(*LLIST_link(slf.p as isize) as isize) == GLUE_NODE {
                    slf.pi = 0;
                } else { return update_heights(slf); }
            }
            PENALTY_NODE => {
                if page_contents < BOX_THERE as _ {
                    return done1(slf);
                } else {
                    slf.pi = MEM[(slf.p + 1) as usize].b32.s1;
                }
            }
            MARK_NODE => { return contribute(slf); }
            INS_NODE => {
                /*1043: "Append an insertion to the current page and goto contribute" */
                if page_contents == EMPTY as _ {
                    freeze_page_specs(INSERTS_ONLY as _);
                }

                let n = *NODE_subtype(slf.p as isize) as u8;
                slf.r = PAGE_INS_HEAD as i32;

                while n as u16 >= *NODE_subtype(*LLIST_link(slf.r as isize) as isize) {
                    slf.r = *LLIST_link(slf.r as isize);
                }

                if *NODE_subtype(slf.r as isize) != n as _{
                    /*1044: "Create a page insertion node with subtype(r) = n, and
                     * include the glue correction for box `n` in the current page
                     * state" */
                    slf.q = get_node(PAGE_INS_NODE_SIZE);
                    *LLIST_link(slf.q as isize) = *LLIST_link(slf.r as isize);
                    *LLIST_link(slf.r as isize) = slf.q;
                    slf.r = slf.q;

                    *NODE_subtype(slf.r as isize) = n as _;
                    *NODE_type(slf.r as isize) = INSERTING as _;
                    ensure_vbox(n);

                    if BOX_REG(n as _).is_texnull() {
                        *BOX_height(slf.r as isize) = 0;
                    } else {
                        *BOX_height(slf.r as isize) = *BOX_height(*BOX_REG(n as _) as isize) + *BOX_depth(*BOX_REG(n as _) as isize);
                    }

                    MEM[(slf.r + 2) as usize].b32.s0 = TEX_NULL;
                    slf.q = *SKIP_REG(n as _);

                    let h: scaled_t = if *COUNT_REG(n as _) == 1000 {
                        *BOX_height(slf.r as isize)
                    } else {
                        x_over_n(*BOX_height(slf.r as isize), 1000) * *COUNT_REG(n as _)
                    };

                    page_so_far[0] -= h + *BOX_width(slf.q as isize);
                    page_so_far[2 + *GLUE_SPEC_stretch_order(slf.q as isize) as usize] += *GLUE_SPEC_stretch(slf.q as isize);
                    page_so_far[6] += *GLUE_SPEC_shrink(slf.q as isize);

                    if *GLUE_SPEC_shrink_order(slf.q as isize) != NORMAL as _ && *GLUE_SPEC_shrink(slf.q as isize) != 0 {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr(b"! ");
                        }
                        print_cstr(b"Infinite glue shrinkage inserted from ");
                        print_esc_cstr(b"skip");
                        print_int(n as i32);
                        help_ptr = 3;
                        help_line[2] =
                            b"The correction glue for page breaking with insertions";
                        help_line[1] =
                            b"must have finite shrinkability. But you may proceed,";
                        help_line[0] =
                            b"since the offensive shrinkability has been made finite.";
                        error();
                    }
                }

                if *NODE_type(slf.r as isize) == SPLIT_UP as _ {
                    insert_penalties +=
                        MEM[(slf.p + 1) as usize].b32.s1
                } else {
                    MEM[(slf.r + 2) as usize].b32.s1 = slf.p;
                    let delta: scaled_t =
                        page_so_far[0] - page_so_far[1] - page_so_far[7] +
                            page_so_far[6];

                    let h: scaled_t = if *COUNT_REG(n as _) == 1000 {
                        *BOX_height(slf.p as isize)
                    } else {
                        x_over_n(*BOX_height(slf.p as isize), 1000) * *COUNT_REG(n as _)
                    };

                    if (h <= 0 || h <= delta) &&
                        *BOX_height(slf.p as isize) + *BOX_height(slf.r as isize) <= *SCALED_REG(n as _) {
                        page_so_far[0] -= h;
                        *BOX_height(slf.r as isize) += *BOX_height(slf.p as isize);
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
                        let mut w: scaled_t = if *COUNT_REG(n as _) <= 0 {
                            MAX_HALFWORD
                        } else {
                            let mut w =
                                page_so_far[0] - page_so_far[1] -
                                    page_so_far[7];
                            if *COUNT_REG(n as _) != 1000 {
                                w = x_over_n(w, *COUNT_REG(n as _))* 1000;
                            }
                            w
                        };

                        if w > *SCALED_REG(n as _) - *BOX_height(slf.r as isize) {
                            w = *SCALED_REG(n as _) - *BOX_height(slf.r as isize);
                        }

                        slf.q =
                            vert_break(MEM[(slf.p + 4) as
                                                        usize].b32.s0, w,
                                       MEM[(slf.p + 2) as
                                                        usize].b32.s1);
                        MEM[(slf.r + 3) as usize].b32.s1 += best_height_plus_depth;

                        if *COUNT_REG(n as _) != 1000 {
                            best_height_plus_depth =
                                x_over_n(best_height_plus_depth, 1000i32) * *COUNT_REG(n as _);
                        }
                        page_so_far[0] -= best_height_plus_depth;
                        *NODE_type(slf.r as isize) = SPLIT_UP as _;
                        MEM[(slf.r + 1) as usize].b32.s1 = slf.q;
                        MEM[(slf.r + 1) as usize].b32.s0 = slf.p;

                        if slf.q.is_texnull() {
                            insert_penalties += EJECT_PENALTY;
                        } else if *NODE_type(slf.q as isize) == PENALTY_NODE {
                            insert_penalties +=
                                MEM[(slf.q + 1) as usize].b32.s1
                        }
                    }
                }
                return contribute(slf);
            }
            _ => confusion(b"page"),
        }

        /*1040: "Check if node p is the new champion breakpoint; then if it is
         * time for a page break, prepare for output, and either fire up the
         * user's output routine and return or ship out the page and goto
         * done." We reach this point when p is a glue, kern, or penalty, and
         * there's already content on the page -- so this might be a place to
         * break the page. */

        if slf.pi < INF_PENALTY {
            /*1042: "Compute the badness b of the current page, using
             * awful_bad if the box is too full." */
            let b = if page_so_far[1] < page_so_far[0] {
                if page_so_far[3] != 0 || page_so_far[4] != 0 || page_so_far[5] != 0 {
                    0_i32
                } else {
                    badness(page_so_far[0] - page_so_far[1], page_so_far[2])
                }
            } else if page_so_far[1] - page_so_far[0] > page_so_far[6] {
                AWFUL_BAD
            } else {
                badness(page_so_far[1] - page_so_far[0], page_so_far[6])
            };

            let mut c = if b < AWFUL_BAD {
                if slf.pi <= EJECT_PENALTY {
                    slf.pi as i32
                } else if b < INF_BAD {
                    b + slf.pi + insert_penalties
                } else {
                    100000
                }
            /* DEPLORABLE */
            } else {
                b
            };

            if insert_penalties >= 10000 {
                c = MAX_HALFWORD
            }

            if c <= least_page_cost {
                best_page_break = slf.p;
                best_size = page_so_far[0];
                least_page_cost = c;
                slf.r = *LLIST_link(PAGE_INS_HEAD as isize);

                while slf.r != PAGE_INS_HEAD as i32 {
                    MEM[(slf.r + 2) as usize].b32.s0 = MEM[(slf.r + 2) as usize].b32.s1;
                    slf.r = *LLIST_link(slf.r as isize);
                }
            }

            if c == AWFUL_BAD || slf.pi <= EJECT_PENALTY {
                fire_up(slf.p);
                if output_active {
                    /* "user's output routine will act" */
                    return (slf, true);
                }
                /* "the page has been shipped out by the default output routine" */
                return (slf, false);
            }
        }

        /* ... resuming 1032 ... I believe the "goto" here can only be
         * triggered if p is a penalty node, and we decided not to break. */

        if *NODE_type(slf.p as isize) < GLUE_NODE || *NODE_type(slf.p as isize) > KERN_NODE {
            return contribute(slf);
        }

        return update_heights(slf);

        unsafe fn update_heights(mut slf: Args) -> (Args, bool) {
            /*1039: "Update the current page measurements with respect to the glue or kern
             * specified by node p" */
            if *NODE_type(slf.p as isize) == KERN_NODE {
                slf.q = slf.p
            } else {
                slf.q = MEM[(slf.p + 1) as usize].b32.s0;
                page_so_far[(2i32 + MEM[slf.q as usize].b16.s1 as i32) as usize] +=
                    MEM[(slf.q + 2) as usize].b32.s1;
                page_so_far[6] += MEM[(slf.q + 3) as usize].b32.s1;
                if MEM[slf.q as usize].b16.s0 as i32 != 0 && MEM[(slf.q + 3) as usize].b32.s1 != 0 {
                    if file_line_error_style_p != 0 {
                        print_file_line();
                    } else {
                        print_nl_cstr(b"! ");
                    }
                    print_cstr(b"Infinite glue shrinkage found on current page");
                    help_ptr = 4_u8;
                    help_line[3] = b"The page about to be output contains some infinitely";
                    help_line[2] = b"shrinkable glue, e.g., `\\vss\' or `\\vskip 0pt minus 1fil\'.";
                    help_line[1] = b"Such glue doesn\'t belong there; but you can safely proceed,";
                    help_line[0] = b"since the offensive shrinkability has been made finite.";
                    error();
                    slf.r = new_spec(slf.q);
                    MEM[slf.r as usize].b16.s0 = 0_u16;
                    delete_glue_ref(slf.q);
                    MEM[(slf.p + 1) as usize].b32.s0 = slf.r;
                    slf.q = slf.r
                }
            }
            page_so_far[1] += page_so_far[7] + MEM[(slf.q + 1) as usize].b32.s1;
            page_so_far[7] = 0i32;
            return contribute(slf);
        }

        unsafe fn contribute(mut slf: Args) -> (Args, bool) {
            /*1038: "Make sure that page_max_depth is not exceeded." */
            if page_so_far[7] > page_max_depth {
                page_so_far[1] += page_so_far[7] - page_max_depth;
                page_so_far[7] = page_max_depth
            }
            /*1033: "Link node p into the current page and goto done." */
            *LLIST_link(page_tail as isize) = slf.p;
            page_tail = slf.p;
            *LLIST_link(CONTRIB_HEAD as isize) = *LLIST_link(slf.p as isize);
            *LLIST_link(slf.p as isize) = TEX_NULL;
            (slf, false)
        }

        unsafe fn done1(mut slf: Args) -> (Args, bool) {
            /*1034: "Recycle node p". This codepath is triggered if we encountered
             * something nonprinting (glue, kern, penalty) and there aren't any
             * yes-printing boxes at the top of the page yet. When that happens,
             * we just discard the nonprinting node. */
            *LLIST_link(CONTRIB_HEAD as isize) = *LLIST_link(slf.p as isize);
            *LLIST_link(slf.p as isize) = TEX_NULL;

            if *INTPAR(IntPar::saving_vdiscards) <= 0 {
                flush_node_list(slf.p);
            } else {
                /* `disc_ptr[LAST_BOX_CODE]` is `tail_page_disc`, the last item
                 * removed by the page builder. `disc_ptr[LAST_BOX_CODE]` is
                 * `page_disc`, the first item removed by the page builder.
                 * `disc_ptr[VSPLIT_CODE]` is `split_disc`, the first item removed
                 * by \vsplit. */
                if disc_ptr[LAST_BOX_CODE as usize].is_texnull() {
                    disc_ptr[LAST_BOX_CODE as usize] = slf.p
                } else {
                    *LLIST_link(disc_ptr[COPY_CODE as usize] as isize) = slf.p;
                }
                disc_ptr[COPY_CODE as usize] = slf.p
            }
            (slf, false)
        }
    }

    if LLIST_link(CONTRIB_HEAD as isize).is_texnull() || output_active as i32 != 0 {
        return;
    }

    let mut args = Args::default();

    loop {
        let (slf, halt) = do_smth(args);
        args = slf;
        if halt {
            return;
        };
        if LLIST_link(CONTRIB_HEAD as isize).is_texnull() {
            break;
        }
    }
    if nest_ptr == 0 {
        cur_list.tail = CONTRIB_HEAD as i32; /* "vertical mode" */
    } else {
        (*nest.offset(0)).tail = CONTRIB_HEAD as i32; /* "other modes" */
    };
    /* "other modes" */
}
