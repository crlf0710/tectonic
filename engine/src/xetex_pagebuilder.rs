#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::help;
use crate::node::*;
use crate::xetex_consts::*;
use crate::xetex_errors::{confusion, error};
use crate::xetex_ini::{
    best_height_plus_depth, cur_list, cur_mark, cur_ptr, dead_cycles, disc_ptr,
    file_line_error_style_p, input_state_t, insert_penalties, last_glue, last_kern, last_node_type,
    last_penalty, line, output_active, page_contents, page_so_far, page_tail, sa_root,
    semantic_pagination_enabled, NEST, NEST_PTR,
};
use crate::xetex_output::{print_cstr, print_esc_cstr, print_file_line, print_int, print_nl_cstr};
use crate::xetex_scaledmath::{x_over_n, Scaled};
use crate::xetex_shipout::ship_out;
use crate::xetex_xetex0::{
    badness, begin_token_list, box_error, delete_glue_ref, delete_token_ref, do_marks,
    find_sa_element, flush_node_list, free_node, geq_word_define, get_node, new_null_box,
    new_save_level, new_skip_param, new_spec, normal_paragraph, prune_page_top, push_nest,
    scan_left_brace, vert_break, vpackage,
};
use crate::xetex_xetexd::{llist_link, LLIST_link, TOKEN_LIST_ref_count, TeXInt, TeXOpt};

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
static mut best_page_break: Option<usize> = Some(0);
static mut best_size: Scaled = Scaled::ZERO;
static mut least_page_cost: i32 = 0;
static mut page_max_depth: Scaled = Scaled::ZERO;
/* XXX other variables belong here but pop up all over the code */

pub(crate) unsafe fn initialize_pagebuilder_variables() {
    page_max_depth = Scaled::ZERO;
}

unsafe fn freeze_page_specs(s: PageContents) {
    page_contents = s;
    page_so_far[0] = *DIMENPAR(DimenPar::vsize);
    page_max_depth = *DIMENPAR(DimenPar::max_depth);
    page_so_far[7] = Scaled::ZERO;
    page_so_far[1] = Scaled::ZERO;
    page_so_far[2] = Scaled::ZERO;
    page_so_far[3] = Scaled::ZERO;
    page_so_far[4] = Scaled::ZERO;
    page_so_far[5] = Scaled::ZERO;
    page_so_far[6] = Scaled::ZERO;
    least_page_cost = MAX_HALFWORD;
}

unsafe fn ensure_vbox(mut n: u8) {
    if let Some(p) = BOX_REG(n as _).opt() {
        if List::from(p).is_horizontal() {
            if file_line_error_style_p != 0 {
                print_file_line();
            } else {
                print_nl_cstr("! ");
            }
            print_cstr("Insertions can only be added to a vbox");
            help!(
                "Tut tut: You\'re trying to \\insert into a",
                "\\box register that now contains an \\hbox.",
                "Proceed, and I\'ll discard its present contents."
            );
            box_error(n);
        }
    }
}

/*1047: "The fire_up subroutine prepares to output the curent page at the best
 * place; then it fires up the user's output routine, if there is one, or it
 * simple ships out the page. There is one parameter, `c`, which represents
 * the node that was being contributed to the page when the decision to force
 * an output was made." */
unsafe fn fire_up(input: &mut input_state_t, c: usize) {
    let mut s: usize = 0;
    let mut n: u8 = 0;
    let mut wait = false;
    let mut save_vbadness: i32 = 0;
    let mut save_vfuzz: Scaled = Scaled::ZERO;
    let mut save_split_top_skip: i32 = 0;
    /*1048: "Set the value of output_penalty" */
    let bpb = best_page_break.unwrap();
    if let TxtNode::Penalty(bpb) = &mut TxtNode::from(bpb) {
        geq_word_define(
            INT_BASE as usize + (IntPar::output_penalty as usize),
            bpb.penalty(),
        );
        bpb.set_penalty(INF_PENALTY);
    } else {
        geq_word_define(
            INT_BASE as usize + (IntPar::output_penalty as usize),
            INF_PENALTY,
        );
    }

    /* ... resuming 1047 ... "We set the values of top_mark, first_mark, and
     * bot_mark. The program uses the fact that `bot_mark != null` implies
     * `first_mark != null`; it also knows that `bot_mark == null` implies
     * `top_mark = first_mark = null`." The do_marks() call basically does the
     * same thing as the code immediately below it, but for all "mark classes"
     * beyond the default one -- a "mark class" being a concept introduced in
     * e-TeX. */

    if let Some(m) = sa_root[ValLevel::Mark as usize] {
        if do_marks(MarkMode::FireUpInit, 0, m) {
            sa_root[ValLevel::Mark as usize] = None;
        }
    }
    if let Some(mbot) = cur_mark[BOT_MARK_CODE] {
        if let Some(mtop) = cur_mark[TOP_MARK_CODE] {
            delete_token_ref(mtop);
        }
        cur_mark[TOP_MARK_CODE as usize] = Some(mbot);
        *TOKEN_LIST_ref_count(mbot) += 1;
        delete_token_ref(cur_mark[FIRST_MARK_CODE].unwrap());
        cur_mark[FIRST_MARK_CODE] = None;
    }

    /*1049: "Put the optimal current page into box 255, update first_mark and
     * bot_mark, append insertions to their boxes, and put the remaining nodes
     * back on the contribution list." */

    if c == bpb {
        best_page_break = None; /* "c not yet linked in" */
    }
    if BOX_REG(255).opt().is_some() {
        /*1050:*/
        if file_line_error_style_p != 0 {
            print_file_line();
        } else {
            print_nl_cstr("! ");
        }
        print_cstr("");
        print_esc_cstr("box");
        print_cstr("255 is not void");
        help!(
            "You shouldn\'t use \\box255 except in \\output routines.",
            "Proceed, and I\'ll discard its present contents."
        );
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
        let mut r = PageInsertion(llist_link(PAGE_INS_HEAD).unwrap());
        while r.ptr() != PAGE_INS_HEAD {
            if r.best_ins_ptr().opt().is_some() {
                n = r.box_reg() as _;
                ensure_vbox(n);

                if BOX_REG(n as _).opt().is_none() {
                    *BOX_REG(n as _) = Some(new_null_box()).tex_int();
                }

                let mut p = (*BOX_REG(n as _) as usize) + 5; /* 5 = list_offset, "position of the list inside the box" */
                while let Some(next) = llist_link(p) {
                    p = next;
                }

                r.set_last_ins_ptr(Some(p).tex_int());
            }
            r = PageInsertion(llist_link(r.ptr()).unwrap());
        }
    }
    let mut q = HOLD_HEAD;
    *LLIST_link(q) = None.tex_int();
    let mut prev_p = PAGE_HEAD;
    let mut popt = llist_link(prev_p);

    while popt != best_page_break {
        let mut p = popt.unwrap();
        match &mut TxtNode::from(p) {
            TxtNode::Ins(p_ins) => {
                if process_inserts {
                    /*1055: "Either insert the material specified by node p into
                     * the appropriate box, or hold it for the next page; also
                     * delete node p from the current page." */
                    let mut r = PageInsertion(llist_link(PAGE_INS_HEAD).unwrap());

                    while r.box_reg() != p_ins.box_reg() {
                        r = PageInsertion(llist_link(r.ptr()).unwrap());
                    }
                    if r.best_ins_ptr().opt().is_none() {
                        wait = true
                    } else {
                        wait = false;

                        s = r.last_ins_ptr() as usize;
                        *LLIST_link(s) = p_ins.ins_ptr();
                        if r.best_ins_ptr().opt() == Some(p) {
                            /*1056: "Wrap up the box specified by node r,
                             * splitting node p if called for; set wait = true if
                             * node p holds a remainder after splitting" */
                            if r.subtype() == PageInsType::SplitUp {
                                if r.broken_ins().opt() == Some(p) && r.broken_ptr().opt().is_some()
                                {
                                    while *LLIST_link(s) != r.broken_ptr() {
                                        s = *LLIST_link(s) as usize;
                                    }
                                    *LLIST_link(s) = None.tex_int();
                                    *GLUEPAR(GluePar::split_top_skip) = p_ins.split_top_ptr();
                                    p_ins.set_ins_ptr(prune_page_top(r.broken_ptr().opt(), false));

                                    if p_ins.ins_ptr().opt().is_some() {
                                        let tmp_ptr = vpackage(
                                            p_ins.ins_ptr().opt(),
                                            Scaled::ZERO,
                                            PackMode::Additional as _,
                                            Scaled::MAX_HALFWORD,
                                        );
                                        p_ins.set_height(tmp_ptr.height() + tmp_ptr.depth());
                                        free_node(tmp_ptr.ptr(), BOX_NODE_SIZE);
                                        wait = true
                                    }
                                }
                            }

                            r.set_best_ins_ptr(None.tex_int());
                            n = r.box_reg() as _; // NODE_subtype(r)
                            let b = List::from(*BOX_REG(n as usize) as usize);
                            let tmp_ptr = b.list_ptr().opt();
                            b.free();
                            *BOX_REG(n as _) = Some(
                                vpackage(
                                    tmp_ptr,
                                    Scaled::ZERO,
                                    PackMode::Additional,
                                    Scaled::MAX_HALFWORD,
                                )
                                .ptr(),
                            )
                            .tex_int();
                        } else {
                            while let Some(next) = llist_link(s) {
                                s = next;
                            }
                            r.set_last_ins_ptr(s as i32);
                        }
                    }

                    /*1057: "Either append the insertion node p after node q, and
                     * remove it from the current page, or delete node(p)" */

                    *LLIST_link(prev_p) = *LLIST_link(p);
                    *LLIST_link(p) = None.tex_int();

                    if wait {
                        *LLIST_link(q) = Some(p).tex_int();
                        q = p;
                        insert_penalties += 1;
                    } else {
                        delete_glue_ref(p_ins.split_top_ptr() as usize);
                        free_node(p, INS_NODE_SIZE);
                    }
                    p = prev_p /*:1057 */
                }
            }
            TxtNode::Mark(p) => {
                if p.class() != 0 {
                    /*1618: "Update the current marks" */
                    find_sa_element(ValLevel::Mark as _, p.class(), true);

                    let mut c = EtexMark(cur_ptr.unwrap());
                    if c.sa_first_mark().opt().is_none() {
                        c.set_sa_first_mark(p.mark_ptr());
                        MarkClass(p.mark_ptr() as usize).rc_inc();
                    }

                    if let Some(m) = c.sa_bot_mark().opt() {
                        delete_token_ref(m);
                    }

                    c.set_sa_bot_mark(p.mark_ptr());
                    MarkClass(p.mark_ptr() as usize).rc_inc();
                } else {
                    /*1051: "Update the values of first_mark and bot_mark" */
                    if cur_mark[FIRST_MARK_CODE].is_none() {
                        cur_mark[FIRST_MARK_CODE] = p.mark_ptr().opt();
                        *TOKEN_LIST_ref_count(cur_mark[FIRST_MARK_CODE].unwrap()) += 1;
                    }
                    if let Some(m) = cur_mark[BOT_MARK_CODE] {
                        delete_token_ref(m);
                    }
                    cur_mark[BOT_MARK_CODE] = p.mark_ptr().opt();
                    *TOKEN_LIST_ref_count(cur_mark[BOT_MARK_CODE].unwrap()) += 1;
                }
            }
            _ => {}
        }

        prev_p = p;
        popt = llist_link(prev_p);
    }
    *GLUEPAR(GluePar::split_top_skip) = save_split_top_skip;

    /*1052: "Break the current page at node p, put it in box 255, and put the
     * remaining nodes on the contribution list". */
    if let Some(p) = popt {
        if llist_link(CONTRIB_HEAD).is_none() {
            if NEST_PTR == 0 {
                cur_list.tail = page_tail;
            } else {
                NEST[0].tail = page_tail;
            }
        }
        *LLIST_link(page_tail) = *LLIST_link(CONTRIB_HEAD);
        *LLIST_link(CONTRIB_HEAD) = Some(p).tex_int();
        *LLIST_link(prev_p) = None.tex_int();
    }

    /* Temporarily futz some variables to inhibit error messages */
    save_vbadness = *INTPAR(IntPar::vbadness);
    *INTPAR(IntPar::vbadness) = INF_BAD;
    save_vfuzz = *DIMENPAR(DimenPar::vfuzz);
    *DIMENPAR(DimenPar::vfuzz) = Scaled::MAX_HALFWORD;
    *BOX_REG(255) = vpackage(
        LLIST_link(PAGE_HEAD as usize).opt(),
        best_size,
        PackMode::Exactly as _,
        page_max_depth,
    )
    .ptr() as i32;
    *INTPAR(IntPar::vbadness) = save_vbadness;
    *DIMENPAR(DimenPar::vfuzz) = save_vfuzz;

    if last_glue != MAX_HALFWORD {
        delete_glue_ref(last_glue as usize);
    }

    /*1026: "Start a new current page" */
    page_contents = PageContents::Empty;
    page_tail = PAGE_HEAD;
    *LLIST_link(PAGE_HEAD) = None.tex_int();
    last_glue = MAX_HALFWORD;
    last_penalty = 0;
    last_kern = Scaled::ZERO;
    last_node_type = -1;
    page_so_far[7] = Scaled::ZERO;
    page_max_depth = Scaled::ZERO;

    if q != HOLD_HEAD {
        *LLIST_link(PAGE_HEAD) = *LLIST_link(HOLD_HEAD);
        page_tail = q;
    }

    /*1054: "Delete the page-insertion nodes" */
    let mut r = llist_link(PAGE_INS_HEAD).unwrap();
    while r != PAGE_INS_HEAD {
        q = llist_link(r).unwrap();
        free_node(r, PAGE_INS_NODE_SIZE);
        r = q;
    }

    *LLIST_link(PAGE_INS_HEAD) = PAGE_INS_HEAD as i32;

    /* ... resuming 1047 ... */

    if let Some(m) = sa_root[ValLevel::Mark as usize] {
        if do_marks(MarkMode::FireUpDone, 0, m) {
            sa_root[ValLevel::Mark as usize] = None;
        }
    }
    if let Some(m) = cur_mark[TOP_MARK_CODE] {
        if cur_mark[FIRST_MARK_CODE].is_none() {
            cur_mark[FIRST_MARK_CODE] = Some(m);
            *TOKEN_LIST_ref_count(m) += 1;
        }
    }

    /* Tectonic: in semantic pagination mode, ignore the output routine. */

    if let Some(l) = LOCAL(Local::output_routine).opt() {
        if !semantic_pagination_enabled {
            if dead_cycles >= *INTPAR(IntPar::max_dead_cycles) {
                /*1059: "Explain that too many dead cycles have happened in a row." */
                if file_line_error_style_p != 0 {
                    print_file_line();
                } else {
                    print_nl_cstr("! ");
                }
                print_cstr("Output loop---");
                print_int(dead_cycles);
                print_cstr(" consecutive dead cycles");
                help!(
                    "I\'ve concluded that your \\output is awry; it never does a",
                    "\\shipout, so I\'m shipping \\box255 out myself. Next time",
                    "increase \\maxdeadcycles if you want me to be more patient!"
                );
                error();
            } else {
                /*1060: "Fire up the user's output routine and return" */
                output_active = true;
                dead_cycles += 1;
                push_nest();
                cur_list.mode = (true, ListMode::VMode);
                cur_list.aux.b32.s1 = IGNORE_DEPTH; /* this is `prev_depth` */
                cur_list.mode_line = -line;
                begin_token_list(input, l, Btl::OutputText);
                new_save_level(GroupCode::Output);
                normal_paragraph();
                scan_left_brace(input);
                return;
            }
        }
    }

    /*1058: "Perform the default output routine." */
    if let Some(p) = LLIST_link(PAGE_HEAD as usize).opt() {
        if let Some(ch) = llist_link(CONTRIB_HEAD) {
            *LLIST_link(page_tail) = Some(ch).tex_int();
        } else {
            if NEST_PTR == 0 {
                cur_list.tail = page_tail;
            } else {
                NEST[0].tail = page_tail;
            }
        }

        *LLIST_link(CONTRIB_HEAD) = Some(p).tex_int();
        *LLIST_link(PAGE_HEAD) = None.tex_int();
        page_tail = PAGE_HEAD;
    }

    flush_node_list(disc_ptr[LAST_BOX_CODE as usize].opt());
    disc_ptr[LAST_BOX_CODE as usize] = None.tex_int();
    ship_out(List::from(*BOX_REG(255) as usize));
    *BOX_REG(255) = None.tex_int();
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

pub(crate) unsafe fn build_page(input: &mut input_state_t) {
    /*1040: "Check if node p is the new champion breakpoint; then if it is
     * time for a page break, prepare for output, and either fire up the
     * user's output routine and return or ship out the page and goto
     * done." We reach this point when p is a glue, kern, or penalty, and
     * there's already content on the page -- so this might be a place to
     * break the page. */
    unsafe fn with_penalty(input: &mut input_state_t, p: usize, pi: i32) -> Option<bool> {
        if pi < INF_PENALTY {
            /*1042: "Compute the badness b of the current page, using
             * awful_bad if the box is too full." */
            let b = if page_so_far[1] < page_so_far[0] {
                if page_so_far[3] != Scaled::ZERO
                    || page_so_far[4] != Scaled::ZERO
                    || page_so_far[5] != Scaled::ZERO
                {
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
                if pi <= EJECT_PENALTY {
                    pi as i32
                } else if b < INF_BAD {
                    b + pi + insert_penalties
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
                best_page_break = Some(p);
                best_size = page_so_far[0];
                least_page_cost = c;
                let mut r = *LLIST_link(PAGE_INS_HEAD) as usize;

                while r != PAGE_INS_HEAD {
                    let mut r_pins = PageInsertion(r);
                    r_pins.set_best_ins_ptr(r_pins.last_ins_ptr());
                    r = *LLIST_link(r) as usize;
                }
            }

            if c == AWFUL_BAD || pi <= EJECT_PENALTY {
                fire_up(input, p);
                if output_active {
                    /* "user's output routine will act" */
                    return Some(true);
                }
                /* "the page has been shipped out by the default output routine" */
                return Some(false);
            }
        }
        None
    }

    /* ... resuming 1032 ... I believe the "goto" here can only be
     * triggered if p is a penalty node, and we decided not to break. */

    unsafe fn contribute(p: usize) {
        /*1038: "Make sure that page_max_depth is not exceeded." */
        if page_so_far[7] > page_max_depth {
            page_so_far[1] += page_so_far[7] - page_max_depth;
            page_so_far[7] = page_max_depth
        }
        /*1033: "Link node p into the current page and goto done." */
        *LLIST_link(page_tail) = Some(p).tex_int();
        page_tail = p;
        *LLIST_link(CONTRIB_HEAD) = *LLIST_link(p);
        *LLIST_link(p) = None.tex_int();
    }

    unsafe fn done1(p: usize) {
        /*1034: "Recycle node p". This codepath is triggered if we encountered
         * something nonprinting (glue, kern, penalty) and there aren't any
         * yes-printing boxes at the top of the page yet. When that happens,
         * we just discard the nonprinting node. */
        *LLIST_link(CONTRIB_HEAD) = *LLIST_link(p);
        *LLIST_link(p) = None.tex_int();

        if *INTPAR(IntPar::saving_vdiscards) <= 0 {
            flush_node_list(Some(p));
        } else {
            /* `disc_ptr[LAST_BOX_CODE]` is `tail_page_disc`, the last item
             * removed by the page builder. `disc_ptr[LAST_BOX_CODE]` is
             * `page_disc`, the first item removed by the page builder.
             * `disc_ptr[VSPLIT_CODE]` is `split_disc`, the first item removed
             * by \vsplit. */
            if disc_ptr[LAST_BOX_CODE as usize].opt().is_none() {
                disc_ptr[LAST_BOX_CODE as usize] = Some(p).tex_int();
            } else {
                *LLIST_link(disc_ptr[COPY_CODE as usize] as usize) = Some(p).tex_int();
            }
            disc_ptr[COPY_CODE as usize] = Some(p).tex_int()
        }
    }

    unsafe fn do_smth(input: &mut input_state_t) -> bool {
        let p = llist_link(CONTRIB_HEAD).unwrap();

        /*1031: "Update the values of last_glue, last_penalty, and last_kern" */
        if last_glue != MAX_HALFWORD {
            delete_glue_ref(last_glue as usize);
        }

        last_penalty = 0;
        last_kern = Scaled::ZERO;
        last_node_type = p as i32 + 1;

        last_glue = MAX_HALFWORD;
        let p_node = TxtNode::from(p);
        match &p_node {
            TxtNode::Glue(g) => {
                last_glue = g.glue_ptr();
                GlueSpec(last_glue as usize).rc_inc();
            }
            TxtNode::Penalty(p) => {
                last_penalty = p.penalty();
            }
            TxtNode::Kern(k) => {
                last_kern = k.width();
            }
            _ => {}
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

        match p_node {
            TxtNode::List(b) => {
                if page_contents == PageContents::Empty
                    || page_contents == PageContents::InsertsOnly
                {
                    /*1036: "Initialize the current page, insert the \topskip glue
                     * ahead of p, and goto continue." */
                    if page_contents == PageContents::Empty {
                        freeze_page_specs(PageContents::BoxThere);
                    } else {
                        page_contents = PageContents::BoxThere
                    }

                    let (q, mut tmp_ptr) = new_skip_param(GluePar::top_skip);

                    if tmp_ptr.size() > b.height() {
                        tmp_ptr.set_size(tmp_ptr.size() - b.height());
                    } else {
                        tmp_ptr.set_size(Scaled::ZERO);
                    }

                    *LLIST_link(q) = Some(b.ptr()).tex_int();
                    *LLIST_link(CONTRIB_HEAD) = Some(q).tex_int();
                } else {
                    /*1037: "Prepare to move a box or rule node to the current
                     * page, then goto contribute." */
                    page_so_far[1] += page_so_far[7] + b.height();
                    page_so_far[7] = b.depth();
                    contribute(b.ptr());
                }
                false
            }
            TxtNode::Rule(r) => {
                if page_contents == PageContents::Empty
                    || page_contents == PageContents::InsertsOnly
                {
                    /*1036: "Initialize the current page, insert the \topskip glue
                     * ahead of p, and goto continue." */
                    if page_contents == PageContents::Empty {
                        freeze_page_specs(PageContents::BoxThere);
                    } else {
                        page_contents = PageContents::BoxThere
                    }

                    let (q, mut tmp_ptr) = new_skip_param(GluePar::top_skip);

                    if tmp_ptr.size() > r.height() {
                        tmp_ptr.set_size(tmp_ptr.size() - r.height());
                    } else {
                        tmp_ptr.set_size(Scaled::ZERO);
                    }

                    *LLIST_link(q) = Some(r.ptr()).tex_int();
                    *LLIST_link(CONTRIB_HEAD) = Some(q).tex_int();
                } else {
                    /*1037: "Prepare to move a box or rule node to the current
                     * page, then goto contribute." */
                    page_so_far[1] += page_so_far[7] + r.height();
                    page_so_far[7] = r.depth();
                    contribute(r.ptr());
                }
                false
            }
            TxtNode::WhatsIt(w) => {
                /*1401: "Prepare to move whatsit p to the current page, then goto contribute" */
                match w {
                    WhatsIt::Pic(p) | WhatsIt::Pdf(p) => {
                        page_so_far[1] += page_so_far[7] + p.height();
                        page_so_far[7] = p.depth();
                    }
                    _ => {}
                }
                contribute(p);
                false
            }
            TxtNode::Glue(mut g) => {
                if page_contents == PageContents::Empty
                    || page_contents == PageContents::InsertsOnly
                {
                    done1(g.ptr());
                } else {
                    match TxtNode::from(page_tail) {
                        TxtNode::List(_)
                        | TxtNode::Rule(_)
                        | TxtNode::Ins(_)
                        | TxtNode::Mark(_)
                        | TxtNode::Adjust(_)
                        | TxtNode::Ligature(_)
                        | TxtNode::Disc(_)
                        | TxtNode::WhatsIt(_) => {
                            if let Some(res) = with_penalty(input, g.ptr(), 0) {
                                return res;
                            }
                        }
                        _ => {}
                    }
                    let q = g.glue_ptr() as usize;
                    let q_spec = GlueSpec(q);
                    page_so_far[2 + q_spec.stretch_order() as usize] += q_spec.stretch();
                    page_so_far[6] += q_spec.shrink();
                    let width = if q_spec.shrink_order() != GlueOrder::Normal
                        && q_spec.shrink() != Scaled::ZERO
                    {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Infinite glue shrinkage found on current page");
                        help!(
                            "The page about to be output contains some infinitely",
                            "shrinkable glue, e.g., `\\vss\' or `\\vskip 0pt minus 1fil\'.",
                            "Such glue doesn\'t belong there; but you can safely proceed,",
                            "since the offensive shrinkability has been made finite."
                        );
                        error();
                        let r = new_spec(q);
                        let mut r_spec = GlueSpec(r);
                        r_spec.set_shrink_order(GlueOrder::Normal);
                        delete_glue_ref(q);
                        g.set_glue_ptr(Some(r).tex_int());
                        r_spec.size()
                    } else {
                        q_spec.size()
                    };
                    page_so_far[1] += page_so_far[7] + width;
                    page_so_far[7] = Scaled::ZERO;
                    contribute(g.ptr());
                }
                false
            }
            TxtNode::Kern(k) => {
                if page_contents == PageContents::Empty
                    || page_contents == PageContents::InsertsOnly
                {
                    done1(k.ptr());
                    return false;
                } else if LLIST_link(k.ptr()).opt().is_none() {
                    return true;
                } else if let TxtNode::Glue(_) = TxtNode::from(*LLIST_link(k.ptr()) as usize) {
                    if let Some(res) = with_penalty(input, k.ptr(), 0) {
                        return res;
                    }
                }
                page_so_far[1] += page_so_far[7] + k.width();
                page_so_far[7] = Scaled::ZERO;
                contribute(k.ptr());
                false
            }
            TxtNode::Penalty(p) => {
                if page_contents == PageContents::Empty
                    || page_contents == PageContents::InsertsOnly
                {
                    done1(p.ptr());
                    false
                } else {
                    if let Some(res) = with_penalty(input, p.ptr(), p.penalty()) {
                        res
                    } else {
                        contribute(p.ptr());
                        false
                    }
                }
            }
            TxtNode::Mark(m) => {
                contribute(m.ptr());
                false
            }
            TxtNode::Ins(p_ins) => {
                /*1043: "Append an insertion to the current page and goto contribute" */
                if page_contents == PageContents::Empty {
                    freeze_page_specs(PageContents::InsertsOnly);
                }

                let n = p_ins.box_reg();
                let mut r = PAGE_INS_HEAD;

                while n >= PageInsertion(*LLIST_link(r) as usize).box_reg() {
                    r = *LLIST_link(r) as usize;
                }

                let mut r_pins = PageInsertion(r);

                if r_pins.box_reg() != n {
                    /*1044: "Create a page insertion node with subtype(r) = n, and
                     * include the glue correction for box `n` in the current page
                     * state" */
                    let q = get_node(PAGE_INS_NODE_SIZE);
                    *LLIST_link(q) = *LLIST_link(r);
                    *LLIST_link(r) = Some(q).tex_int();

                    r_pins = PageInsertion(q);

                    r_pins.set_box_reg(n).set_subtype(PageInsType::Inserting);
                    ensure_vbox(n as _);

                    r_pins.set_height(if let Some(br) = BOX_REG(n as _).opt() {
                        let br = List::from(br);
                        br.height() + br.depth()
                    } else {
                        Scaled::ZERO
                    });

                    r_pins.set_best_ins_ptr(None.tex_int());
                    let q = *SKIP_REG(n as _) as usize;

                    let h = if *COUNT_REG(n as _) == 1000 {
                        r_pins.height()
                    } else {
                        x_over_n(r_pins.height(), 1000).0 * *COUNT_REG(n as _)
                    };

                    let mut q_spec = GlueSpec(q);
                    page_so_far[0] -= h + q_spec.size();
                    page_so_far[2 + q_spec.stretch_order() as usize] += q_spec.stretch();
                    page_so_far[6] += q_spec.shrink();

                    if q_spec.shrink_order() != GlueOrder::Normal && q_spec.shrink() != Scaled::ZERO
                    {
                        if file_line_error_style_p != 0 {
                            print_file_line();
                        } else {
                            print_nl_cstr("! ");
                        }
                        print_cstr("Infinite glue shrinkage inserted from ");
                        print_esc_cstr("skip");
                        print_int(n as i32);
                        help!(
                            "The correction glue for page breaking with insertions",
                            "must have finite shrinkability. But you may proceed,",
                            "since the offensive shrinkability has been made finite."
                        );
                        error();
                    }
                }

                if r_pins.subtype() == PageInsType::SplitUp {
                    insert_penalties += p_ins.float_cost()
                } else {
                    r_pins.set_last_ins_ptr(Some(p_ins.ptr()).tex_int());
                    let delta: Scaled =
                        page_so_far[0] - page_so_far[1] - page_so_far[7] + page_so_far[6];

                    let h = if *COUNT_REG(n as _) == 1000 {
                        p_ins.height()
                    } else {
                        x_over_n(p_ins.height(), 1000).0 * *COUNT_REG(n as _)
                    };

                    if (h <= Scaled::ZERO || h <= delta)
                        && p_ins.height() + r_pins.height() <= *SCALED_REG(n as _)
                    {
                        page_so_far[0] -= h;
                        r_pins.set_height(r_pins.height() + p_ins.height());
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
                        let mut w = if *COUNT_REG(n as _) <= 0 {
                            Scaled::MAX_HALFWORD
                        } else {
                            let mut w = page_so_far[0] - page_so_far[1] - page_so_far[7];
                            if *COUNT_REG(n as _) != 1000 {
                                w = x_over_n(w, *COUNT_REG(n as _)).0 * 1000;
                            }
                            w
                        };

                        w = w.min(*SCALED_REG(n as _) - r_pins.height());

                        let q = vert_break(p_ins.ins_ptr(), w, p_ins.depth());
                        r_pins.set_height(r_pins.height() + best_height_plus_depth);

                        if *COUNT_REG(n as _) != 1000 {
                            best_height_plus_depth =
                                x_over_n(best_height_plus_depth, 1000).0 * *COUNT_REG(n as _);
                        }
                        page_so_far[0] -= best_height_plus_depth;
                        r_pins
                            .set_subtype(PageInsType::SplitUp)
                            .set_broken_ptr(q)
                            .set_broken_ins(Some(p_ins.ptr()).tex_int());

                        if let Some(q) = q.opt() {
                            if let TxtNode::Penalty(q) = TxtNode::from(q) {
                                insert_penalties += q.penalty();
                            }
                        } else {
                            insert_penalties += EJECT_PENALTY;
                        }
                    }
                }
                contribute(p_ins.ptr());
                false
            }
            _ => confusion("page"),
        }
    }

    if llist_link(CONTRIB_HEAD).is_none() || output_active {
        return;
    }

    loop {
        if do_smth(input) {
            return;
        };
        if llist_link(CONTRIB_HEAD).is_none() {
            break;
        }
    }
    if NEST_PTR == 0 {
        cur_list.tail = CONTRIB_HEAD; /* "vertical mode" */
    } else {
        NEST[0].tail = CONTRIB_HEAD; /* "other modes" */
    };
    /* "other modes" */
}
