#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

use crate::xetex_scaledmath::Scaled;
const S_72_27: Scaled = Scaled(4736287);

use std::ffi::CString;
use std::io::Write;

use crate::node::{Kern, List, TxtNode, BOX_NODE_SIZE, MEDIUM_NODE_SIZE, RULE_NODE_SIZE};
use crate::xetex_consts::{IntPar, INTPAR};
use crate::xetex_ini::{
    cur_h, cur_v, input_state_t, job_name, rule_dp, rule_ht, rule_wd, synctex_enabled, MEM,
    TOTAL_PAGES,
};
use crate::xetex_io::name_of_input_file;
use crate::xetex_texmfmp::gettexstring;
use crate::xetex_xetexd::{SYNCTEX_line, SYNCTEX_tag};
use bridge::{ttstub_issue_error, ttstub_issue_warning, ttstub_output_close, ttstub_output_open};

use bridge::OutputHandleWrapper;
pub(crate) type str_number = i32;
bitflags::bitflags! {
    #[repr(C)]
    pub(crate) struct Flags: u32 {
        const CONTENT_READY = 0b00000001;
        const OFF = 0b00000010;
        const NOT_VOID = 0b00000100;
        const WARN = 0b00001000;
        const OUTPUT_P = 0b00010000;
    }
}
/* recorders know how to record a node */
/*  Here are all the local variables gathered in one "synchronization context"  */
#[repr(C)]
pub(crate) struct Context {
    pub(crate) file: Option<OutputHandleWrapper>,
    pub(crate) root_name: String,
    pub(crate) count: i32,
    pub(crate) node: usize,
    pub(crate) recorder: synctex_recorder_t,
    pub(crate) tag: i32,
    pub(crate) line: i32,
    pub(crate) curh: Scaled,
    pub(crate) curv: Scaled,
    pub(crate) magnification: i32,
    pub(crate) unit: i32,
    pub(crate) total_length: usize,
    pub(crate) lastv: Scaled,
    pub(crate) form_depth: i32,
    pub(crate) synctex_tag_counter: u32,
    pub(crate) flags: Flags,
}
/*  For non-GCC compilation.  */
/*  UNIT is the scale. TeX coordinates are very accurate and client won't need
 *  that, at leat in a first step.  1.0 <-> 2^16 = 65536.
 *  The TeX unit is sp (scaled point) or pt/65536 which means that the scale
 *  factor to retrieve a bp unit (a postscript) is 72/72.27/65536 =
 *  1/4096/16.06 = 1/8192/8.03
 *  Here we use 1/SYNCTEX_UNIT_FACTOR as scale factor, then we can limit ourselves to
 *  integers. This default value assumes that TeX magnification factor is 1000.
 *  The real TeX magnification factor is used to fine tune the synctex context
 *  scale in the synctex_dot_open function.
 *  IMPORTANT: We can say that the natural unit of .synctex files is SYNCTEX_UNIT_FACTOR sp.
 *  To retrieve the proper bp unit, we'll have to divide by 8.03.  To reduce
 *  rounding errors, we'll certainly have to add 0.5 for non negative integers
 *  and +/-0.5 for negative integers.  This trick is mainly to gain speed and
 *  size. A binary file would be more appropriate in that respect, but I guess
 *  that some clients like auctex would not like it very much.  we cannot use
 *  "<<13" instead of "/SYNCTEX_UNIT_FACTOR" because the integers are signed and we do not
 *  want the sign bit to be propagated.  The origin of the coordinates is at
 *  the top left corner of the page.  For pdf mode, it is straightforward, but
 *  for dvi mode, we'll have to record the 1in offset in both directions,
 *  eventually modified by the magnification.
 */
type synctex_recorder_t = Option<unsafe fn(_: usize) -> ()>;

const default_synctex_ctxt: Context = Context {
    file: None,
    root_name: String::new(),
    count: 0i32,
    node: 0,
    recorder: None,
    tag: 0i32,
    line: 0i32,
    curh: Scaled::ZERO,
    curv: Scaled::ZERO,
    magnification: 0i32,
    unit: 0i32,
    total_length: 0,
    lastv: Scaled(-1),
    form_depth: 0i32,
    synctex_tag_counter: 0_u32,
    flags: Flags::empty(),
};
static mut synctex_ctxt: Context = default_synctex_ctxt;

unsafe fn get_current_name() -> String {
    /* This used to always make the pathname absolute but I'm getting rid of
     * that since it ends up adding dependencies on a bunch of functions I
     * don't want to have to deal with. */
    name_of_input_file.clone()
}
/* synctex.h

Copyright (c) 2008, 2009 jerome DOT laurens AT u-bourgogne DOT fr

This file is part of the SyncTeX package.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE

Acknowledgments:
----------------
The author received useful remarks from the pdfTeX developers, especially Hahn The Thanh,
and significant help from XeTeX developer Jonathan Kew

Nota Bene:
----------
If you include or use a significant part of the synctex package into a software,
I would appreciate to be listed as contributor and see "SyncTeX" highlighted.

Version 1
Latest Revision: Wed Jul  1 08:17:50 UTC 2009

*/
/*  Send this message to init the synctex command value to the command line option.
 *  Sending this message too early will cause a bus error.  */
pub(crate) unsafe fn synctex_init_command() {
    /* In the web2c implementations this dealt with the -synctex command line
     * argument. */
    /* Reset state */
    synctex_ctxt = default_synctex_ctxt;
    if synctex_enabled != 0 {
        *INTPAR(IntPar::synctex) = 1i32
    } else {
        *INTPAR(IntPar::synctex) = 0i32
        /* \synctex=0 : don't record stuff */
    };
}
/*  Free all memory used, close the file if any,
 *  It is sent locally when there is a problem with synctex output.
 *  It is sent by pdftex when a fatal error occurred in pdftex.web. */
unsafe fn synctexabort() {
    if let Some(_file) = synctex_ctxt.file.as_mut() {
        ttstub_output_close(synctex_ctxt.file.take().unwrap());
    }
    synctex_ctxt.root_name = String::new();
    synctex_ctxt.flags.insert(Flags::OFF);
    /* disable synctex */
}
const synctex_suffix: &str = ".synctex";
const synctex_suffix_gz: &str = ".gz";
/*  synctex_dot_open ensures that the foo.synctex file is open.
 *  In case of problem, it definitely disables synchronization.
 *  Now all the output synchronization info is gathered in only one file.
 *  It is possible to split this info into as many different output files as sheets
 *  plus 1 for the control but the overall benefits are not so clear.
 *  For example foo-i.synctex would contain input synchronization
 *  information for page i alone.
 */
unsafe fn synctex_dot_open() -> bool {
    if synctex_ctxt.flags.contains(Flags::OFF) || *INTPAR(IntPar::synctex) == 0 {
        return false;
    }
    if synctex_ctxt.file.is_some() {
        return true;
    }
    let mut tmp = gettexstring(job_name);
    if !tmp.is_empty() {
        tmp += synctex_suffix;
        tmp += synctex_suffix_gz;
        let the_name = CString::new(tmp.as_str()).unwrap();
        synctex_ctxt.file = ttstub_output_open(the_name.as_ptr(), 1i32);
        if synctex_ctxt.file.is_some() {
            if !(synctex_record_preamble() != 0) {
                synctex_ctxt.magnification = 1000i32;
                synctex_ctxt.unit = 1i32;
                if !synctex_ctxt.root_name.is_empty() {
                    synctex_record_input(1i32, &synctex_ctxt.root_name);
                    synctex_ctxt.root_name = String::new();
                }
                synctex_ctxt.count = 0i32;
                return true;
            }
        }
    }
    /*printf("\nSyncTeX warning: no synchronization, problem with %s\n", the_name);*/
    synctexabort();
    false
}
/* *
 *  synctex_record_settings must be called very late,
 *  only once there is an opportunity to know whether
 *  in pdf or dvi mode.
 */
unsafe fn synctex_prepare_content() -> bool {
    if synctex_ctxt.flags.contains(Flags::CONTENT_READY) {
        return synctex_ctxt.file.is_some();
    }
    if synctex_dot_open() && 0i32 == synctex_record_settings() && 0i32 == synctex_record_content() {
        synctex_ctxt.flags.insert(Flags::CONTENT_READY);
        return synctex_ctxt.file.is_some();
    }
    synctexabort();
    false
}
/*  Send this message when starting a new input.  */
/*  Each time TeX opens a file, it sends a synctexstartinput message and enters
 *  this function.  Here, a new synchronization tag is created and stored in
 *  the synctex_tag of the TeX current input context.  Each synchronized
 *  TeX node will record this tag instead of the file name.  synctexstartinput
 *  writes the mapping synctag <-> file name to the .synctex (or .synctex.gz) file.  A client
 *  will read the .synctex file and retrieve this mapping, it will be able to
 *  open the correct file just knowing its tag.  If the same file is read
 *  multiple times, it might be associated to different tags.  Synchronization
 *  controllers, either in viewers, editors or standalone should be prepared to
 *  handle this situation and take the appropriate action if they want to
 *  optimize memory.  No two different files will have the same positive tag.
 *  It is not advisable to definitely store the file names here.  If the file
 *  names ever have to be stored, it should definitely be done at the TeX level
 *  just like src-specials do, such that other components of the program can use
 *  it.  This function does not make any difference between the files, it
 *  treats the same way .tex, .aux, .sty ... files, even if many of them do not
 *  contain any material meant to be typeset.
 */
pub(crate) unsafe fn synctex_start_input(input: &mut input_state_t) {
    if synctex_ctxt.flags.contains(Flags::OFF) {
        return;
    }
    /*  synctex_tag_counter is a counter uniquely identifying the file actually
     *  open.  Each time tex opens a new file, synctexstartinput will increment this
     *  counter  */
    if !synctex_ctxt.synctex_tag_counter > 0_u32 {
        synctex_ctxt.synctex_tag_counter = synctex_ctxt.synctex_tag_counter.wrapping_add(1)
    } else {
        /*  we have reached the limit, subsequent files will be softly ignored
         *  this makes a lot of files... even in 32 bits
         *  Maybe we will limit this to 16bits and
         *  use the 16 other bits to store the column number */
        synctex_ctxt.synctex_tag_counter = 0_u32;
        /* was this, but this looks like a bug */
        /* input.synctex_tag = 0; */
        return;
    } /*  -> *TeX.web  */
    input.synctex_tag = synctex_ctxt.synctex_tag_counter as i32;
    if synctex_ctxt.synctex_tag_counter == 1_u32 {
        /*  this is the first file TeX ever opens, in general \jobname.tex we
         *  do not know yet if synchronization will ever be enabled so we have
         *  to store the file name, because we will need it later.
         *  This is necessary because \jobname can be different */
        synctex_ctxt.root_name = get_current_name();
        if synctex_ctxt.root_name.is_empty() {
            synctex_ctxt.root_name = String::from("texput");
        }
        return;
    }
    if synctex_ctxt.file.is_some() || synctex_dot_open() {
        let tmp = get_current_name();
        /* Always record the input, even if INTPAR(synctex) is 0 */
        synctex_record_input(input.synctex_tag, &tmp);
    };
}
/*  Send this message to clean memory, and close the file.  */
/*  All the synctex... functions below have the smallest set of parameters.  It
 *  appears to be either the address of a node, or nothing at all.  Using mem,
 *  which is the place where all the nodes are stored, one can retrieve every
 *  information about a node.  The other information is obtained through the
 *  global context variable.
 */
/*  Free all memory used and close the file,
 *  sent by close_files_and_terminate in tex.web.
 *  synctexterminate() is called when the TeX run terminates.
 */
pub(crate) unsafe fn synctex_terminate(mut _log_opened: bool) {
    if let Some(_file) = synctex_ctxt.file.as_mut() {
        /* We keep the file even if no tex output is produced
         * (synctex_ctxt.flags.not_void == 0). I assume that this means that there
         * was an error and tectonic will not save anything anyway. */
        synctex_record_postamble();
        ttstub_output_close(synctex_ctxt.file.take().unwrap());
    }
    synctexabort();
}
/*  Recording the "{..." line.  In *tex.web, use synctex_sheet(pdf_output) at
 *  the very beginning of the ship_out procedure.
*/
/*  Recording the "{..." line.  In *tex.web, use synctex_sheet(pdf_output) at
 *  the very beginning of the ship_out procedure.
 */
pub(crate) unsafe fn synctex_sheet(mut mag: i32) {
    if synctex_ctxt.flags.contains(Flags::OFF) {
        if *INTPAR(IntPar::synctex) != 0 && !synctex_ctxt.flags.contains(Flags::WARN) {
            synctex_ctxt.flags.insert(Flags::WARN);
            ttstub_issue_warning(
                "SyncTeX was disabled -- changing the value of \\synctex has no effect",
            );
        }
        return;
    }
    if synctex_prepare_content() {
        /*  First possibility: the .synctex file is already open because SyncTeX was activated on the CLI
         *  or it was activated with the \synctex macro and the first page is already shipped out.
         *  Second possibility: tries to open the .synctex, useful if synchronization was enabled
         *  from the source file and not from the CLI. */
        if TOTAL_PAGES == 0 {
            /*  Now it is time to properly set up the scale factor. */
            if mag > 0i32 {
                synctex_ctxt.magnification = mag
            }
        }
        synctex_record_sheet(TOTAL_PAGES + 1);
    };
}
/*  Recording the "}..." line.  In *tex.web, use synctex_teehs at
 *  the very end of the ship_out procedure.
*/
/*  Recording the "}..." line.  In *tex.web, use synctex_teehs at
 *  the very end of the ship_out procedure.
 */
pub(crate) unsafe fn synctex_teehs() {
    if synctex_ctxt.flags.contains(Flags::OFF) || synctex_ctxt.file.is_none() {
        return;
    } /* not TOTAL_PAGES+1*/
    synctex_record_teehs(TOTAL_PAGES);
}
/*  This message is sent when a vlist will be shipped out, more precisely at
 *  the beginning of the vlist_out procedure in *TeX.web.  It will be balanced
 *  by a synctex_tsilv, sent at the end of the vlist_out procedure.  p is the
 *  address of the vlist We assume that p is really a vlist node! */
/*  When an hlist ships out, it can contain many different kern/glue nodes with
 *  exactly the same sync tag and line.  To reduce the size of the .synctex
 *  file, we only display a kern node sync info when either the sync tag or the
 *  line changes.  Also, we try ro reduce the distance between the chosen nodes
 *  in order to improve accuracy.  It means that we display information for
 *  consecutive nodes, as far as possible.  This tricky part uses a "recorder",
 *  which is the address of the routine that knows how to write the
 *  synchronization info to the .synctex file.  It also uses criteria to detect
 *  a change in the context, this is the macro SYNCTEX_???_CONTEXT_DID_CHANGE. The
 *  SYNCTEX_IGNORE macro is used to detect unproperly initialized nodes.  See
 *  details in the implementation of the functions below.  */
/*  This message is sent when a vlist will be shipped out, more precisely at
 *  the beginning of the vlist_out procedure in *TeX.web.  It will be balanced
 *  by a synctex_tsilv, sent at the end of the vlist_out procedure.  p is the
 *  address of the vlist. We assume that p is really a vlist node! */
pub(crate) unsafe fn synctex_vlist(this_box: &List) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    } /*  0 to reset  */
    synctex_ctxt.node = this_box.ptr(); /*  reset  */
    synctex_ctxt.recorder = None;
    synctex_ctxt.tag = *SYNCTEX_tag(this_box.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.line = *SYNCTEX_line(this_box.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_record_node_vlist(this_box);
}
/*  Recording a "}" line ending a vbox: this message is sent whenever a vlist
 *  has been shipped out. It is used to close the vlist nesting level. It is
 *  sent at the end of each vlist_out procedure in *TeX.web to balance a former
 *  synctex_vlist sent at the beginning of that procedure.    */
/*  Recording a "f" line ending a vbox: this message is sent whenever a vlist
 *  has been shipped out. It is used to close the vlist nesting level. It is
 *  sent at the end of the vlist_out procedure in *TeX.web to balance a former
 *  synctex_vlist sent at the beginning of that procedure.    */
pub(crate) unsafe fn synctex_tsilv(this_box: usize) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    }
    /*  Ignoring any pending info to be recorded  */
    synctex_ctxt.node = this_box; /*  0 to reset  */
    synctex_ctxt.tag = *SYNCTEX_tag(this_box, BOX_NODE_SIZE);
    synctex_ctxt.line = *SYNCTEX_line(this_box, BOX_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    synctex_record_node_tsilv(this_box);
}
/*  This message is sent when a void vlist will be shipped out.
 *  There is no need to balance a void vlist.  */
/*  This message is sent when a void vlist will be shipped out.
 *  There is no need to balance a void vlist.  */
pub(crate) unsafe fn synctex_void_vlist(p: &List, mut _this_box: &List) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    } /*  reset  */
    synctex_ctxt.node = p.ptr(); /*  reset  */
    synctex_ctxt.tag = *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.line = *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    synctex_record_node_void_vlist(p);
}
/*  Send this message when an hlist will be shipped out, more precisely at
 *  the beginning of the hlist_out procedure in *TeX.web.  It must be balanced
 *  by a synctex_tsilh, sent at the end of the hlist_out procedure.  p is the
 *  address of the hlist. */
/*  This message is sent when an hlist will be shipped out, more precisely at
 *  the beginning of the hlist_out procedure in *TeX.web.  It will be balanced
 *  by a synctex_tsilh, sent at the end of the hlist_out procedure.  p is the
 *  address of the hlist We assume that p is really an hlist node! */
pub(crate) unsafe fn synctex_hlist(this_box: &List) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    } /*  0 to reset  */
    synctex_ctxt.node = this_box.ptr(); /*  reset  */
    synctex_ctxt.tag = *SYNCTEX_tag(this_box.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.line = *SYNCTEX_line(this_box.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    synctex_record_node_hlist(this_box);
}
/*  Send this message at the end of the various hlist_out procedure in *TeX.web
 *  to balance a former synctex_hlist.    */
/*  Recording a ")" line ending an hbox this message is sent whenever an hlist
 *  has been shipped out it is used to close the hlist nesting level. It is
 *  sent at the end of the hlist_out procedure in *TeX.web to balance a former
 *  synctex_hlist sent at the beginning of that procedure.    */
pub(crate) unsafe fn synctex_tsilh(this_box: usize) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    }
    /*  Ignoring any pending info to be recorded  */
    synctex_ctxt.node = this_box; /*  0 to force next node to be recorded!  */
    synctex_ctxt.tag = *SYNCTEX_tag(this_box, BOX_NODE_SIZE); /*  reset  */
    synctex_ctxt.line = *SYNCTEX_line(this_box, BOX_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    synctex_record_node_tsilh(this_box);
}
/*  This message is sent when a void hlist will be shipped out.
 *  There is no need to balance a void hlist.  */
/*  This message is sent when a void hlist will be shipped out.
 *  There is no need to balance a void hlist.  */
pub(crate) unsafe fn synctex_void_hlist(p: &List, _this_box: &List) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    }
    /*  the sync context has changed  */
    if synctex_ctxt.recorder.is_some() {
        /*  but was not yet recorded  */
        synctex_ctxt.recorder.expect("non-null function pointer")(synctex_ctxt.node);
        /*  0 to reset  */
    } /*  reset  */
    synctex_ctxt.node = p.ptr();
    synctex_ctxt.tag = *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.line = *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    synctex_record_node_void_hlist(p);
}
/*  Send this message whenever an inline math node will ship out. */
/*  This macro will detect a change in the synchronization context.  As long as
 *  the synchronization context remains the same, there is no need to write
 *  synchronization info: it would not help more.  The synchronization context
 *  has changed when either the line number or the file tag has changed.  */
/*  glue code, this message is sent whenever an inline math node will ship out
See: @ @<Output the non-|char_node| |p| for...  */
pub(crate) unsafe fn synctex_math(p: usize, mut _this_box: usize) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    }
    if synctex_ctxt.recorder.is_some()
        && (0 == synctex_ctxt.node
            || *SYNCTEX_tag(p, MEDIUM_NODE_SIZE) != synctex_ctxt.tag
            || *SYNCTEX_line(p, MEDIUM_NODE_SIZE) != synctex_ctxt.line)
    {
        /*  the sync context did change  */
        synctex_ctxt.recorder.expect("non-null function pointer")(synctex_ctxt.node);
        /*  no need to record once more  */
    }
    synctex_ctxt.node = p;
    synctex_ctxt.tag = *SYNCTEX_tag(p, MEDIUM_NODE_SIZE);
    synctex_ctxt.line = *SYNCTEX_line(p, MEDIUM_NODE_SIZE);
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    synctex_record_node_math(p);
    /*  always record synchronously  */
}
/*  Send this message whenever an horizontal rule or glue node will ship out. */
/*  this message is sent whenever an horizontal glue node or rule node ships out
See: move_past:...    */
pub(crate) unsafe fn synctex_horizontal_rule_or_glue(p: usize, mut _this_box: usize) {
    match TxtNode::from(p) {
        TxtNode::Rule(r) => {
            if synctex_ctxt.flags.contains(Flags::OFF)
                || *INTPAR(IntPar::synctex) == 0
                || 0i32 >= *SYNCTEX_tag(r.ptr(), RULE_NODE_SIZE)
                || 0i32 >= *SYNCTEX_line(r.ptr(), RULE_NODE_SIZE)
            {
                return;
            }
        }
        TxtNode::Glue(g) => {
            if synctex_ctxt.flags.contains(Flags::OFF)
                || *INTPAR(IntPar::synctex) == 0
                || 0i32 >= *SYNCTEX_tag(g.ptr(), MEDIUM_NODE_SIZE)
                || 0i32 >= *SYNCTEX_line(g.ptr(), MEDIUM_NODE_SIZE)
            {
                return;
            }
        }
        TxtNode::Kern(k) => {
            if synctex_ctxt.flags.contains(Flags::OFF)
                || *INTPAR(IntPar::synctex) == 0
                || 0i32 >= *SYNCTEX_tag(k.ptr(), MEDIUM_NODE_SIZE)
                || 0i32 >= *SYNCTEX_line(k.ptr(), MEDIUM_NODE_SIZE)
            {
                return;
            }
        }
        _ => {
            ttstub_issue_error(&format!(
                "unknown node type {} in SyncTeX",
                MEM[p].b16.s1 as i32
            )); /*  always record synchronously: maybe some text is outside the box  */
        }
    } /*  always record synchronously: maybe some text is outside the box  */
    synctex_ctxt.node = p; /*  always record synchronously: maybe some text is outside the box  */
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    synctex_ctxt.recorder = None;
    match TxtNode::from(p) {
        TxtNode::Rule(r) => {
            synctex_ctxt.tag = *SYNCTEX_tag(r.ptr(), RULE_NODE_SIZE);
            synctex_ctxt.line = *SYNCTEX_line(r.ptr(), RULE_NODE_SIZE);
            synctex_record_node_rule(r.ptr());
        }
        TxtNode::Glue(g) => {
            synctex_ctxt.tag = *SYNCTEX_tag(g.ptr(), MEDIUM_NODE_SIZE);
            synctex_ctxt.line = *SYNCTEX_line(g.ptr(), MEDIUM_NODE_SIZE);
            synctex_record_node_glue(g.ptr());
        }
        TxtNode::Kern(k) => {
            synctex_ctxt.tag = *SYNCTEX_tag(k.ptr(), MEDIUM_NODE_SIZE);
            synctex_ctxt.line = *SYNCTEX_line(k.ptr(), MEDIUM_NODE_SIZE);
            synctex_record_node_kern(k.ptr());
        }
        _ => {
            ttstub_issue_error(&format!(
                "unknown node type {} in SyncTeX",
                MEM[p].b16.s1 as i32
            ));
        }
    };
}
/*  Send this message whenever a kern node will ship out. */
/*  this message is sent whenever a kern node ships out
See: @ @<Output the non-|char_node| |p| for...    */
pub(crate) unsafe fn synctex_kern(p: usize, this_box: usize) {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || 0i32 >= *SYNCTEX_tag(p, MEDIUM_NODE_SIZE)
        || 0i32 >= *SYNCTEX_line(p, MEDIUM_NODE_SIZE)
    {
        return;
    }
    if 0 == synctex_ctxt.node
        || *SYNCTEX_tag(p, MEDIUM_NODE_SIZE) != synctex_ctxt.tag
        || *SYNCTEX_line(p, MEDIUM_NODE_SIZE) != synctex_ctxt.line
    {
        /*  the sync context has changed  */
        if synctex_ctxt.recorder.is_some() {
            /*  but was not yet recorded  */
            synctex_ctxt.recorder.expect("non-null function pointer")(synctex_ctxt.node);
        }
        if synctex_ctxt.node == this_box {
            /* first node in the list */
            synctex_ctxt.node = p;
            synctex_ctxt.tag = *SYNCTEX_tag(p, MEDIUM_NODE_SIZE);
            synctex_ctxt.line = *SYNCTEX_line(p, MEDIUM_NODE_SIZE);
            synctex_ctxt.recorder = Some(synctex_record_node_kern as unsafe fn(_: usize) -> ())
        } else {
            synctex_ctxt.node = p;
            synctex_ctxt.tag = *SYNCTEX_tag(p, MEDIUM_NODE_SIZE);
            synctex_ctxt.line = *SYNCTEX_line(p, MEDIUM_NODE_SIZE);
            synctex_ctxt.recorder = None;
            /*  always record when the context has just changed
             *  and when not the first node  */
            synctex_record_node_kern(p);
        }
    } else {
        /*  just update the geometry and type (for future improvements)  */
        synctex_ctxt.node = p;
        synctex_ctxt.tag = *SYNCTEX_tag(p, MEDIUM_NODE_SIZE);
        synctex_ctxt.line = *SYNCTEX_line(p, MEDIUM_NODE_SIZE);
        synctex_ctxt.recorder = Some(synctex_record_node_kern as unsafe fn(_: usize) -> ())
    };
}
/*  For debugging purpose only    */
/*  this message should be sent to record information
synchronously for the current location    */
pub(crate) unsafe fn synctex_current() {
    /* magic pt/in conversion */
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return;
    } /* XXX: should this be `+=`? */
    let s = format!(
        "x{},{}:{},{}\n",
        synctex_ctxt.tag,
        synctex_ctxt.line,
        (cur_h + S_72_27).0 / synctex_ctxt.unit,
        (cur_v + S_72_27).0 / synctex_ctxt.unit,
    ); /* XXX: should this be `+=`? */
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_settings() -> i32 {
    if synctex_ctxt.file.is_none() {
        return 0i32;
    }
    let s = format!(
        "Output:pdf\nMagnification:{}\nUnit:{}\nX Offset:0\nY Offset:0\n",
        synctex_ctxt.magnification, synctex_ctxt.unit,
    );
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        return 0i32;
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_preamble() -> i32 {
    let s = format!("SyncTeX Version:{}\n", 1,);
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length = len;
        return 0i32;
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_input(mut tag: i32, name: &str) -> i32 {
    let s = format!("Input:{}:{}\n", tag, name);
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        return 0i32;
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_anchor() -> i32 {
    let s = format!("!{}\n", synctex_ctxt.total_length,);
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length = len;
        synctex_ctxt.count += 1;
        return 0i32;
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_content() -> i32 {
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(b"Content:\n") {
        synctex_ctxt.total_length += len;
        return 0i32;
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_sheet(mut sheet: usize) -> i32 {
    if 0i32 == synctex_record_anchor() {
        let s = format!("{{{}\n", sheet);
        if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
            synctex_ctxt.total_length += len;
            synctex_ctxt.count += 1;
            return 0i32;
        }
    }
    synctexabort();
    -1i32
}
/*  Recording a "}..." or a ">" line  */
#[inline]
unsafe fn synctex_record_teehs(mut sheet: usize) -> i32 {
    if 0i32 == synctex_record_anchor() {
        let s = format!("}}{}\n", sheet);
        if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
            synctex_ctxt.total_length += len;
            synctex_ctxt.count += 1;
            return 0i32;
        }
    }
    synctexabort();
    -1i32
}
/*  Recording the "<..." line.  In pdftex.web, use synctex_pdfxform(p) at
 *  the very beginning of the pdf_ship_out procedure.
 */
pub(crate) unsafe fn synctex_pdfxform(mut p: i32) {
    if synctex_ctxt.flags.contains(Flags::OFF) {
        if *INTPAR(IntPar::synctex) != 0 && !synctex_ctxt.flags.contains(Flags::WARN) {
            synctex_ctxt.flags.insert(Flags::WARN);
            ttstub_issue_warning(
                "SyncTeX was disabled - changing the value of \\synctex has no effect",
            );
        }
        return;
    }
    if synctex_prepare_content() {
        synctex_record_pdfxform(p);
    };
}
/*  Recording the ">" line.  In pdftex.web, use synctex_mrofxfdp at
 *  the very end of the ship_out procedure.
 */
pub(crate) unsafe fn synctex_mrofxfdp() {
    if !synctex_ctxt.file.is_none() {
        synctex_record_mrofxfdp();
    };
}
pub(crate) unsafe fn synctex_pdfrefxform(mut objnum: i32) {
    if !synctex_ctxt.file.is_none() {
        synctex_record_node_pdfrefxform(objnum);
    };
}
/*  Recording a "<..." line  */
#[inline]
unsafe fn synctex_record_pdfxform(mut _form: i32) -> i32 {
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return 0i32;
    } else {
        /* XXX Tectonic: guessing that SYNCTEX_PDF_CUR_FORM = synctex_ctxt.form_depth here */
        synctex_ctxt.form_depth += 1;
        let s = format!("<{}\n", synctex_ctxt.form_depth,);
        if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
            synctex_ctxt.total_length += len;
            synctex_ctxt.count += 1;
            return 0i32;
        }
    }
    synctexabort();
    -1i32
}
/*  Recording a ">" line  */
#[inline]
unsafe fn synctex_record_mrofxfdp() -> i32 {
    if 0i32 == synctex_record_anchor() {
        /* XXX Tectonic: mistake here in original source, no %d in format string */
        synctex_ctxt.form_depth -= 1;
        if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(b">\n") {
            synctex_ctxt.total_length += len;
            synctex_ctxt.count += 1;
            return 0i32;
        }
    }
    synctexabort();
    -1i32
}
/*  Recording a "f..." line  */
#[inline]
unsafe fn synctex_record_node_pdfrefxform(mut objnum: i32) -> i32
/* UNUSED form JL */ {
    synctex_ctxt.curh = cur_h + S_72_27;
    synctex_ctxt.curv = cur_v + S_72_27;
    if synctex_ctxt.flags.contains(Flags::OFF)
        || *INTPAR(IntPar::synctex) == 0
        || synctex_ctxt.file.is_none()
    {
        return 0i32;
    } else {
        let s = format!(
            "f{}:{},{}\n",
            objnum,
            (cur_h + S_72_27).0 / synctex_ctxt.unit,
            (cur_v + S_72_27).0 / synctex_ctxt.unit,
        );
        synctex_ctxt.lastv = cur_v + S_72_27;
        if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
            synctex_ctxt.total_length += len;
            synctex_ctxt.count += 1;
            return 0i32;
        }
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_node_void_vlist(p: &List) {
    let s = format!(
        "v{},{}:{},{}:{},{},{}\n",
        *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE),
        *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
        p.width().0 / synctex_ctxt.unit,
        p.height().0 / synctex_ctxt.unit,
        p.depth().0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_vlist(p: &List) {
    synctex_ctxt.flags.insert(Flags::NOT_VOID);
    let s = format!(
        "[{},{}:{},{}:{},{},{}\n",
        *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE),
        *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
        p.width().0 / synctex_ctxt.unit,
        p.height().0 / synctex_ctxt.unit,
        p.depth().0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_tsilv(_p: usize) {
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(b"]\n") {
        synctex_ctxt.total_length += len
    /* is it correct that synctex_ctxt.count is not incremented here? */
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_void_hlist(p: &List) {
    let s = format!(
        "h{},{}:{},{}:{},{},{}\n",
        *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE),
        *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
        p.width().0 / synctex_ctxt.unit,
        p.height().0 / synctex_ctxt.unit,
        p.depth().0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_hlist(p: &List) {
    synctex_ctxt.flags.insert(Flags::NOT_VOID);
    let s = format!(
        "({},{}:{},{}:{},{},{}\n",
        *SYNCTEX_tag(p.ptr(), BOX_NODE_SIZE),
        *SYNCTEX_line(p.ptr(), BOX_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
        p.width().0 / synctex_ctxt.unit,
        p.height().0 / synctex_ctxt.unit,
        p.depth().0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_tsilh(_p: usize) {
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(b")\n") {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_count() -> i32 {
    let s = format!("Count:{}\n", synctex_ctxt.count,);
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        return 0i32;
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_postamble() -> i32 {
    if 0i32 == synctex_record_anchor() {
        if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(b"Postamble:\n") {
            synctex_ctxt.total_length += len;
            if synctex_record_count() == 0 && synctex_record_anchor() == 0 {
                if let Ok(len) = synctex_ctxt
                    .file
                    .as_mut()
                    .unwrap()
                    .write(b"Post scriptum:\n")
                {
                    synctex_ctxt.total_length += len;
                    return 0i32;
                }
            }
        }
    }
    synctexabort();
    -1i32
}
#[inline]
unsafe fn synctex_record_node_glue(p: usize) {
    let s = format!(
        "g{},{}:{},{}\n",
        *SYNCTEX_tag(p, MEDIUM_NODE_SIZE),
        *SYNCTEX_line(p, MEDIUM_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_kern(p: usize) {
    let s = format!(
        "k{},{}:{},{}:{}\n",
        *SYNCTEX_tag(p, MEDIUM_NODE_SIZE),
        *SYNCTEX_line(p, MEDIUM_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
        Kern(p).width().0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
#[inline]
unsafe fn synctex_record_node_rule(p: usize) {
    let s = format!(
        "r{},{}:{},{}:{},{},{}\n",
        *SYNCTEX_tag(p, RULE_NODE_SIZE),
        *SYNCTEX_line(p, RULE_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit,
        rule_wd.0 / synctex_ctxt.unit,
        rule_ht.0 / synctex_ctxt.unit,
        rule_dp.0 / synctex_ctxt.unit,
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}
unsafe fn synctex_record_node_math(p: usize) {
    let s = format!(
        "${},{}:{},{}\n",
        *SYNCTEX_tag(p, MEDIUM_NODE_SIZE),
        *SYNCTEX_line(p, MEDIUM_NODE_SIZE),
        synctex_ctxt.curh.0 / synctex_ctxt.unit,
        synctex_ctxt.curv.0 / synctex_ctxt.unit
    );
    synctex_ctxt.lastv = cur_v + S_72_27;
    if let Ok(len) = synctex_ctxt.file.as_mut().unwrap().write(s.as_bytes()) {
        synctex_ctxt.total_length += len;
        synctex_ctxt.count += 1
    } else {
        synctexabort();
    };
}

/*
License:
--------
Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE

Except as contained in this notice, the name of the copyright holder
shall not be used in advertising or otherwise to promote the sale,
use or other dealings in this Software without prior written
authorization from the copyright holder.

Important notice:
-----------------
This file is named "synctex.c", it may or may not have a header counterpart
depending on its use.  It aims to provide basic components useful for the
input/output synchronization technology for TeX.
The purpose of the implementation is threefold
- firstly, it defines a new input/output synchronization technology named
"synchronize texnology", "SyncTeX" or "synctex"
- secondly, it defines the naming convention and format of the auxiliary file
used by this technology
- thirdly, it defines the API of a controller and a controller, used in
particular by the pdfTeX and XeTeX programs to prepare synchronization.

All these are up to a great extent de facto definitions, which means that they
are partly defined by the implementation itself.

This technology was first designed for pdfTeX, an extension of TeX managing the
pdf output file format, but it can certainly be adapted to other programs built
from TeX as long as the extensions do not break too much the core design.
Moreover, the synchronize texnology only relies on code concept and not
implementation details, so it can be ported to other TeX systems.  In order to
support SyncTeX, one can start reading the dedicated section in synctex.ch,
sync-pdftex.ch and sync-xetex.ch. Actually, support is provided for TeX, e-TeX,
pdfTeX and XeTeX.

Other existing public synchronization technologies are defined by srcltx.sty -
also used by source specials - and pdfsync.sty.  Like them, the synchronize
texnology is meant to be shared by various text editors, viewers and TeX
engines.  A centralized reference and source of information is available in TeX-Live.

Versioning:
-----------
As synctex is embedded into different TeX implementation, there is an independent
versionning system.
For TeX implementations, the actual version is: 3
For .synctex file format, the actual version is SYNCTEX_VERSION below

Please, do not remove these explanations.

Acknowledgments:
----------------
The author received useful remarks from the pdfTeX developers, especially Hahn The Thanh,
and significant help from XeTeX developer Jonathan Kew

Nota Bene:
----------
If you include or use a significant part of the synctex package into a software,
I would appreciate to be listed as contributor and see "SyncTeX" highlighted.

History:
--------
Version 1.14
Fri Apr 15 19:10:57 UTC 2011
- taking output_directory into account
- Replaced FOPEN_WBIN_MODE by FOPEN_W_MODE when opening the text version of the .synctex file.
- Merging with LuaTeX's version of synctex.c

Version 3
- very minor design change to take luatex into account
- typo fixed
- some size_t replaced by int
- very minor code design change to remove wrong xetex specific warnings

Version 2
Fri Sep 19 14:55:31 UTC 2008
- support for file names containing spaces.
This is one thing that xetex and pdftex do not manage the same way.
When the input file name contains a space character ' ',
pdftex will automatically enclose this name between two quote characters '"',
making programs believe that these quotes are really part of the name.
xetex does nothing special.
For that reason, running the command line
xetex --synctex=-1 "my file.tex"
is producing the expected file named <my file.synctex>, (the '<' and '>' are not part of the name)
whereas running the command line
pdftex --synctex=-1 "my file.tex"
was producing the unexpected file named <"my file".synctex> where the two '"' characters were part of the name.
Of course, that was breaking the typesetting mechanism when pdftex was involved.
To solve this problem, we prefer to rely on the output_file_name instead of the jobname.
In the case when no output_file_name is available, we use jobname and test if the file name
starts and ends with a quote character. Every synctex output file is removed because we consider
TeX encontered a problem.
There is some conditional coding.

*/
/* deleted because unused in Tectonic:
    synctex_record_node_char (AKA synctex_node_recorder),
    synctex_record_node_unknown (AKA synctex_node_recorder),
*/
