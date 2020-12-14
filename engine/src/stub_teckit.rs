/*------------------------------------------------------------------------
Copyright (C) 2002-2014 SIL International. All rights reserved.

Distributable under the terms of either the Common Public License or the
GNU Lesser General Public License, as specified in the LICENSING.txt file.

File: TECkit_Engine.h
Responsibility: Jonathan Kew
Last reviewed: Not yet.

Description:
    Public API to the TECkit conversion engine.
-------------------------------------------------------------------------*/
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Opaque_TECkit_Converter {
    _unused: [u8; 0],
}

extern "C" {
    pub(crate) fn TECkit_CreateConverter(
        mapping: *mut u8,
        mappingSize: u32,
        mapForward: u8,
        sourceForm: u16,
        targetForm: u16,
        converter: *mut TECkit_Converter,
    ) -> TECkit_Status;
    pub(crate) fn TECkit_ConvertBuffer(
        converter: TECkit_Converter,
        inBuffer: *const u8,
        inLength: u32,
        inUsed: *mut u32,
        outBuffer: *mut u8,
        outLength: u32,
        outUsed: *mut u32,
        inputIsComplete: u8,
    ) -> TECkit_Status;
}

pub(crate) type TECkit_Status = i64;

pub(crate) type TECkit_Converter = *mut Opaque_TECkit_Converter;
