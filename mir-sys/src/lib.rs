#![expect(non_upper_case_globals, non_camel_case_types)]

#[rustfmt::skip]
mod bindings;

pub use bindings::*;

#[derive(Copy, Clone)]
#[repr(C, align(16))]
pub union MIR_imm_t {
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    // pub ld: f128,
}

pub type MIR_op_mode_t_u8 = u8;

#[derive(Copy, Clone)]
#[repr(C, align(16))]
#[cfg(target_endian = "little")]
pub struct MIR_op_t {
    pub data: *mut ::std::os::raw::c_void,
    pub mode: MIR_op_mode_t_u8,
    pub value_mode: MIR_op_mode_t_u8,
    pub u: MIR_op_t__u,
}

#[derive(Copy, Clone)]
#[repr(C, align(16))]
pub union MIR_op_t__u {
    pub reg: MIR_reg_t,
    pub var: MIR_reg_t,
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    // pub ld: f128,
    pub ref_: MIR_item_t,
    pub str_: MIR_str_t,
    pub mem: MIR_mem_t,
    pub var_mem: MIR_mem_t,
    pub label: MIR_label_t,
}

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub union MIR_val_t {
    pub ic: MIR_insn_code_t,
    pub a: *mut ::std::os::raw::c_void,
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    // pub ld: f128,
}

pub type MIR_type_t_u8 = u8;

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub struct MIR_data {
    pub name: *const ::std::os::raw::c_char,
    pub el_type: MIR_type_t_u8,
    pub nel: usize,
    __padding_0: u64,
    pub u: MIR_data__u,
}

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub union MIR_data__u {
    // pub d: f128,
    pub els: [u8; 1usize],
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_mem_t {
    pub type_: MIR_type_t_u8,
    pub scale: MIR_scale_t,
    pub alias: MIR_alias_t,
    pub nonalias: MIR_alias_t,
    pub nloc: u32,
    pub base: MIR_reg_t,
    pub index: MIR_reg_t,
    pub disp: MIR_disp_t,
}

pub type MIR_insn_code_t_u32 = u32;

#[repr(C)]
#[repr(align(16))]
pub struct MIR_insn {
    pub data: *mut ::std::os::raw::c_void,
    pub insn_link: DLIST_LINK_MIR_insn_t,
    pub code: MIR_insn_code_t_u32,
    pub nops: u32,
    pub ops: [MIR_op_t; 1usize],
}

#[allow(non_snake_case)]
#[inline]
pub fn MIR_init() -> MIR_context_t {
    if !cfg!(feature = "bundled") || cfg!(debug_assertions) {
        assert_eq!(MIR_API_VERSION, unsafe { _MIR_get_api_version() });
    }
    unsafe { _MIR_init() }
}
