#![expect(non_upper_case_globals, non_camel_case_types)]

#[rustfmt::skip]
#[expect(clippy::unreadable_literal)]
mod bindings;

#[rustfmt::skip]
#[expect(clippy::wildcard_imports)]
mod bindings_gen;

pub use bindings::*;

pub use bindings_gen::*;

pub type MIR_op_mode_t_u8 = u8;

#[derive(Copy, Clone)]
#[repr(C)]
#[cfg(target_endian = "little")]
pub struct MIR_op_t {
    pub data: *mut ::std::os::raw::c_void,
    pub mode: MIR_op_mode_t_u8,
    pub value_mode: MIR_op_mode_t_u8,
    pub u: MIR_op_t__u,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union MIR_op_t__u {
    pub reg: MIR_reg_t,
    pub var: MIR_reg_t,
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    pub ld: f64,
    pub ref_: MIR_item_t,
    pub str_: MIR_str_t,
    pub mem: MIR_mem_t,
    pub var_mem: MIR_mem_t,
    pub label: MIR_label_t,
}

pub type MIR_type_t_u8 = u8;

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
pub struct MIR_insn {
    pub data: *mut ::std::os::raw::c_void,
    pub insn_link: DLIST_LINK_MIR_insn_t,
    pub code: MIR_insn_code_t_u32,
    pub nops: u32,
    pub ops: [MIR_op_t; 1usize],
}

#[expect(non_snake_case)]
#[expect(clippy::used_underscore_items)]
#[expect(clippy::float_cmp)]
#[inline]
#[must_use]
pub fn MIR_init() -> MIR_context_t {
    debug_assert_eq!(MIR_API_VERSION, unsafe { _MIR_get_api_version() });
    unsafe { _MIR_init() }
}
