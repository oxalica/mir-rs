#![cfg_attr(docsrs, feature(doc_auto_cfg))]
//! # [MIR project][mir] bindings for Rust
//!
//! This is the low-level binding that only exposes raw C types and functions,
//! and handles compilation and linking of C code.
//!
//! See [`mir-rs`][mir-rs] crate for high-level ergonomic APIs.
//!
//! [mir]: https://github.com/vnmakarov/mir
//! [mir-rs]: https://crates.io/crates/mir-rs
//!
//! ## Source
//!
//! The source code of mir C library is bundled in this package. Since upstream lacks some
//! API/features that are necessary for Rust integration, we use a forked version of mir.
//!
// NB: Sync with submodule and build.rs.
//! The fork can be seen at
//! [this repo](https://github.com/oxalica/mir/tree/1.0.0-rust.x),
//! and the current revision is based on upstream version
//! [v1.0.0](https://github.com/vnmakarov/mir/releases/tag/v1.0.0).
//!
//! ⚠️ The API/ABI is **NOT** compatible with upstream mir library.
//!
//! ## Features
//!
//! ⚠️  Warning: Currently, due to [lack of support of `bindgen`][bindgen-fncfg-issue],
//! extern functions may still be generated even with corresponding feature disabled.
//! Be careful to avoid using them when disabling features, or you may encounter link errors.
//!
//! - `default`: Implies `io`, `scan`, `interp`, `gen`.
//!
//! - `io`: De/serialization of MIR memory representation into/from bytes.
//!   If disabled, C macro `MIR_NO_IO` is set for compilation.
//!
//!   Guarded APIs:
//!   - `MIR_write{,_module}{,_with_func}`
//!   - `MIR_read{,_with_func}`
//!
//! - `scan`: Parsing of MIR textual representation.
//!   If disabled, C macro `MIR_NO_SCAN` is set for compilation.
//!
//!   Guarded API:
//!   - `MIR_scan_string`
//!
//! - `interp`: Enables MIR interpreter.
//!   If disabled, C macro `MIR_NO_INTERP` is set for compilation.
//!
//!   Guarded APIs:
//!   - `MIR_interp*`
//!
//! - `gen`: MIR native code generator.
//!   If disabled, `mir-gen.c` will not be compiled.
//!
//!   Guarded APIs:
//!   - `MIR_gen*`
//!   - `MIR_set_*_gen_interface`
//!
//! - `gen-debug`: Debug logging in MIR native code generator. It implies `gen`.
//!   If disabled, C macro `MIR_NO_GEN_DEBUG` is set for compilation.
//!
//!   Guarded APIs:
//!   - `MIR_gen_set_debug_{file,level}`
//!
//! - `assert`: Debug assertions.
//!   If disabled, C macro `NDEBUG` is set for compilation.
//!   It is implicitly enabled when `debug_assertions` is on (eg. in dev profile).
//!
//! [bindgen-fncfg-issue]: https://github.com/rust-lang/rust-bindgen/issues/2978
#![expect(non_upper_case_globals, non_camel_case_types)]

#[rustfmt::skip]
#[allow(clippy::unreadable_literal)]
mod bindings;

#[rustfmt::skip]
#[allow(clippy::wildcard_imports)]
#[cfg(feature = "gen")]
mod bindings_gen;

pub use bindings::*;

#[cfg(feature = "gen")]
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
