#![allow(non_camel_case_types, non_upper_case_globals)]
use libc::{c_char, c_int, c_uint, c_void, size_t, FILE};

#[repr(C)]
#[repr(align(16))]
#[derive(Debug, Clone, Copy)]
pub struct c_longdouble(pub [u8; 16]);

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DList<T> {
    pub head: *mut T,
    pub tail: *mut T,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Varr<T> {
    pub els_num: size_t,
    pub size: size_t,
    pub varr: *mut T,
}

pub const MIR_API_VERSION: f64 = 0.1;

pub type htab_hash_t = c_uint;

pub type MIR_error_type_t = c_uint;

pub const MIR_no_error: MIR_error_type_t = 0;
pub const MIR_syntax_error: MIR_error_type_t = 1;
pub const MIR_binary_io_error: MIR_error_type_t = 2;
pub const MIR_alloc_error: MIR_error_type_t = 3;
pub const MIR_finish_error: MIR_error_type_t = 4;
pub const MIR_no_module_error: MIR_error_type_t = 5;
pub const MIR_nested_module_error: MIR_error_type_t = 6;
pub const MIR_no_func_error: MIR_error_type_t = 7;
pub const MIR_func_error: MIR_error_type_t = 8;
pub const MIR_vararg_func_error: MIR_error_type_t = 9;
pub const MIR_nested_func_error: MIR_error_type_t = 10;
pub const MIR_wrong_param_value_error: MIR_error_type_t = 11;
pub const MIR_reserved_name_error: MIR_error_type_t = 12;
pub const MIR_import_export_error: MIR_error_type_t = 13;
pub const MIR_undeclared_func_reg_error: MIR_error_type_t = 14;
pub const MIR_repeated_decl_error: MIR_error_type_t = 15;
pub const MIR_reg_type_error: MIR_error_type_t = 16;
pub const MIR_wrong_type_error: MIR_error_type_t = 17;
pub const MIR_unique_reg_error: MIR_error_type_t = 18;
pub const MIR_undeclared_op_ref_error: MIR_error_type_t = 19;
pub const MIR_ops_num_error: MIR_error_type_t = 20;
pub const MIR_call_op_error: MIR_error_type_t = 21;
pub const MIR_unspec_op_error: MIR_error_type_t = 22;
pub const MIR_ret_error: MIR_error_type_t = 23;
pub const MIR_op_mode_error: MIR_error_type_t = 24;
pub const MIR_out_op_error: MIR_error_type_t = 25;
pub const MIR_invalid_insn_error: MIR_error_type_t = 26;
pub const MIR_ctx_change_error: MIR_error_type_t = 27;
pub const MIR_parallel_error: MIR_error_type_t = 28;

pub type MIR_error_func_t =
    Option<unsafe extern "C" fn(error_type: MIR_error_type_t, format: *const c_char, ...) -> !>;

pub type MIR_insn_code_t = c_uint;
pub const MIR_MOV: MIR_insn_code_t = 0;
pub const MIR_FMOV: MIR_insn_code_t = 1;
pub const MIR_DMOV: MIR_insn_code_t = 2;
pub const MIR_LDMOV: MIR_insn_code_t = 3;
pub const MIR_EXT8: MIR_insn_code_t = 4;
pub const MIR_EXT16: MIR_insn_code_t = 5;
pub const MIR_EXT32: MIR_insn_code_t = 6;
pub const MIR_UEXT8: MIR_insn_code_t = 7;
pub const MIR_UEXT16: MIR_insn_code_t = 8;
pub const MIR_UEXT32: MIR_insn_code_t = 9;
pub const MIR_I2F: MIR_insn_code_t = 10;
pub const MIR_I2D: MIR_insn_code_t = 11;
pub const MIR_I2LD: MIR_insn_code_t = 12;
pub const MIR_UI2F: MIR_insn_code_t = 13;
pub const MIR_UI2D: MIR_insn_code_t = 14;
pub const MIR_UI2LD: MIR_insn_code_t = 15;
pub const MIR_F2I: MIR_insn_code_t = 16;
pub const MIR_D2I: MIR_insn_code_t = 17;
pub const MIR_LD2I: MIR_insn_code_t = 18;
pub const MIR_F2D: MIR_insn_code_t = 19;
pub const MIR_F2LD: MIR_insn_code_t = 20;
pub const MIR_D2F: MIR_insn_code_t = 21;
pub const MIR_D2LD: MIR_insn_code_t = 22;
pub const MIR_LD2F: MIR_insn_code_t = 23;
pub const MIR_LD2D: MIR_insn_code_t = 24;
pub const MIR_NEG: MIR_insn_code_t = 25;
pub const MIR_NEGS: MIR_insn_code_t = 26;
pub const MIR_FNEG: MIR_insn_code_t = 27;
pub const MIR_DNEG: MIR_insn_code_t = 28;
pub const MIR_LDNEG: MIR_insn_code_t = 29;
pub const MIR_ADD: MIR_insn_code_t = 30;
pub const MIR_ADDS: MIR_insn_code_t = 31;
pub const MIR_FADD: MIR_insn_code_t = 32;
pub const MIR_DADD: MIR_insn_code_t = 33;
pub const MIR_LDADD: MIR_insn_code_t = 34;
pub const MIR_SUB: MIR_insn_code_t = 35;
pub const MIR_SUBS: MIR_insn_code_t = 36;
pub const MIR_FSUB: MIR_insn_code_t = 37;
pub const MIR_DSUB: MIR_insn_code_t = 38;
pub const MIR_LDSUB: MIR_insn_code_t = 39;
pub const MIR_MUL: MIR_insn_code_t = 40;
pub const MIR_MULS: MIR_insn_code_t = 41;
pub const MIR_FMUL: MIR_insn_code_t = 42;
pub const MIR_DMUL: MIR_insn_code_t = 43;
pub const MIR_LDMUL: MIR_insn_code_t = 44;
pub const MIR_DIV: MIR_insn_code_t = 45;
pub const MIR_DIVS: MIR_insn_code_t = 46;
pub const MIR_UDIV: MIR_insn_code_t = 47;
pub const MIR_UDIVS: MIR_insn_code_t = 48;
pub const MIR_FDIV: MIR_insn_code_t = 49;
pub const MIR_DDIV: MIR_insn_code_t = 50;
pub const MIR_LDDIV: MIR_insn_code_t = 51;
pub const MIR_MOD: MIR_insn_code_t = 52;
pub const MIR_MODS: MIR_insn_code_t = 53;
pub const MIR_UMOD: MIR_insn_code_t = 54;
pub const MIR_UMODS: MIR_insn_code_t = 55;
pub const MIR_AND: MIR_insn_code_t = 56;
pub const MIR_ANDS: MIR_insn_code_t = 57;
pub const MIR_OR: MIR_insn_code_t = 58;
pub const MIR_ORS: MIR_insn_code_t = 59;
pub const MIR_XOR: MIR_insn_code_t = 60;
pub const MIR_XORS: MIR_insn_code_t = 61;
pub const MIR_LSH: MIR_insn_code_t = 62;
pub const MIR_LSHS: MIR_insn_code_t = 63;
pub const MIR_RSH: MIR_insn_code_t = 64;
pub const MIR_RSHS: MIR_insn_code_t = 65;
pub const MIR_URSH: MIR_insn_code_t = 66;
pub const MIR_URSHS: MIR_insn_code_t = 67;
pub const MIR_EQ: MIR_insn_code_t = 68;
pub const MIR_EQS: MIR_insn_code_t = 69;
pub const MIR_FEQ: MIR_insn_code_t = 70;
pub const MIR_DEQ: MIR_insn_code_t = 71;
pub const MIR_LDEQ: MIR_insn_code_t = 72;
pub const MIR_NE: MIR_insn_code_t = 73;
pub const MIR_NES: MIR_insn_code_t = 74;
pub const MIR_FNE: MIR_insn_code_t = 75;
pub const MIR_DNE: MIR_insn_code_t = 76;
pub const MIR_LDNE: MIR_insn_code_t = 77;
pub const MIR_LT: MIR_insn_code_t = 78;
pub const MIR_LTS: MIR_insn_code_t = 79;
pub const MIR_ULT: MIR_insn_code_t = 80;
pub const MIR_ULTS: MIR_insn_code_t = 81;
pub const MIR_FLT: MIR_insn_code_t = 82;
pub const MIR_DLT: MIR_insn_code_t = 83;
pub const MIR_LDLT: MIR_insn_code_t = 84;
pub const MIR_LE: MIR_insn_code_t = 85;
pub const MIR_LES: MIR_insn_code_t = 86;
pub const MIR_ULE: MIR_insn_code_t = 87;
pub const MIR_ULES: MIR_insn_code_t = 88;
pub const MIR_FLE: MIR_insn_code_t = 89;
pub const MIR_DLE: MIR_insn_code_t = 90;
pub const MIR_LDLE: MIR_insn_code_t = 91;
pub const MIR_GT: MIR_insn_code_t = 92;
pub const MIR_GTS: MIR_insn_code_t = 93;
pub const MIR_UGT: MIR_insn_code_t = 94;
pub const MIR_UGTS: MIR_insn_code_t = 95;
pub const MIR_FGT: MIR_insn_code_t = 96;
pub const MIR_DGT: MIR_insn_code_t = 97;
pub const MIR_LDGT: MIR_insn_code_t = 98;
pub const MIR_GE: MIR_insn_code_t = 99;
pub const MIR_GES: MIR_insn_code_t = 100;
pub const MIR_UGE: MIR_insn_code_t = 101;
pub const MIR_UGES: MIR_insn_code_t = 102;
pub const MIR_FGE: MIR_insn_code_t = 103;
pub const MIR_DGE: MIR_insn_code_t = 104;
pub const MIR_LDGE: MIR_insn_code_t = 105;
pub const MIR_JMP: MIR_insn_code_t = 106;
pub const MIR_BT: MIR_insn_code_t = 107;
pub const MIR_BTS: MIR_insn_code_t = 108;
pub const MIR_BF: MIR_insn_code_t = 109;
pub const MIR_BFS: MIR_insn_code_t = 110;
pub const MIR_BEQ: MIR_insn_code_t = 111;
pub const MIR_BEQS: MIR_insn_code_t = 112;
pub const MIR_FBEQ: MIR_insn_code_t = 113;
pub const MIR_DBEQ: MIR_insn_code_t = 114;
pub const MIR_LDBEQ: MIR_insn_code_t = 115;
pub const MIR_BNE: MIR_insn_code_t = 116;
pub const MIR_BNES: MIR_insn_code_t = 117;
pub const MIR_FBNE: MIR_insn_code_t = 118;
pub const MIR_DBNE: MIR_insn_code_t = 119;
pub const MIR_LDBNE: MIR_insn_code_t = 120;
pub const MIR_BLT: MIR_insn_code_t = 121;
pub const MIR_BLTS: MIR_insn_code_t = 122;
pub const MIR_UBLT: MIR_insn_code_t = 123;
pub const MIR_UBLTS: MIR_insn_code_t = 124;
pub const MIR_FBLT: MIR_insn_code_t = 125;
pub const MIR_DBLT: MIR_insn_code_t = 126;
pub const MIR_LDBLT: MIR_insn_code_t = 127;
pub const MIR_BLE: MIR_insn_code_t = 128;
pub const MIR_BLES: MIR_insn_code_t = 129;
pub const MIR_UBLE: MIR_insn_code_t = 130;
pub const MIR_UBLES: MIR_insn_code_t = 131;
pub const MIR_FBLE: MIR_insn_code_t = 132;
pub const MIR_DBLE: MIR_insn_code_t = 133;
pub const MIR_LDBLE: MIR_insn_code_t = 134;
pub const MIR_BGT: MIR_insn_code_t = 135;
pub const MIR_BGTS: MIR_insn_code_t = 136;
pub const MIR_UBGT: MIR_insn_code_t = 137;
pub const MIR_UBGTS: MIR_insn_code_t = 138;
pub const MIR_FBGT: MIR_insn_code_t = 139;
pub const MIR_DBGT: MIR_insn_code_t = 140;
pub const MIR_LDBGT: MIR_insn_code_t = 141;
pub const MIR_BGE: MIR_insn_code_t = 142;
pub const MIR_BGES: MIR_insn_code_t = 143;
pub const MIR_UBGE: MIR_insn_code_t = 144;
pub const MIR_UBGES: MIR_insn_code_t = 145;
pub const MIR_FBGE: MIR_insn_code_t = 146;
pub const MIR_DBGE: MIR_insn_code_t = 147;
pub const MIR_LDBGE: MIR_insn_code_t = 148;
pub const MIR_CALL: MIR_insn_code_t = 149;
pub const MIR_INLINE: MIR_insn_code_t = 150;
pub const MIR_SWITCH: MIR_insn_code_t = 151;
pub const MIR_RET: MIR_insn_code_t = 152;
pub const MIR_ALLOCA: MIR_insn_code_t = 153;
pub const MIR_BSTART: MIR_insn_code_t = 154;
pub const MIR_BEND: MIR_insn_code_t = 155;
pub const MIR_VA_ARG: MIR_insn_code_t = 156;
pub const MIR_VA_BLOCK_ARG: MIR_insn_code_t = 157;
pub const MIR_VA_START: MIR_insn_code_t = 158;
pub const MIR_VA_END: MIR_insn_code_t = 159;
pub const MIR_LABEL: MIR_insn_code_t = 160;
pub const MIR_UNSPEC: MIR_insn_code_t = 161;
pub const MIR_PHI: MIR_insn_code_t = 162;
pub const MIR_INVALID_INSN: MIR_insn_code_t = 163;
pub const MIR_INSN_BOUND: MIR_insn_code_t = 164;

pub type MIR_type_t = c_uint;
pub const MIR_T_I8: MIR_type_t = 0;
pub const MIR_T_U8: MIR_type_t = 1;
pub const MIR_T_I16: MIR_type_t = 2;
pub const MIR_T_U16: MIR_type_t = 3;
pub const MIR_T_I32: MIR_type_t = 4;
pub const MIR_T_U32: MIR_type_t = 5;
pub const MIR_T_I64: MIR_type_t = 6;
pub const MIR_T_U64: MIR_type_t = 7;
pub const MIR_T_F: MIR_type_t = 8;
pub const MIR_T_D: MIR_type_t = 9;
pub const MIR_T_LD: MIR_type_t = 10;
pub const MIR_T_P: MIR_type_t = 11;
pub const MIR_T_BLK: MIR_type_t = 12;
pub const MIR_T_RBLK: MIR_type_t = 17;
pub const MIR_T_UNDEF: MIR_type_t = 18;
pub const MIR_T_BOUND: MIR_type_t = 19;

pub type MIR_scale_t = u8;
pub type MIR_disp_t = i64;
pub type MIR_reg_t = u32;

pub const MIR_MAX_REG_NUM: u32 = u32::MAX;
pub const MIR_NON_HARD_REG: u32 = MIR_MAX_REG_NUM;

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub union MIR_imm_t {
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    pub ld: c_longdouble,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_mem_t {
    pub type_: u8,
    pub scale: MIR_scale_t,
    pub base: MIR_reg_t,
    pub index: MIR_reg_t,
    pub disp: MIR_disp_t,
}

pub type MIR_label_t = *mut MIR_insn;
pub type MIR_name_t = *const c_char;

pub type MIR_op_mode_t = c_uint;
pub const MIR_OP_UNDEF: MIR_op_mode_t = 0;
pub const MIR_OP_REG: MIR_op_mode_t = 1;
pub const MIR_OP_HARD_REG: MIR_op_mode_t = 2;
pub const MIR_OP_INT: MIR_op_mode_t = 3;
pub const MIR_OP_UINT: MIR_op_mode_t = 4;
pub const MIR_OP_FLOAT: MIR_op_mode_t = 5;
pub const MIR_OP_DOUBLE: MIR_op_mode_t = 6;
pub const MIR_OP_LDOUBLE: MIR_op_mode_t = 7;
pub const MIR_OP_REF: MIR_op_mode_t = 8;
pub const MIR_OP_STR: MIR_op_mode_t = 9;
pub const MIR_OP_MEM: MIR_op_mode_t = 10;
pub const MIR_OP_HARD_REG_MEM: MIR_op_mode_t = 11;
pub const MIR_OP_LABEL: MIR_op_mode_t = 12;
pub const MIR_OP_BOUND: MIR_op_mode_t = 13;

pub type MIR_item_t = *mut MIR_item;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_str {
    pub len: size_t,
    pub s: *const c_char,
}
pub type MIR_str_t = MIR_str;

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub struct MIR_op_t {
    pub data: *mut c_void,
    pub mode: MIR_op_mode_t,
    pub value_mode: MIR_op_mode_t,
    pub u: MIR_op_t_union,
}

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub union MIR_op_t_union {
    pub reg: MIR_reg_t,
    pub hard_reg: MIR_reg_t,
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    pub ld: c_longdouble,
    pub ref_: MIR_item_t,
    pub str_: MIR_str_t,
    pub mem: MIR_mem_t,
    pub hard_reg_mem: MIR_mem_t,
    pub label: MIR_label_t,
}

pub type MIR_insn_t = *mut MIR_insn;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct DLIST_LINK_MIR_insn_t {
    pub prev: MIR_insn_t,
    pub next: MIR_insn_t,
}

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub struct MIR_insn {
    pub data: *mut c_void,
    pub insn_link: DList<MIR_insn_t>,
    pub code: u32,
    pub nops: u32,
    pub ops: [MIR_op_t; 1usize],
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_var_t {
    pub type_: MIR_type_t,
    pub name: *const c_char,
    pub size: size_t,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_func {
    pub name: *const c_char,
    pub func_item: MIR_item_t,
    pub original_vars_num: size_t,
    pub insns: DList<MIR_insn_t>,
    pub original_insns: DList<MIR_insn_t>,
    pub nres: u32,
    pub nargs: u32,
    pub last_temp_num: u32,
    pub n_inlines: u32,
    pub res_types: *mut MIR_type_t,
    pub vararg_p: c_char,
    pub expr_p: c_char,
    pub vars: *mut Varr<MIR_var_t>,
    pub machine_code: *mut c_void,
    pub call_addr: *mut c_void,
    pub internal: *mut c_void,
}

pub type MIR_func_t = *mut MIR_func;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_proto {
    pub name: *const c_char,
    pub nres: u32,
    pub res_types: *mut MIR_type_t,
    pub vararg_p: c_char,
    pub args: *mut Varr<MIR_var_t>,
}

pub type MIR_proto_t = *mut MIR_proto;

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub struct MIR_data {
    pub name: *const c_char,
    pub el_type: MIR_type_t,
    pub nel: size_t,
    // pub _padding0: u64,
    pub u: MIR_data_union,
}

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub union MIR_data_union {
    pub d: c_longdouble,
    pub els: [u8; 1usize],
}

pub type MIR_data_t = *mut MIR_data;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_ref_data {
    pub name: *const c_char,
    pub ref_item: MIR_item_t,
    pub disp: i64,
    pub load_addr: *mut c_void,
}

pub type MIR_ref_data_t = *mut MIR_ref_data;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_expr_data {
    pub name: *const c_char,
    pub expr_item: MIR_item_t,
    pub load_addr: *mut c_void,
}

pub type MIR_expr_data_t = *mut MIR_expr_data;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_bss {
    pub name: *const c_char,
    pub len: u64,
}

pub type MIR_bss_t = *mut MIR_bss;

pub type MIR_module_t = *mut MIR_module;

pub type MIR_item_type_t = c_uint;
pub const MIR_func_item: MIR_item_type_t = 0;
pub const MIR_proto_item: MIR_item_type_t = 1;
pub const MIR_import_item: MIR_item_type_t = 2;
pub const MIR_export_item: MIR_item_type_t = 3;
pub const MIR_forward_item: MIR_item_type_t = 4;
pub const MIR_data_item: MIR_item_type_t = 5;
pub const MIR_ref_data_item: MIR_item_type_t = 6;
pub const MIR_expr_data_item: MIR_item_type_t = 7;
pub const MIR_bss_item: MIR_item_type_t = 8;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct MIR_item {
    pub data: *mut c_void,
    pub module: MIR_module_t,
    pub item_link: DList<MIR_item_t>,
    pub item_type: MIR_item_type_t,
    pub ref_def: MIR_item_t,
    pub addr: *mut c_void,
    pub export_p: c_char,
    pub section_head_p: c_char,
    pub u: MIR_item_union,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub union MIR_item_union {
    pub func: MIR_func_t,
    pub proto: MIR_proto_t,
    pub import_id: MIR_name_t,
    pub export_id: MIR_name_t,
    pub forward_id: MIR_name_t,
    pub data: MIR_data_t,
    pub ref_data: MIR_ref_data_t,
    pub expr_data: MIR_expr_data_t,
    pub bss: MIR_bss_t,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_module {
    pub data: *mut c_void,
    pub name: *const c_char,
    pub items: DList<MIR_item_t>,
    pub module_link: DList<MIR_module_t>,
    pub last_temp_item_num: u32,
}

#[repr(C)]
pub struct MIR_context {
    _unused: [u8; 0],
}
pub type MIR_context_t = *mut MIR_context;

#[repr(C)]
#[repr(align(16))]
#[derive(Copy, Clone)]
pub union MIR_val_t {
    pub ic: MIR_insn_code_t,
    pub a: *mut c_void,
    pub i: i64,
    pub u: u64,
    pub f: f32,
    pub d: f64,
    pub ld: c_longdouble,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct MIR_code_reloc_t {
    pub offset: size_t,
    pub value: *const c_void,
}

#[repr(C)]
pub struct c2mir_macro_command {
    pub def_p: c_int,
    pub name: *const c_char,
    pub def: *const c_char,
}

#[repr(C)]
pub struct c2mir_options {
    message_file: *mut FILE,
    debug_p: c_int,
    verbose_p: c_int,
    ignore_warnings_p: c_int,
    no_prepro_p: c_int,
    prepro_only_p: c_int,
    syntax_only_p: c_int,
    pedantic_p: c_int,
    asm_p: c_int,
    object_p: c_int,
    module_num: size_t,
    prepro_output_file: *mut FILE,
    output_file_name: *const c_char,
    macro_commands_num: size_t,
    include_dirs_num: size_t,
    macro_commands: *mut c2mir_macro_command,
    include_dirs: *mut *const c_char,
}

extern "C" {
    fn _MIR_get_api_version() -> f64;
    fn _MIR_init() -> MIR_context_t;

    pub fn MIR_finish(ctx: MIR_context_t);
    pub fn MIR_new_module(ctx: MIR_context_t, name: *const c_char) -> MIR_module_t;
    pub fn MIR_get_module_list(ctx: MIR_context_t) -> *mut DList<MIR_module_t>;
    pub fn MIR_new_import(ctx: MIR_context_t, name: *const c_char) -> MIR_item_t;
    pub fn MIR_new_export(ctx: MIR_context_t, name: *const c_char) -> MIR_item_t;
    pub fn MIR_new_forward(ctx: MIR_context_t, name: *const c_char) -> MIR_item_t;
    pub fn MIR_new_bss(ctx: MIR_context_t, name: *const c_char, len: size_t) -> MIR_item_t;
    pub fn MIR_new_data(
        ctx: MIR_context_t,
        name: *const c_char,
        el_type: MIR_type_t,
        nel: size_t,
        els: *const c_void,
    ) -> MIR_item_t;
    pub fn MIR_new_string_data(
        ctx: MIR_context_t,
        name: *const c_char,
        str_: MIR_str_t,
    ) -> MIR_item_t;
    pub fn MIR_new_ref_data(
        ctx: MIR_context_t,
        name: *const c_char,
        item: MIR_item_t,
        disp: i64,
    ) -> MIR_item_t;
    pub fn MIR_new_expr_data(
        ctx: MIR_context_t,
        name: *const c_char,
        expr_item: MIR_item_t,
    ) -> MIR_item_t;
    pub fn MIR_new_proto_arr(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        vars: *mut MIR_var_t,
    ) -> MIR_item_t;
    pub fn MIR_new_proto(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        ...
    ) -> MIR_item_t;
    pub fn MIR_new_vararg_proto_arr(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        vars: *mut MIR_var_t,
    ) -> MIR_item_t;
    pub fn MIR_new_vararg_proto(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        ...
    ) -> MIR_item_t;
    pub fn MIR_new_func_arr(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        vars: *mut MIR_var_t,
    ) -> MIR_item_t;
    pub fn MIR_new_func(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        ...
    ) -> MIR_item_t;
    pub fn MIR_new_vararg_func_arr(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        vars: *mut MIR_var_t,
    ) -> MIR_item_t;
    pub fn MIR_new_vararg_func(
        ctx: MIR_context_t,
        name: *const c_char,
        nres: size_t,
        res_types: *mut MIR_type_t,
        nargs: size_t,
        ...
    ) -> MIR_item_t;
    pub fn MIR_item_name(ctx: MIR_context_t, item: MIR_item_t) -> *const c_char;
    pub fn MIR_get_item_func(ctx: MIR_context_t, item: MIR_item_t) -> MIR_func_t;
    pub fn MIR_new_func_reg(
        ctx: MIR_context_t,
        func: MIR_func_t,
        type_: MIR_type_t,
        name: *const c_char,
    ) -> MIR_reg_t;
    pub fn MIR_finish_func(ctx: MIR_context_t);
    pub fn MIR_finish_module(ctx: MIR_context_t);
    pub fn MIR_get_error_func(ctx: MIR_context_t) -> MIR_error_func_t;
    pub fn MIR_set_error_func(ctx: MIR_context_t, func: MIR_error_func_t);
    pub fn MIR_new_insn_arr(
        ctx: MIR_context_t,
        code: MIR_insn_code_t,
        nops: size_t,
        ops: *mut MIR_op_t,
    ) -> MIR_insn_t;
    pub fn MIR_new_insn(ctx: MIR_context_t, code: MIR_insn_code_t, ...) -> MIR_insn_t;
    pub fn MIR_new_call_insn(ctx: MIR_context_t, nops: size_t, ...) -> MIR_insn_t;
    pub fn MIR_new_ret_insn(ctx: MIR_context_t, nops: size_t, ...) -> MIR_insn_t;
    pub fn MIR_copy_insn(ctx: MIR_context_t, insn: MIR_insn_t) -> MIR_insn_t;
    pub fn MIR_insn_name(ctx: MIR_context_t, code: MIR_insn_code_t) -> *const c_char;
    pub fn MIR_insn_nops(ctx: MIR_context_t, insn: MIR_insn_t) -> size_t;
    pub fn MIR_insn_op_mode(
        ctx: MIR_context_t,
        insn: MIR_insn_t,
        nop: size_t,
        out_p: *mut c_int,
    ) -> MIR_op_mode_t;
    pub fn MIR_new_label(ctx: MIR_context_t) -> MIR_insn_t;
    pub fn MIR_reg(ctx: MIR_context_t, reg_name: *const c_char, func: MIR_func_t) -> MIR_reg_t;
    pub fn MIR_reg_type(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) -> MIR_type_t;
    pub fn MIR_reg_name(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) -> *const c_char;
    pub fn MIR_new_reg_op(ctx: MIR_context_t, reg: MIR_reg_t) -> MIR_op_t;
    pub fn MIR_new_int_op(ctx: MIR_context_t, v: i64) -> MIR_op_t;
    pub fn MIR_new_uint_op(ctx: MIR_context_t, v: u64) -> MIR_op_t;
    pub fn MIR_new_float_op(ctx: MIR_context_t, v: f32) -> MIR_op_t;
    pub fn MIR_new_double_op(ctx: MIR_context_t, v: f64) -> MIR_op_t;
    pub fn MIR_new_ldouble_op(ctx: MIR_context_t, v: c_longdouble) -> MIR_op_t;
    pub fn MIR_new_ref_op(ctx: MIR_context_t, item: MIR_item_t) -> MIR_op_t;
    pub fn MIR_new_str_op(ctx: MIR_context_t, str_: MIR_str_t) -> MIR_op_t;
    pub fn MIR_new_mem_op(
        ctx: MIR_context_t,
        type_: MIR_type_t,
        disp: MIR_disp_t,
        base: MIR_reg_t,
        index: MIR_reg_t,
        scale: MIR_scale_t,
    ) -> MIR_op_t;
    pub fn MIR_new_label_op(ctx: MIR_context_t, label: MIR_label_t) -> MIR_op_t;
    pub fn MIR_op_eq_p(ctx: MIR_context_t, op1: MIR_op_t, op2: MIR_op_t) -> c_int;
    pub fn MIR_op_hash_step(ctx: MIR_context_t, h: htab_hash_t, op: MIR_op_t) -> htab_hash_t;
    pub fn MIR_append_insn(ctx: MIR_context_t, func: MIR_item_t, insn: MIR_insn_t);
    pub fn MIR_prepend_insn(ctx: MIR_context_t, func: MIR_item_t, insn: MIR_insn_t);
    pub fn MIR_insert_insn_after(
        ctx: MIR_context_t,
        func: MIR_item_t,
        after: MIR_insn_t,
        insn: MIR_insn_t,
    );
    pub fn MIR_insert_insn_before(
        ctx: MIR_context_t,
        func: MIR_item_t,
        before: MIR_insn_t,
        insn: MIR_insn_t,
    );
    pub fn MIR_remove_insn(ctx: MIR_context_t, func: MIR_item_t, insn: MIR_insn_t);
    pub fn MIR_change_module_ctx(old_ctx: MIR_context_t, m: MIR_module_t, new_ctx: MIR_context_t);
    pub fn MIR_reverse_branch_code(code: MIR_insn_code_t) -> MIR_insn_code_t;
    pub fn MIR_type_str(ctx: MIR_context_t, tp: MIR_type_t) -> *const c_char;
    pub fn MIR_output_op(ctx: MIR_context_t, f: *mut FILE, op: MIR_op_t, func: MIR_func_t);
    pub fn MIR_output_insn(
        ctx: MIR_context_t,
        f: *mut FILE,
        insn: MIR_insn_t,
        func: MIR_func_t,
        newline_p: c_int,
    );
    pub fn MIR_output_item(ctx: MIR_context_t, f: *mut FILE, item: MIR_item_t);
    pub fn MIR_output_module(ctx: MIR_context_t, f: *mut FILE, module: MIR_module_t);
    pub fn MIR_output(ctx: MIR_context_t, f: *mut FILE);
    pub fn MIR_write(ctx: MIR_context_t, f: *mut FILE);
    pub fn MIR_write_module(ctx: MIR_context_t, f: *mut FILE, module: MIR_module_t);
    pub fn MIR_read(ctx: MIR_context_t, f: *mut FILE);
    pub fn MIR_write_with_func(
        ctx: MIR_context_t,
        writer_func: ::std::option::Option<
            unsafe extern "C" fn(arg1: MIR_context_t, arg2: u8) -> c_int,
        >,
    );
    pub fn MIR_write_module_with_func(
        ctx: MIR_context_t,
        writer_func: ::std::option::Option<
            unsafe extern "C" fn(arg1: MIR_context_t, arg2: u8) -> c_int,
        >,
        module: MIR_module_t,
    );
    pub fn MIR_read_with_func(
        ctx: MIR_context_t,
        reader_func: ::std::option::Option<unsafe extern "C" fn(arg1: MIR_context_t) -> c_int>,
    );
    pub fn MIR_scan_string(ctx: MIR_context_t, str_: *const c_char);
    pub fn MIR_get_global_item(ctx: MIR_context_t, name: *const c_char) -> MIR_item_t;
    pub fn MIR_load_module(ctx: MIR_context_t, m: MIR_module_t);
    pub fn MIR_load_external(ctx: MIR_context_t, name: *const c_char, addr: *mut c_void);
    pub fn MIR_link(
        ctx: MIR_context_t,
        set_interface: ::std::option::Option<
            unsafe extern "C" fn(ctx: MIR_context_t, item: MIR_item_t),
        >,
        import_resolver: ::std::option::Option<
            unsafe extern "C" fn(arg1: *const c_char) -> *mut c_void,
        >,
    );
    pub fn MIR_interp(
        ctx: MIR_context_t,
        func_item: MIR_item_t,
        results: *mut MIR_val_t,
        nargs: size_t,
        ...
    );
    pub fn MIR_interp_arr(
        ctx: MIR_context_t,
        func_item: MIR_item_t,
        results: *mut MIR_val_t,
        nargs: size_t,
        vals: *mut MIR_val_t,
    );
    // pub fn MIR_interp_arr_varg(
    //     ctx: MIR_context_t,
    //     func_item: MIR_item_t,
    //     results: *mut MIR_val_t,
    //     nargs: size_t,
    //     vals: *mut MIR_val_t,
    //     va: *mut __va_list_tag,
    // );
    pub fn MIR_set_interp_interface(ctx: MIR_context_t, func_item: MIR_item_t);
    pub fn MIR_gen_init(ctx: MIR_context_t, gens_num: c_int);
    pub fn MIR_gen_set_debug_file(ctx: MIR_context_t, gen_num: c_int, f: *mut FILE);
    pub fn MIR_gen_set_debug_level(ctx: MIR_context_t, gen_num: c_int, debug_level: c_int);
    pub fn MIR_gen_set_optimize_level(ctx: MIR_context_t, gen_num: c_int, level: c_uint);
    pub fn MIR_gen(ctx: MIR_context_t, gen_num: c_int, func_item: MIR_item_t) -> *mut c_void;
    pub fn MIR_set_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t);
    pub fn MIR_set_parallel_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t);
    pub fn MIR_set_lazy_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t);
    pub fn MIR_gen_finish(ctx: MIR_context_t);

    pub fn c2mir_init(ctx: MIR_context_t);
    pub fn c2mir_finish(ctx: MIR_context_t);
    pub fn c2mir_compile(
        ctx: MIR_context_t,
        ops: *mut c2mir_options,
        getc_func: Option<extern "C" fn(*mut c_void) -> c_int>,
        getc_data: *mut c_void,
        source_name: *const c_char,
        output_file: *mut FILE,
    );
}

#[allow(non_snake_case)]
#[inline(always)]
pub unsafe fn MIR_init() -> MIR_context_t {
    // We lock the C source in tree, so version checking is not necessary.
    debug_assert_eq!(MIR_API_VERSION, _MIR_get_api_version());
    _MIR_init()
}
