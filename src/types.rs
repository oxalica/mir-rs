use std::fmt;

use sealed::Sealed;

use crate::ffi;

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Val(pub(crate) ffi::MIR_val_t);

impl Default for Val {
    fn default() -> Self {
        Self(unsafe { std::mem::zeroed::<ffi::MIR_val_t>() })
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Val").field("i64", &self.as_i64()).finish()
    }
}

impl From<i64> for Val {
    fn from(i: i64) -> Self {
        Val(ffi::MIR_val_t { i })
    }
}

impl Val {
    pub fn as_i64(self) -> i64 {
        unsafe { self.0.i }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Ty(pub(crate) ffi::MIR_type_t);

impl Ty {
    pub const I8: Self = Self(ffi::MIR_T_I8);
    pub const U8: Self = Self(ffi::MIR_T_U8);
    pub const I16: Self = Self(ffi::MIR_T_I16);
    pub const U16: Self = Self(ffi::MIR_T_U16);
    pub const I32: Self = Self(ffi::MIR_T_I32);
    pub const U32: Self = Self(ffi::MIR_T_U32);
    pub const I64: Self = Self(ffi::MIR_T_I64);
    pub const U64: Self = Self(ffi::MIR_T_U64);
    pub const F: Self = Self(ffi::MIR_T_F);
    pub const D: Self = Self(ffi::MIR_T_D);
    pub const LD: Self = Self(ffi::MIR_T_LD);
    pub const P: Self = Self(ffi::MIR_T_P);
    pub const BLK: Self = Self(ffi::MIR_T_BLK);
    pub const RBLK: Self = Self(ffi::MIR_T_RBLK);
    pub const UNDEF: Self = Self(ffi::MIR_T_UNDEF);
    pub const BOUND: Self = Self(ffi::MIR_T_BOUND);
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Reg(pub(crate) ffi::MIR_reg_t);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Operand(pub(crate) ffi::MIR_op_t);

mod sealed {
    use crate::ffi;

    pub trait Sealed {
        unsafe fn build(self, ctx: ffi::MIR_context_t) -> super::Operand;
    }
}

pub trait IntoOperand: Sealed {}

impl IntoOperand for super::Reg {}
impl Sealed for super::Reg {
    unsafe fn build(self, ctx: ffi::MIR_context_t) -> Operand {
        unsafe { Operand(ffi::MIR_new_reg_op(ctx, self.0)) }
    }
}

impl IntoOperand for i64 {}
impl Sealed for i64 {
    unsafe fn build(self, ctx: ffi::MIR_context_t) -> Operand {
        unsafe { Operand(ffi::MIR_new_int_op(ctx, self)) }
    }
}

pub(crate) trait IntoOperands {
    type Arr;
    unsafe fn build(self, ctx: ffi::MIR_context_t) -> Self::Arr;
}

macro_rules! impl_into_ops {
    ($($len:literal, $($name:ident),*;)*) => {
        $(
            impl<$($name: IntoOperand),*> IntoOperands for ($($name,)*) {
                type Arr = [Operand; $len];
                unsafe fn build(self, ctx: ffi::MIR_context_t) -> Self::Arr {
                    #[expect(non_snake_case)]
                    let ($($name,)*) = self;
                    unsafe { [$($name.build(ctx)),*] }
                }
            }
        )*
    };
}
impl_into_ops! {
    1, A;
    2, A, B;
    3, A, B, C;
}

pub trait InstBuilder: Sized {
    #[expect(private_bounds)]
    fn build<const LEN: usize>(
        self,
        code: ffi::MIR_insn_code_t,
        ops: impl IntoOperands<Arr = [Operand; LEN]>,
    );

    fn add(self, dst: impl IntoOperand, src1: impl IntoOperand, src2: impl IntoOperand) {
        self.build(ffi::MIR_ADD, (dst, src1, src2));
    }

    fn ret(self, v: impl IntoOperand) {
        self.build(ffi::MIR_RET, (v,));
    }
}
