use std::fmt;
use std::marker::PhantomData;
use std::ptr::{NonNull, null_mut};

use paste::paste;
use smallvec::SmallVec;

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
        f.debug_struct("Val")
            .field("i64", &self.as_i64())
            .field("u64", &self.as_u64())
            .field("f32", &self.as_f32())
            .field("f64", &self.as_f64())
            .finish()
    }
}

macro_rules! impl_val_variant {
    ($($name:ident: $ty:ident;)*) => {
        $(
            impl From<$ty> for Val {
                fn from($name: $ty) -> Self {
                    Val(ffi::MIR_val_t { $name })
                }
            }
        )*
        impl Val {
            paste! {
                $(
                pub fn [<as_ $ty>](self) -> $ty {
                    unsafe { self.0.$name }
                }
                )*
            }
        }
    };
}

impl_val_variant! {
    i: i64;
    u: u64;
    f: f32;
    d: f64;
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

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ProtoItem<'module> {
    pub(crate) item: NonNull<ffi::MIR_item>,
    pub(crate) _marker: PhantomData<&'module ()>,
}

impl<'a> From<ProtoItem<'a>> for Operand<'a> {
    fn from(item: ProtoItem<'a>) -> Self {
        Self {
            op: unsafe { ffi::MIR_new_ref_op(null_mut(), item.item.as_ptr()) },
            _marker: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ImportItem<'module> {
    pub(crate) item: NonNull<ffi::MIR_item>,
    pub(crate) _marker: PhantomData<&'module ()>,
}

impl<'a> From<ImportItem<'a>> for Operand<'a> {
    fn from(item: ImportItem<'a>) -> Self {
        Self {
            op: unsafe { ffi::MIR_new_ref_op(null_mut(), item.item.as_ptr()) },
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Operand<'a> {
    pub(crate) op: ffi::MIR_op_t,
    pub(crate) _marker: PhantomData<&'a ()>,
}

pub trait IntoOperand<'a>: Into<Operand<'a>> {}
pub trait IntoOutOperand<'a>: IntoOperand<'a> {}

impl<'a, T> IntoOperand<'a> for T where Operand<'a>: From<T> {}

impl IntoOutOperand<'_> for Reg {}
impl From<Reg> for Operand<'_> {
    fn from(reg: Reg) -> Self {
        Self {
            op: unsafe { ffi::MIR_new_reg_op(null_mut(), reg.0) },
            _marker: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Label<'func>(
    pub(crate) ffi::MIR_label_t,
    pub(crate) PhantomData<&'func ()>,
);

impl<'a> From<Label<'a>> for Operand<'a> {
    fn from(label: Label<'a>) -> Self {
        Self {
            op: unsafe { ffi::MIR_new_label_op(null_mut(), label.0) },
            _marker: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MemOp {
    ty: Ty,
    disp: i64,
    base: Reg,
    index: Reg,
    scale: u8,
}

impl MemOp {
    pub fn new_base(ty: Ty, base: Reg) -> Self {
        Self {
            ty,
            disp: 0,
            base,
            index: Reg(0),
            scale: 0,
        }
    }

    pub fn disp(self, disp: i64) -> Self {
        Self { disp, ..self }
    }

    pub fn index(self, index: Reg) -> Self {
        Self { index, ..self }
    }

    pub fn scale(self, scale: u8) -> Self {
        assert!(matches!(scale, 1 | 2 | 4 | 8), "scale must be 1, 2, 4 or 8");
        Self { scale, ..self }
    }
}

impl IntoOutOperand<'_> for MemOp {}
impl From<MemOp> for Operand<'_> {
    fn from(mem: MemOp) -> Self {
        Self {
            op: unsafe {
                ffi::MIR_new_mem_op(
                    null_mut(),
                    mem.ty.0,
                    mem.disp,
                    mem.base.0,
                    mem.index.0,
                    mem.scale,
                )
            },
            _marker: PhantomData,
        }
    }
}

macro_rules! impl_literal_into_op {
    ($($ty:ident, $func:ident;)*) => {
        $(
            impl From<$ty> for Operand<'_> {
                fn from(v: $ty) -> Self {
                    Self {
                        op: unsafe { ffi::$func(null_mut(), v) },
                        _marker: PhantomData,
                    }
                }
            }
        )*
    };
}

impl_literal_into_op! {
    i64, MIR_new_int_op;
    u64, MIR_new_uint_op;
    f32, MIR_new_float_op;
    f64, MIR_new_double_op;
}

macro_rules! def_simple_insn {
    (__impl $args:tt $name:ident) => {
        def_simple_insn!(__impl $args $name paste!(ffi::[<MIR_ $name:upper>]));
    };
    (__impl (dst $(, $src:ident)*) $name:ident $code:expr) => {
        fn $name<'o>(self, dst: impl IntoOutOperand<'o>, $($src: impl IntoOperand<'o>),*) {
            build_insn(self, $code, [dst.into(), $($src.into()),*]);
        }
    };
    (
        $args:tt;
        $($name:ident $(($code:expr))?),*
        $(,)?
    ) => {
        $(def_simple_insn!(__impl $args $name $($code)?);)*
    };
}

macro_rules! def_jump_insn {
    (__impl (label $(, $src:ident)*) $name:ident) => {
        #[allow(clippy::needless_lifetimes)]
        fn $name<'o>(self, label: Label<'o>, $($src: impl IntoOperand<'o>),*) {
            build_insn(self, paste!(ffi::[<MIR_ $name:upper>]), [label.into(), $($src.into()),*]);
        }
    };
    (
        $args:tt;
        $($name:ident $(($code:expr))?),*
        $(,)?
    ) => {
        $(def_jump_insn!(__impl $args $name $($code)?);)*
    };
}

/// # Safety
/// TODO
pub unsafe trait InsnBuilder<'func>: Sized {
    fn get_raw_ctx(&self) -> ffi::MIR_context_t;
    /// # Safety
    /// TODO
    unsafe fn insert(self, insn: ffi::MIR_insn_t);
}

impl<'func, T: InsnBuilder<'func>> InsnBuilderExt<'func> for T {}

// Mostly follows the order in mir.h.
pub trait InsnBuilderExt<'func>: InsnBuilder<'func> {
    // Unary ops.
    def_simple_insn! {
        (dst, src);

        mov, fmov, dmov, // ldmov

        ext8, ext16, ext32, uext8, uext16, uext32,

        i2f, i2d, // i2ld
        ui2f, ui2d, // ui2ld
        f2i, d2i, // ld2i
        f2d, d2f, // f2ld, d2ld, ld2f, ld2d

        neg, negs, fneg, dneg, // ldneg

        addr, addr8, addr16, addr32,
    }

    // Binary ops.
    def_simple_insn! {
        (dst, a, b);

        add, adds, fadd, dadd, // ldadd
        sub, subs, fsub, dsub, // ldsub
        mul, muls, fmul, dmul, // ldmul
        div, divs, udiv, udivs, fdiv, ddiv, // lddiv
        mod_(ffi::MIR_MOD), mods, umod, umods,

        and, ands, or, ors, xor, xors,
        lsh, lshs, rsh, rshs, ursh, urshs,

        eq, eqs, feq, deq, // ldeq
        ne, nes, fne, dne, // ldne
        lt, lts, ult, ults, flt, dlt, // ldlt
        le, les, ule, ules, fle, dle, // ldle
        gt, gts, ugt, ugts, fgt, dgt, // ldgt
        ge, ges, uge, uges, fge, dge, // ldge

        addo, addos, subo, subos, mulo, mulos, umulo, umulos,
    }

    // Jumps.
    def_jump_insn!((label); jmp, bo, bno, ubo, ubno); // jmpi, laddr
    def_jump_insn!((label, v); bt, bts, bf, bfs);
    def_jump_insn! {
        (label, a, b);
        beq, beqs, fbeq, dbeq, // ldbeq
        bne, bnes, fbne, dbne, // ldbne
        blt, blts, ublt, ublts, fblt, dblt, // ldblt
        ble, bles, uble, ubles, fble, dble, // ldble
        bgt, bgts, ubgt, ubgts, fbgt, dbgt, // ldbgt
        bge, bges, ubge, ubges, fbge, dbge, // ldbge
    }

    // Call.
    fn call<'o>(
        self,
        proto: ProtoItem<'o>,
        func: impl IntoOperand<'o>,
        results: impl IntoIterator<Item = Operand<'o>>,
        args: impl IntoIterator<Item = Operand<'o>>,
    ) {
        build_insn(
            self,
            ffi::MIR_CALL,
            [proto.into(), func.into()]
                .into_iter()
                .chain(results)
                .chain(args),
        );
    }
    fn inline<'o>(
        self,
        proto: ProtoItem<'o>,
        func: impl IntoOperand<'o>,
        results: impl IntoIterator<Item = Operand<'o>>,
        args: impl IntoIterator<Item = Operand<'o>>,
    ) {
        build_insn(
            self,
            ffi::MIR_INLINE,
            [proto.into(), func.into()]
                .into_iter()
                .chain(results)
                .chain(args),
        );
    }
    fn jcall<'o>(
        self,
        proto: ProtoItem<'o>,
        func: impl IntoOperand<'o>,
        results: impl IntoIterator<Item = Operand<'o>>,
        args: impl IntoIterator<Item = Operand<'o>>,
    ) {
        build_insn(
            self,
            ffi::MIR_JCALL,
            [proto.into(), func.into()]
                .into_iter()
                .chain(results)
                .chain(args),
        );
    }

    // Return.
    fn ret<'o>(self, v: impl IntoOperand<'o>) {
        build_insn(self, ffi::MIR_RET, [v.into()]);
    }
    fn jret<'o>(self, v: impl IntoOperand<'o>) {
        build_insn(self, ffi::MIR_JRET, [v.into()]);
    }

    // Function frame.
    def_simple_insn!((dst, len); alloca);
    // va_arg, va_block_arg, va_start, va_end

    // Label.
    fn label(self, label: Label<'_>) {
        unsafe { self.insert(label.0) };
    }
    fn new_label(self) -> Label<'func> {
        let insn = unsafe { ffi::MIR_new_label(self.get_raw_ctx()) };
        unsafe { self.insert(insn) };
        Label(insn, PhantomData)
    }
}

fn build_insn<'func, 'o>(
    this: impl InsnBuilder<'func>,
    code: mir_sys::MIR_insn_code_t,
    ops: impl IntoIterator<Item = Operand<'o>>,
) {
    let ctx = this.get_raw_ctx();
    let ops = ops.into_iter().collect::<SmallVec<[Operand; 6]>>();
    let insn = unsafe {
        ffi::MIR_new_insn_arr(
            ctx,
            code,
            ops.len(),
            ops.as_ptr().cast::<ffi::MIR_op_t>().cast_mut(),
        )
    };
    unsafe { this.insert(insn) };
}
