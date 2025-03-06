use std::ffi::{CStr, c_char};
use std::marker::PhantomData;
use std::ptr::{NonNull, null_mut};
use std::{fmt, ops};

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

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Ty(pub(crate) ffi::MIR_type_t);

macro_rules! impl_ty_variants {
    ($($var:ident),* $(,)?) => {
        impl Ty {
            $(pub const $var: Self = Self(paste!(ffi::[<MIR_T_ $var>]));)*
        }

        impl fmt::Debug for Ty {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let s = match *self {
                    $(Self::$var => concat!("Ty(MIR_T_", stringify!($var), ")"),)*
                    Self(raw) => return f.debug_tuple("Ty").field(&raw).finish(),
                };
                f.write_str(s)
            }
        }
    };
}

impl_ty_variants! {
    I8, U8, I16, U16, I32, U32, I64, U64,
    F, D, LD, P,
    BLK, RBLK, UNDEF, BOUND,
}

#[test]
fn ty_debug() {
    assert_eq!(format!("{:?}", Ty::I64), "Ty(MIR_T_I64)");
    assert_eq!(format!("{:?}", Ty(99)), "Ty(99)");
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Reg(pub(crate) ffi::MIR_reg_t);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ItemRef<'a>(
    pub(crate) NonNull<ffi::MIR_item>,
    pub(crate) PhantomData<&'a ffi::MIR_item>,
);

impl ItemRef<'_> {
    pub(crate) unsafe fn from_raw(raw: *mut ffi::MIR_item) -> Self {
        let raw = NonNull::new(raw).expect("item must not be null");
        Self(raw, PhantomData)
    }

    pub fn as_raw(&self) -> *mut ffi::MIR_item {
        self.0.as_ptr()
    }
}

impl<'a> From<ItemRef<'a>> for Operand<'a> {
    fn from(item: ItemRef<'a>) -> Self {
        Operand {
            op: unsafe { ffi::MIR_new_ref_op(null_mut(), item.as_raw()) },
            _marker: PhantomData,
        }
    }
}

impl fmt::Debug for ItemRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fs = f.debug_struct("ItemRef");
        let item = unsafe { self.0.as_ref() };
        fs.field("ptr", &std::ptr::from_ref(self))
            .field("module", &item.module)
            .field("addr", &item.addr)
            .field("type", &item.item_type);
        let data = &item.u;
        let mut cb = |data| {
            fs.field("u", data);
        };
        match item.item_type {
            ffi::MIR_func_item => cb(unsafe { &*data.func.cast::<FuncItemData>() }),
            ffi::MIR_proto_item => cb(unsafe { &*data.func.cast::<ProtoItemData>() }),
            ffi::MIR_import_item => cb(&DebugImportLikeItemData(
                "ImportItemData",
                "import_id",
                unsafe { data.import_id },
            )),
            ffi::MIR_export_item => cb(&DebugImportLikeItemData(
                "ExportItemData",
                "export_id",
                unsafe { data.export_id },
            )),
            ffi::MIR_forward_item => cb(&DebugImportLikeItemData(
                "ForwardItemData",
                "forward_id",
                unsafe { data.forward_id },
            )),
            ffi::MIR_data_item => cb(unsafe { &*data.data.cast::<DataItemData>() }),
            ffi::MIR_ref_data_item => cb(unsafe { &*data.ref_data.cast::<RefDataItemData>() }),
            ffi::MIR_lref_data_item => {
                cb(unsafe { &*data.lref_data.cast::<LabelRefDataItemData>() })
            }
            ffi::MIR_expr_data_item => cb(unsafe { &*data.expr_data.cast::<ExprDataItemData>() }),
            ffi::MIR_bss_item => cb(unsafe { &*data.bss.cast::<BssItemData>() }),
            _ => {}
        }
        fs.finish_non_exhaustive()
    }
}

macro_rules! def_item_ref_variant {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy)]
        #[repr(transparent)]
        pub struct $name<'a>(pub(crate) ItemRef<'a>);

        impl<'a> ops::Deref for $name<'a> {
            type Target = ItemRef<'a>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'a> From<$name<'a>> for Operand<'a> {
            fn from(item: $name<'a>) -> Self {
                item.0.into()
            }
        }
    };
}

def_item_ref_variant!(FuncItemRef);

impl FuncItemRef<'_> {
    /// # Safety
    /// The returned reference is invalidated if any MIR functions is called.
    pub(crate) unsafe fn data(&self) -> &ffi::MIR_func {
        unsafe { &*self.0.0.as_ref().u.func }
    }
}

#[repr(transparent)]
struct FuncItemData(ffi::MIR_func);

impl fmt::Debug for FuncItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("FuncItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("func_item", &u.func_item)
            .field("original_vars_num", &u.original_insns)
            .field("nres", &u.nres)
            .field("nargs", &u.nargs)
            .field("res_types", &unsafe {
                std::slice::from_raw_parts(u.res_types.cast::<Ty>(), u.nres as usize)
            })
            .field("varargs_p", &(u.vararg_p != 0))
            .field("expr_p", &(u.expr_p != 0))
            .field("jret_p", &(u.jret_p != 0))
            .field("vars", &DebugVars(u.vars))
            .field("global_vars", &DebugVars(u.global_vars))
            .finish_non_exhaustive()
    }
}

#[repr(transparent)]
struct DebugVar(ffi::MIR_var);

impl fmt::Debug for DebugVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Var")
            .field("type", &Ty(self.0.type_))
            .field("name", &unsafe { CStr::from_ptr(self.0.name) })
            // .field("size", &self.0.size) // Contains garbage?
            .finish_non_exhaustive()
    }
}

struct DebugVars(*mut ffi::VARR_MIR_var_t);

impl fmt::Debug for DebugVars {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let vars = if self.0.is_null() {
            &[]
        } else {
            unsafe {
                let varr = &*self.0;
                std::slice::from_raw_parts(varr.varr.cast::<DebugVar>().cast_const(), varr.els_num)
            }
        };
        vars.fmt(f)
    }
}

def_item_ref_variant!(ProtoItemRef);

#[repr(transparent)]
struct ProtoItemData(ffi::MIR_proto);

impl fmt::Debug for ProtoItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("ProtoItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("nres", &u.nres)
            .field("res_types", &unsafe {
                std::slice::from_raw_parts(u.res_types.cast::<Ty>(), u.nres as usize)
            })
            .finish_non_exhaustive()
    }
}

def_item_ref_variant!(ImportItemRef);
def_item_ref_variant!(ExportItemRef);
def_item_ref_variant!(ForwardItemRef);

struct DebugImportLikeItemData(&'static str, &'static str, *const c_char);

impl fmt::Debug for DebugImportLikeItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(self.0)
            .field(self.1, &unsafe { CStr::from_ptr(self.2) })
            .finish()
    }
}

def_item_ref_variant!(DataItemRef);

#[repr(transparent)]
struct DataItemData(ffi::MIR_data);

impl fmt::Debug for DataItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("DataItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("el_type", &Ty(u.el_type))
            .field("nel", &u.nel)
            .finish_non_exhaustive()
    }
}

def_item_ref_variant!(RefDataItemRef);

#[repr(transparent)]
struct RefDataItemData(ffi::MIR_ref_data);

impl fmt::Debug for RefDataItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("RefDataItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("ref_item", &unsafe { ItemRef::from_raw(u.ref_item) })
            .field("disp", &u.disp)
            .field("load_addr", &u.load_addr)
            .finish()
    }
}

def_item_ref_variant!(LabelRefDataItemRef);

#[repr(transparent)]
struct LabelRefDataItemData(ffi::MIR_lref_data);

impl fmt::Debug for LabelRefDataItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("LableRefDataItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("disp", &u.disp)
            .field("load_addr", &u.load_addr)
            .finish()
    }
}

def_item_ref_variant!(ExprDataItemRef);

#[repr(transparent)]
struct ExprDataItemData(ffi::MIR_expr_data);

impl fmt::Debug for ExprDataItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("ExprItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("expr_item", &unsafe { ItemRef::from_raw(u.expr_item) })
            .finish_non_exhaustive()
    }
}

def_item_ref_variant!(BssItemRef);

#[repr(transparent)]
struct BssItemData(ffi::MIR_bss);

impl fmt::Debug for BssItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("BssItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("len", &u.len)
            .finish()
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
    (__impl (_ $(, $src:ident)*) $name:ident $code:expr) => {
        fn $name<'o>(self, $($src: impl IntoOperand<'o>),*) {
            build_insn(self, $code, [$($src.into()),*]);
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

macro_rules! def_call_insn {
    ($($name:ident),* $(,)?) => {
        $(
            fn $name<'o>(
                self,
                proto: ProtoItemRef<'o>,
                func: impl IntoOperand<'o>,
                results: impl IntoIterator<Item = Operand<'o>>,
                args: impl IntoIterator<Item = Operand<'o>>,
            ) {
                build_insn(
                    self,
                    paste!(ffi::[<MIR_ $name:upper>]),
                    [proto.into(), func.into()]
                        .into_iter()
                        .chain(results)
                        .chain(args),
                );
            }
        )*
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
    def_jump_insn!((label); jmp, bo, bno, ubo, ubno);
    def_simple_insn!((_, label_op); jmpi);
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
    fn laddr<'o>(self, dst: impl IntoOutOperand<'o>, label: Label<'_>) {
        build_insn(self, ffi::MIR_LADDR, [dst.into(), label.into()])
    }

    // Call and return.
    def_call_insn!(call, inline, jcall);
    def_simple_insn!((_, v); ret, jret);

    // Function frame.
    def_simple_insn!((dst, len); alloca);
    def_simple_insn!((_, va_list); va_start, va_end);
    def_simple_insn!((dst, va_list, size, block_type); va_block_arg);
    fn va_arg<'o>(self, dst: impl IntoOutOperand<'o>, va_list: impl IntoOperand<'o>, mem: MemOp) {
        build_insn(
            self,
            ffi::MIR_VA_ARG,
            [dst.into(), va_list.into(), mem.into()],
        );
    }

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
