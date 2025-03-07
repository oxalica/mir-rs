use std::ffi::{CStr, c_char};
use std::marker::PhantomData;
use std::ptr::{NonNull, null_mut};
use std::{fmt, ops};

use paste::paste;
use smallvec::SmallVec;

use crate::{MemoryFile, MirContext, ffi};

/// Type of virtual registers.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Ty(pub(crate) ffi::MIR_type_t);

macro_rules! impl_ty_variants {
    ($($var:ident),* $(,)?) => {
        impl Ty {
            $(
                #[doc = concat!("`MIR_T_", stringify!($var), "`")]
                pub const $var: Self = Self(paste!(ffi::[<MIR_T_ $var>]));
            )*
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

/// Virtual register identifier.
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Reg(pub(crate) ffi::MIR_reg_t);

/// Item type.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ItemType(pub(crate) ffi::MIR_item_type_t);

macro_rules! impl_item_type_variants {
    ($($var:ident),* $(,)?) => {
        impl ItemType {
            $(
                #[doc = paste!(concat!("`MIR_", stringify!([<$var:lower>]), "_item"))]
                pub const $var: Self = Self(paste!(ffi::[<MIR_ $var:lower _item>]));
            )*
        }

        impl fmt::Debug for ItemType {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let s = match *self {
                    $(Self::$var => paste!(concat!("ItemType(MIR_", stringify!($var:lower), "_item)")),)*
                    Self(raw) => return f.debug_tuple("ItemType").field(&raw).finish(),
                };
                f.write_str(s)
            }
        }
    };
}

impl_item_type_variants! {
    FUNC, PROTO,
    IMPORT, EXPORT, FORWARD,
    DATA, REF_DATA, LREF_DATA, EXPR_DATA,
    BSS,
}

/// Reference to an item.
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

    /// Get the underlying pointer for FFI.
    #[must_use]
    pub fn as_raw(&self) -> *mut ffi::MIR_item {
        self.0.as_ptr()
    }

    /// Dump the content of the context in a textual representation for human consumption.
    #[must_use]
    pub fn dump(&self, ctx: &MirContext) -> String {
        MemoryFile::with(|file| unsafe { ffi::MIR_output_item(ctx.as_raw(), file, self.as_raw()) })
            .1
    }

    /// Get the item type.
    #[must_use]
    pub fn type_(&self) -> ItemType {
        unsafe { ItemType(self.0.as_ref().item_type) }
    }

    /// Get the name of the item.
    #[must_use]
    pub fn name(&self) -> Option<&CStr> {
        let item = unsafe { self.0.as_ref() };
        let data = &item.u;
        let ptr = unsafe {
            match item.item_type {
                ffi::MIR_func_item => (*data.func.cast::<FuncItemData>()).0.name,
                ffi::MIR_proto_item => (*data.func.cast::<ProtoItemData>()).0.name,
                ffi::MIR_import_item => data.import_id,
                ffi::MIR_export_item => data.export_id,
                ffi::MIR_forward_item => data.forward_id,
                ffi::MIR_data_item => (*data.data.cast::<DataItemData>()).0.name,
                ffi::MIR_ref_data_item => (*data.ref_data.cast::<RefDataItemData>()).0.name,
                ffi::MIR_lref_data_item => (*data.lref_data.cast::<LabelRefDataItemData>()).0.name,
                ffi::MIR_expr_data_item => (*data.expr_data.cast::<ExprDataItemData>()).0.name,
                ffi::MIR_bss_item => (*data.bss.cast::<BssItemData>()).0.name,
                _ => return None,
            }
        };
        if ptr.is_null() {
            None
        } else {
            unsafe { Some(CStr::from_ptr(ptr)) }
        }
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
            .field("type", &self.type_());
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
                cb(unsafe { &*data.lref_data.cast::<LabelRefDataItemData>() });
            }
            ffi::MIR_expr_data_item => cb(unsafe { &*data.expr_data.cast::<ExprDataItemData>() }),
            ffi::MIR_bss_item => cb(unsafe { &*data.bss.cast::<BssItemData>() }),
            _ => {}
        }
        fs.finish_non_exhaustive()
    }
}

/// Item downcast failure.
#[derive(Debug, Clone)]
pub struct ItemRefDowncastError(());

impl fmt::Display for ItemRefDowncastError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("item type mismatch")
    }
}

impl std::error::Error for ItemRefDowncastError {}

macro_rules! def_item_ref_variant {
    ($name:ident, $item_type:expr, $doc:literal) => {
        /// Reference to a
        #[doc = $doc]
        /// item.
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

        impl<'a> TryFrom<ItemRef<'a>> for $name<'a> {
            type Error = ItemRefDowncastError;
            fn try_from(item: ItemRef<'a>) -> Result<Self, Self::Error> {
                if unsafe { item.0.as_ref().item_type } == $item_type {
                    Ok(Self(item))
                } else {
                    Err(ItemRefDowncastError(()))
                }
            }
        }
    };
}

def_item_ref_variant!(FuncItemRef, ffi::MIR_func_item, "function");

impl FuncItemRef<'_> {
    /// # Safety
    /// The returned reference is invalidated if any MIR functions is called.
    #[cfg(feature = "interp")]
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

def_item_ref_variant!(ProtoItemRef, ffi::MIR_proto_item, "prototype");

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

def_item_ref_variant!(ImportItemRef, ffi::MIR_import_item, "import");
def_item_ref_variant!(ExportItemRef, ffi::MIR_export_item, "export");
def_item_ref_variant!(ForwardItemRef, ffi::MIR_forward_item, "forward declaration");

struct DebugImportLikeItemData(&'static str, &'static str, *const c_char);

impl fmt::Debug for DebugImportLikeItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(self.0)
            .field(self.1, &unsafe { CStr::from_ptr(self.2) })
            .finish()
    }
}

def_item_ref_variant!(DataItemRef, ffi::MIR_data_item, "data");

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

def_item_ref_variant!(RefDataItemRef, ffi::MIR_ref_data_item, "referential data");

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

def_item_ref_variant!(LabelRefDataItemRef, ffi::MIR_lref_data_item, "label data");

#[repr(transparent)]
struct LabelRefDataItemData(ffi::MIR_lref_data);

impl fmt::Debug for LabelRefDataItemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        f.debug_struct("LabelRefDataItemData")
            .field("name", &unsafe { CStr::from_ptr(u.name) })
            .field("disp", &u.disp)
            .field("load_addr", &u.load_addr)
            .finish()
    }
}

def_item_ref_variant!(ExprDataItemRef, ffi::MIR_expr_data_item, "computed data");

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

def_item_ref_variant!(BssItemRef, ffi::MIR_bss_item, "writable memory segment");

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

/// Operand of instructions.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Operand<'a> {
    pub(crate) op: ffi::MIR_op_t,
    pub(crate) _marker: PhantomData<&'a ()>,
}

impl fmt::Debug for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("Operand");
        unsafe {
            match ffi::MIR_op_mode_t::from(self.op.mode) {
                ffi::MIR_OP_REG => s.field("reg", &self.op.u.reg),
                ffi::MIR_OP_VAR => s.field("var", &self.op.u.var),
                ffi::MIR_OP_INT => s.field("int", &self.op.u.i),
                ffi::MIR_OP_UINT => s.field("uint", &self.op.u.u),
                ffi::MIR_OP_FLOAT => s.field("float", &self.op.u.f),
                ffi::MIR_OP_DOUBLE => s.field("double", &self.op.u.d),
                ffi::MIR_OP_REF => s.field("ref", &ItemRef::from_raw(self.op.u.ref_)),
                ffi::MIR_OP_MEM => s.field("mem", &self.op.u.mem),
                ffi::MIR_OP_LABEL => s.field("label", &..),
                // Not implemented yet.
                // ffi::MIR_OP_STR
                // ffi::MIR_OP_VAR_MEM
                // ffi::MIR_OP_BOUND
                _ => return s.finish_non_exhaustive(),
            }
            .finish()
        }
    }
}

/// Convertible to [`Operand`].
pub trait IntoOperand<'a>: Into<Operand<'a>> {}

/// Convertible to [`Operand`], but can only be used in destination place.
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

/// Label identifier.
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Label<'a>(pub(crate) ffi::MIR_label_t, pub(crate) PhantomData<&'a ()>);

impl<'a> From<Label<'a>> for Operand<'a> {
    fn from(label: Label<'a>) -> Self {
        Self {
            op: unsafe { ffi::MIR_new_label_op(null_mut(), label.0) },
            _marker: PhantomData,
        }
    }
}

/// Memory operand.
///
/// It represents the data located at address `(disp + base_reg + index_reg * scale)`,
/// where `disp` is a constant and `scale` is either 1, 2, 4 or 8.
#[derive(Debug, Clone, Copy)]
pub struct MemOp {
    ty: Ty,
    disp: i64,
    base: Reg,
    index: Reg,
    scale: u8,
}

impl MemOp {
    /// Create a new memory operand with type `ty`, based on register `base`.
    #[must_use]
    pub fn new_base(ty: Ty, base: Reg) -> Self {
        Self {
            ty,
            disp: 0,
            base,
            index: Reg(0),
            scale: 1,
        }
    }

    /// Set the displacement and return updated operand.
    #[must_use]
    pub fn disp(self, disp: i64) -> Self {
        Self { disp, ..self }
    }

    /// Set the index register and return updated operand.
    #[must_use]
    pub fn index(self, index: Reg) -> Self {
        Self { index, ..self }
    }

    /// Set the scale and return updated operand.
    ///
    /// # Panics
    ///
    /// Panics if `scale` is not 1, 2, 4 or 8.
    #[must_use]
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

/// !! Not a public API !!
///
/// # Safety
/// `get_raw_ctx` must return a valid MIR context.
#[doc(hidden)]
pub unsafe trait InsnBuilderBase<'func>: Sized {
    fn get_raw_ctx(&self) -> ffi::MIR_context_t;
    /// # Safety
    /// `insn` must be a valid instruction that is not inserted anywhere.
    unsafe fn insert(self, insn: ffi::MIR_insn_t);
}

impl<'func, T: InsnBuilderBase<'func>> InsnBuilder<'func> for T {}

/// The instruction builder.
///
/// See detail instruction semantics in [MIR documentation][upstream-docs].
///
/// [upstream-docs]: https://github.com/vnmakarov/mir/blob/v1.0.0/MIR.md#mir-insns
///
/// # Example
///
/// ```
/// # fn codegen(f: &mir::MirFuncBuilder<'_, '_>, [sum, a, b]: [mir::Reg; 3]) {
/// use mir::InsnBuilder as _;
/// // let f: &MirFuncBuilder<'_, '_>;
/// // let a, b, c: Reg;
/// f.ins().add(sum, a, b);
/// f.ins().ret(sum);
/// # }
/// ```
#[allow(missing_docs)]
pub trait InsnBuilder<'func>: InsnBuilderBase<'func> {
    // Mostly follows the order in mir.h.

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
        build_insn(self, ffi::MIR_LADDR, [dst.into(), label.into()]);
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
    /// Bind (insert) a previously unbound label to the current location.
    fn label(self, label: Label<'_>) {
        unsafe { self.insert(label.0) };
    }
    /// Create a new label on the current location (immediately insert).
    fn new_label(self) -> Label<'func> {
        let insn = unsafe { ffi::MIR_new_label(self.get_raw_ctx()) };
        unsafe { self.insert(insn) };
        Label(insn, PhantomData)
    }
}

fn build_insn<'func, 'o>(
    this: impl InsnBuilderBase<'func>,
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
