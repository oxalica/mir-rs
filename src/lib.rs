use std::cell::Cell;
use std::ffi::{CStr, c_char, c_void};
use std::marker::PhantomData;
use std::ptr::{NonNull, null, null_mut};

pub use codegen::MirGenContext;
use mem_file::MemoryFile;
pub use mir_sys as ffi;
pub use types::{
    BssItemRef, DataItemRef, ExportItemRef, ExprDataItemRef, ForwardItemRef, FuncItemRef,
    ImportItemRef, InsnBuilder, InsnBuilderBase, IntoOperand, ItemRef, Label, LabelRefDataItemRef,
    MemOp, Operand, ProtoItemRef, RefDataItemRef, Reg, Ty, Val,
};

mod codegen;
mod mem_file;
mod types;

#[derive(Debug)]
pub struct MirContext {
    ctx: NonNull<ffi::MIR_context>,
    module: Cell<Option<NonNull<ffi::MIR_module>>>,
    func_item: Cell<Option<NonNull<ffi::MIR_item>>>,
}

impl Default for MirContext {
    fn default() -> Self {
        Self::new()
    }
}

unsafe extern "C-unwind" {
    fn MIRRS_error_handler_trampoline(
        error_type: ffi::MIR_error_type_t,
        format: *const c_char,
        ...
    ) -> !;
}

#[unsafe(no_mangle)]
unsafe extern "C-unwind" fn MIRRS_error_handler_rust(
    error_type: ffi::MIR_error_type_t,
    msg: *const u8,
    len: usize,
) -> ! {
    let msg = String::from_utf8_lossy(unsafe { std::slice::from_raw_parts(msg, len) });
    panic!("mir error {error_type}: {msg}");
}

type ImportResolver = dyn Fn(&CStr) -> *mut c_void;

impl MirContext {
    pub fn new() -> Self {
        let ctx = ffi::MIR_init();
        unsafe { ffi::MIR_set_error_func(ctx, Some(MIRRS_error_handler_trampoline)) };
        Self {
            ctx: NonNull::new(ctx).expect("context must not be NULL"),
            module: Cell::new(None),
            func_item: Cell::new(None),
        }
    }

    pub fn as_raw(&self) -> *mut ffi::MIR_context {
        self.ctx.as_ptr()
    }

    pub fn enter_new_module(&self, name: &CStr) -> MirModuleBuilder<'_> {
        assert!(self.module.get().is_none(), "already inside a module");
        let module = unsafe { ffi::MIR_new_module(self.as_raw(), name.as_ptr()) };
        self.module
            .set(Some(NonNull::new(module).expect("module must not be null")));
        MirModuleBuilder { ctx: self }
    }

    pub fn load_module(&self, module: MirModuleRef<'_>) {
        unsafe { ffi::MIR_load_module(self.as_raw(), module.module.as_ptr()) };
    }

    /// # Safety
    /// `addr` must be a valid function pointer with matching prototype.
    pub unsafe fn load_external(&self, name: &CStr, addr: *mut c_void) {
        unsafe { ffi::MIR_load_external(self.as_raw(), name.as_ptr(), addr) };
    }

    /// # Safety
    /// `resolver` must return valid function pointers with matching prototype.
    pub(crate) unsafe fn link_modules(
        &self,
        set_interface: Option<
            unsafe extern "C-unwind" fn(ctx: ffi::MIR_context_t, item: ffi::MIR_item_t),
        >,
        resolver: Option<&ImportResolver>,
    ) {
        thread_local! {
            static RESOLVER_CALLBACK: Cell<Option<NonNull<ImportResolver>>> = const { Cell::new(None) };
        }

        unsafe extern "C-unwind" fn trampoline(name: *const c_char) -> *mut c_void {
            RESOLVER_CALLBACK.with(|cb| {
                let cb = unsafe { cb.get().expect("resolver must be set").as_ref() };
                cb(unsafe { CStr::from_ptr(name) })
            })
        }

        match resolver {
            Some(resolver) => RESOLVER_CALLBACK.with(|cb| {
                assert!(cb.get().is_none(), "cannot link recursively");
                cb.set(Some(NonNull::from(resolver)));
                unsafe { ffi::MIR_link(self.as_raw(), set_interface, Some(trampoline)) };
                cb.set(None);
            }),
            None => unsafe { ffi::MIR_link(self.as_raw(), set_interface, None) },
        }
    }

    pub fn link_modules_for_interpret(&self) {
        unsafe { self.link_modules(Some(ffi::MIR_set_interp_interface), None) }
    }

    /// # Safety
    /// `resolver` must return valid function pointers with matching prototype.
    pub unsafe fn link_modules_for_interpret_with_resolver(&self, resolver: &ImportResolver) {
        unsafe { self.link_modules(Some(ffi::MIR_set_interp_interface), Some(resolver)) }
    }

    /// # Safety
    /// TODO
    pub unsafe fn interpret_unchecked(
        &self,
        func: FuncItemRef<'_>,
        results: &mut [Val],
        args: &[Val],
    ) {
        let data = unsafe { func.data() };
        debug_assert_eq!(data.nres as usize, results.len());
        debug_assert_eq!(data.nargs as usize, args.len());
        unsafe {
            ffi::MIR_interp_arr(
                self.as_raw(),
                func.as_raw(),
                results.as_mut_ptr().cast::<ffi::MIR_val_t>(),
                args.len(),
                args.as_ptr().cast::<ffi::MIR_val_t>().cast_mut(),
            )
        };
    }
}

impl Drop for MirContext {
    fn drop(&mut self) {
        if std::thread::panicking() {
            // MIR_finish* may fail. Avoid double-panicking when something already goes wrong.
            return;
        }

        if self.func_item.get().is_some() {
            unsafe { ffi::MIR_finish_func(self.as_raw()) };
        }
        if self.module.get().is_some() {
            unsafe { ffi::MIR_finish_module(self.as_raw()) };
        }
        unsafe { ffi::MIR_finish(self.as_raw()) };
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MirModuleRef<'ctx> {
    module: NonNull<ffi::MIR_module>,
    _marker: PhantomData<&'ctx MirContext>,
}

impl MirModuleRef<'_> {
    pub fn as_raw(&self) -> *mut ffi::MIR_module {
        self.module.as_ptr()
    }

    pub fn dump(&self, ctx: &MirContext) -> String {
        MemoryFile::with(|file| unsafe {
            ffi::MIR_output_module(ctx.as_raw(), file, self.as_raw())
        })
        .1
    }
}

#[derive(Debug)]
pub struct MirModuleBuilder<'ctx> {
    ctx: &'ctx MirContext,
}

impl Drop for MirModuleBuilder<'_> {
    fn drop(&mut self) {
        self.ctx.module.take().expect("must be inside a module");
        unsafe { ffi::MIR_finish_module(self.as_raw_ctx()) };
    }
}

impl<'ctx> MirModuleBuilder<'ctx> {
    pub fn finish(self) -> MirModuleRef<'ctx> {
        let module = self.ctx.module.get().expect("must be inside a module");
        drop(self);
        MirModuleRef {
            module,
            _marker: PhantomData,
        }
    }

    fn as_raw_ctx(&self) -> *mut ffi::MIR_context {
        self.ctx.as_raw()
    }

    pub fn add_proto(&self, name: &CStr, rets: &[Ty], args: &[(&CStr, Ty)]) -> ProtoItemRef<'_> {
        let c_args = args
            .iter()
            .map(|(name, ty)| ffi::MIR_var {
                type_: ty.0,
                name: name.as_ptr(),
                // Unused.
                size: 0,
            })
            .collect::<Vec<_>>();
        let item = unsafe {
            ItemRef::from_raw(ffi::MIR_new_proto_arr(
                self.as_raw_ctx(),
                name.as_ptr(),
                rets.len(),
                rets.as_ptr().cast::<ffi::MIR_type_t>().cast_mut(),
                c_args.len(),
                c_args.as_ptr().cast_mut(),
            ))
        };
        ProtoItemRef(item)
    }

    pub fn add_import(&self, name: &CStr) -> ImportItemRef<'_> {
        let item =
            unsafe { ItemRef::from_raw(ffi::MIR_new_import(self.as_raw_ctx(), name.as_ptr())) };
        ImportItemRef(item)
    }

    pub fn add_export(&self, name: &CStr) -> ExportItemRef<'_> {
        let item =
            unsafe { ItemRef::from_raw(ffi::MIR_new_export(self.as_raw_ctx(), name.as_ptr())) };
        ExportItemRef(item)
    }

    pub fn add_forward(&self, name: &CStr) -> ForwardItemRef<'_> {
        let item =
            unsafe { ItemRef::from_raw(ffi::MIR_new_forward(self.as_raw_ctx(), name.as_ptr())) };
        ForwardItemRef(item)
    }

    pub fn add_data<'a>(&self, name: impl Into<Option<&'a CStr>>, data: &[u8]) -> DataItemRef<'_> {
        unsafe {
            DataItemRef(ItemRef::from_raw(ffi::MIR_new_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), |s| s.as_ptr()),
                Ty::U8.0,
                data.len(),
                data.as_ptr().cast(),
            )))
        }
    }

    pub fn add_ref_data<'a>(
        &self,
        name: impl Into<Option<&'a CStr>>,
        ref_item: ItemRef<'_>,
        disp: i64,
    ) -> RefDataItemRef<'_> {
        unsafe {
            RefDataItemRef(ItemRef::from_raw(ffi::MIR_new_ref_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), |s| s.as_ptr()),
                ref_item.as_raw(),
                disp,
            )))
        }
    }

    pub fn add_expr_data<'a>(
        &self,
        name: impl Into<Option<&'a CStr>>,
        expr_func: FuncItemRef<'_>,
    ) -> ExprDataItemRef<'_> {
        unsafe {
            ExprDataItemRef(ItemRef::from_raw(ffi::MIR_new_expr_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), |s| s.as_ptr()),
                expr_func.as_raw(),
            )))
        }
    }

    pub fn add_label_ref_data<'a>(
        &self,
        name: impl Into<Option<&'a CStr>>,
        label: Label<'_>,
        base_label: Option<Label<'_>>,
        disp: i64,
    ) -> LabelRefDataItemRef<'_> {
        unsafe {
            LabelRefDataItemRef(ItemRef::from_raw(ffi::MIR_new_lref_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), |s| s.as_ptr()),
                label.0,
                base_label.map_or(null_mut(), |lbl| lbl.0),
                disp,
            )))
        }
    }

    pub fn add_bss<'a>(&self, name: impl Into<Option<&'a CStr>>, len: usize) -> BssItemRef<'_> {
        unsafe {
            BssItemRef(ItemRef::from_raw(ffi::MIR_new_bss(
                self.as_raw_ctx(),
                name.into().map_or(null(), |s| s.as_ptr()),
                len,
            )))
        }
    }

    pub fn enter_new_function(
        &'_ self,
        name: &CStr,
        rets: &[Ty],
        args: &[(&CStr, Ty)],
    ) -> MirFuncBuilder<'_, 'ctx> {
        assert!(
            self.ctx.func_item.get().is_none(),
            "already inside a function"
        );
        let c_args = args
            .iter()
            .map(|(name, ty)| ffi::MIR_var {
                type_: ty.0,
                name: name.as_ptr(),
                // Unused.
                size: 0,
            })
            .collect::<Vec<_>>();
        let func_item = unsafe {
            ffi::MIR_new_func_arr(
                self.as_raw_ctx(),
                name.as_ptr(),
                rets.len(),
                rets.as_ptr().cast::<ffi::MIR_type_t>().cast_mut(),
                c_args.len(),
                c_args.as_ptr().cast_mut(),
            )
        };
        self.ctx.func_item.set(Some(
            NonNull::new(func_item).expect("item must not be null"),
        ));
        let func = unsafe { NonNull::new((*func_item).u.func).expect("function must not be null") };
        MirFuncBuilder {
            func,
            ctx: self.ctx,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct MirFuncBuilder<'module, 'ctx> {
    func: NonNull<ffi::MIR_func>,
    ctx: &'ctx MirContext,
    _marker: PhantomData<&'module MirModuleRef<'ctx>>,
}

impl Drop for MirFuncBuilder<'_, '_> {
    fn drop(&mut self) {
        self.ctx
            .func_item
            .take()
            .expect("must be inside a function");
        unsafe { ffi::MIR_finish_func(self.ctx.ctx.as_ptr()) };
    }
}

impl<'ctx> MirFuncBuilder<'_, 'ctx> {
    pub fn finish(self) -> FuncItemRef<'ctx> {
        let func_item = self.ctx.func_item.get().expect("must be inside a function");
        drop(self);
        FuncItemRef(ItemRef(func_item, PhantomData))
    }

    pub fn get_reg(&self, name: &CStr) -> Reg {
        let reg = unsafe { ffi::MIR_reg(self.ctx.ctx.as_ptr(), name.as_ptr(), self.func.as_ptr()) };
        Reg(reg)
    }

    pub fn new_local_reg(&self, name: &CStr, ty: Ty) -> Reg {
        let reg = unsafe {
            ffi::MIR_new_func_reg(
                self.ctx.ctx.as_ptr(),
                self.func.as_ptr(),
                ty.0,
                name.as_ptr(),
            )
        };
        Reg(reg)
    }

    pub fn new_label(&self) -> Label<'_> {
        let insn = unsafe { ffi::MIR_new_label(self.ctx.ctx.as_ptr()) };
        Label(insn, PhantomData)
    }

    pub fn ins(&self) -> FuncInstBuilder<'_, 'ctx> {
        FuncInstBuilder {
            ctx: self.ctx,
            _marker: PhantomData,
        }
    }
}

pub struct FuncInstBuilder<'func, 'ctx> {
    ctx: &'ctx MirContext,
    _marker: PhantomData<&'func MirFuncBuilder<'func, 'ctx>>,
}

unsafe impl<'func> InsnBuilderBase<'func> for FuncInstBuilder<'func, '_> {
    fn get_raw_ctx(&self) -> ffi::MIR_context_t {
        self.ctx.ctx.as_ptr()
    }

    unsafe fn insert(self, insn: ffi::MIR_insn_t) {
        unsafe {
            ffi::MIR_append_insn(
                self.ctx.as_raw(),
                self.ctx
                    .func_item
                    .get()
                    .expect("must be inside a function")
                    .as_ptr()
                    .cast::<ffi::MIR_item>(),
                insn,
            );
        }
    }
}
