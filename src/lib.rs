use std::cell::Cell;
use std::ffi::CStr;
use std::marker::PhantomData;
use std::ptr::NonNull;

use types::IntoOperands;

pub use mir_sys as ffi;
pub use types::{InstBuilder, IntoOperand, Operand, Reg, Ty, Val};

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

impl MirContext {
    pub fn new() -> Self {
        Self {
            ctx: NonNull::new(ffi::MIR_init()).expect("context must not be NULL"),
            module: Cell::new(None),
            func_item: Cell::new(None),
        }
    }

    pub fn enter_new_module(&self, name: &CStr) -> MirModuleBuilder<'_> {
        assert!(self.module.get().is_none(), "already inside a module");
        let module = unsafe { ffi::MIR_new_module(self.ctx.as_ptr(), name.as_ptr()) };
        self.module
            .set(Some(NonNull::new(module).expect("module must not be null")));
        MirModuleBuilder { ctx: self }
    }

    pub fn load_module(&self, module: MirModule<'_>) {
        unsafe { ffi::MIR_load_module(self.ctx.as_ptr(), module.module.as_ptr()) };
    }

    pub fn link_modules_for_interpreter(&self) {
        unsafe {
            ffi::MIR_link(
                self.ctx.as_ptr(),
                Some(ffi::MIR_set_interp_interface),
                // TODO
                None,
            );
        }
    }

    /// # Safety
    /// TODO
    pub unsafe fn interpret_unchecked(
        &self,
        func_item: MirFuncItem<'_>,
        results: &mut [Val],
        args: &[Val],
    ) {
        let func = unsafe { &*func_item.get_raw_func() };
        debug_assert_eq!(func.nres as usize, results.len());
        debug_assert_eq!(func.nargs as usize, args.len());
        unsafe {
            ffi::MIR_interp_arr(
                self.ctx.as_ptr(),
                func_item.func_item.as_ptr(),
                results.as_mut_ptr().cast::<ffi::MIR_val_t>(),
                args.len(),
                args.as_ptr().cast::<ffi::MIR_val_t>().cast_mut(),
            )
        };
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MirFuncItem<'ctx> {
    func_item: NonNull<ffi::MIR_item>,
    _marker: PhantomData<&'ctx MirContext>,
}

impl MirFuncItem<'_> {
    fn get_raw_func(&self) -> *mut ffi::MIR_func {
        unsafe { self.func_item.as_ref().u.func }
    }
}

impl Drop for MirContext {
    fn drop(&mut self) {
        if self.func_item.get().is_some() {
            // FIXME: This may fail.
            unsafe { ffi::MIR_finish_func(self.ctx.as_ptr()) };
        }
        if self.module.get().is_some() {
            unsafe { ffi::MIR_finish_module(self.ctx.as_ptr()) };
        }
        unsafe { ffi::MIR_finish(self.ctx.as_ptr()) };
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MirModule<'ctx> {
    module: NonNull<ffi::MIR_module>,
    _marker: PhantomData<&'ctx MirContext>,
}

#[derive(Debug)]
pub struct MirModuleBuilder<'ctx> {
    ctx: &'ctx MirContext,
}

impl Drop for MirModuleBuilder<'_> {
    fn drop(&mut self) {
        self.ctx.module.take().expect("must be inside a module");
        unsafe { ffi::MIR_finish_module(self.ctx.ctx.as_ptr()) };
    }
}

impl<'ctx> MirModuleBuilder<'ctx> {
    pub fn finish(self) -> MirModule<'ctx> {
        let module = self.ctx.module.get().expect("must be inside a module");
        drop(self);
        MirModule {
            module,
            _marker: PhantomData,
        }
    }

    pub fn enter_new_function<'module>(
        &'module self,
        name: &CStr,
        ret_types: &[Ty],
        args: &[(&CStr, Ty)],
    ) -> MirFuncBuilder<'module, 'ctx> {
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
                self.ctx.ctx.as_ptr(),
                name.as_ptr(),
                ret_types.len(),
                ret_types.as_ptr().cast::<ffi::MIR_type_t>().cast_mut(),
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
    _marker: PhantomData<&'module MirModule<'ctx>>,
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
    pub fn finish(self) -> MirFuncItem<'ctx> {
        let func_item = self.ctx.func_item.get().expect("must be inside a function");
        drop(self);
        MirFuncItem {
            func_item,
            _marker: PhantomData,
        }
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

    pub fn ins(&mut self) -> FuncInstBuilder<'_, 'ctx> {
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

impl InstBuilder for FuncInstBuilder<'_, '_> {
    #[expect(private_bounds)]
    fn build<const LEN: usize>(
        self,
        code: mir_sys::MIR_insn_code_t,
        ops: impl IntoOperands<Arr = [Operand; LEN]>,
    ) {
        let ops = unsafe { ops.build(self.ctx.ctx.as_ptr()) };
        let insn = unsafe {
            ffi::MIR_new_insn_arr(
                self.ctx.ctx.as_ptr(),
                code,
                LEN,
                ops.as_ptr().cast::<ffi::MIR_op_t>().cast_mut(),
            )
        };
        unsafe {
            ffi::MIR_append_insn(
                self.ctx.ctx.as_ptr(),
                self.ctx
                    .func_item
                    .get()
                    .expect("must be inside a function")
                    .as_ptr()
                    .cast::<ffi::MIR_item>(),
                insn,
            )
        };
    }
}
