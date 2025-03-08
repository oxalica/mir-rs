#![cfg_attr(docsrs, feature(doc_auto_cfg))]
//! # [MIR project][mir] bindings for Rust
//!
//! [mir]: https://github.com/vnmakarov/mir
//!
//! ## Quick start
//!
//! > You can also check [`tests/smoke.rs`][more-example] for more examples.
//!
//! [more-example]: https://github.com/oxalica/mir-rs/blob/main/tests/smoke.rs
//!
//! ### Construct a function and execute it via MIR interpreter
//!
//! ```
//! # #[cfg(feature = "interp")] {
//! use mir::{InsnBuilder, MirContext, Ty, Val};
//!
//! // Initialize a context.
//! let ctx = MirContext::new();
//!
//! // Create a module, a function, append instructions, and finally sealing them.
//! let m = ctx.enter_new_module(c"module_add");
//! // Note that MIR requires all argument types to be I64.
//! // extern "C" fn add(a: i64, b: i64) -> i64
//! let f = m.enter_new_function(c"add", &[Ty::I64], &[(c"a", Ty::I64), (c"b", Ty::I64)]);
//! let a = f.get_reg(c"a");
//! let b = f.get_reg(c"b");
//! let ret = f.new_local_reg(c"ret", Ty::I64);
//! f.ins().add(ret, a, b);
//! f.ins().ret(ret);
//! let func = f.finish();
//! let module = m.finish();
//!
//! // Load and link modules.
//! ctx.load_module(module);
//! ctx.link_modules_for_interpret();
//!
//! // Execute our functions.
//! let mut ret = [Val::default()];
//! unsafe { ctx.interpret_unchecked(func, &mut ret, &[Val::from(40i64), Val::from(2i64)]) };
//! assert_eq!(ret[0].as_i64(), 42);
//! # }
//! ```
//!
//! ### Codegen a function to native code and execute it natively
//!
//! ```
//! # #[cfg(feature = "gen")] {
//! use mir::{InsnBuilder, MirContext, MirGenContext, Ty};
//!
//! // Initialize a context and codegen context.
//! let ctx = MirGenContext::new(MirContext::new());
//!
//! // Same creation code.
//! let m = ctx.enter_new_module(c"module_add");
//! // ...
//! # let f = m.enter_new_function(c"add", &[Ty::I64], &[(c"a", Ty::I64), (c"b", Ty::I64)]);
//! # let a = f.get_reg(c"a");
//! # let b = f.get_reg(c"b");
//! # let ret = f.new_local_reg(c"ret", Ty::I64);
//! # f.ins().add(ret, a, b);
//! # f.ins().ret(ret);
//! # let func = f.finish();
//! let module = m.finish();
//!
//! // Set optimization level and/or other configurables.
//! ctx.set_opt_level(3);
//! // Load and link modules, for codegen.
//! ctx.load_module(module);
//! ctx.link_modules_for_codegen();
//!
//! // Codegen and get a pointer to generated function.
//! let func_ptr = ctx.codegen_func(func);
//! type AddFunc = extern "C" fn(a: i64, b: i64) -> i64;
//! let func_ptr = unsafe { std::mem::transmute::<*mut _, AddFunc>(func_ptr) };
//!
//! // Call it!
//! assert_eq!(func_ptr(40, 2), 42);
//! # }
//! ```
//!
//! ## Panics and errors
//!
//! Unfortunately MIR [treats all errors as fatal errors][mir-error-issue] and is likely to
//! continue be like this.
//! In mir-rs, we did a best-effort recovery to unwind in error callback inside C code via
//! ["C-unwind" ABI][c-unwind]. This is always safe because all intermediate C frames are
//! [Plain Old Frames][pof]. But it may leave the C states inconsistent, thus it is strongly
//! recommended to drop the [`MirContext`] if any modification method panics.
//!
//! [c-unwind]: https://rust-lang.github.io/rfcs/2945-c-unwind-abi.html
//! [pof]: https://rust-lang.github.io/rfcs/2945-c-unwind-abi.html#plain-old-frames
//! [mir-error-issue]: https://github.com/vnmakarov/mir/issues/220
//!
//! ## Features
//!
//! - `default`: Implies `io`, `interp`, `gen`.
//!
//! - `io`: De/serialization of MIR memory representation into/from bytes.
//!
//! - `interp`: Enables MIR interpreter.
//!
//! - `gen`: MIR native code generator.
//!
//! - `gen-debug`: Debug logging in MIR native code generator. It implies `gen`.
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
use std::cell::Cell;
use std::ffi::{CStr, c_char, c_void};
use std::marker::PhantomData;
use std::ptr::{self, NonNull, null, null_mut};

use mem_file::MemoryFile;
use types::InsnBuilderBase;

pub use mir_sys as ffi;
pub use types::{
    BssItemRef, DataItemRef, ExportItemRef, ExprDataItemRef, ForwardItemRef, FuncItemRef,
    ImportItemRef, InsnBuilder, IntoOperand, IntoOutOperand, ItemRef, ItemType, Label,
    LabelRefDataItemRef, MemOp, Operand, ProtoItemRef, RefDataItemRef, Reg, Ty,
};

#[cfg(feature = "gen")]
mod codegen;
#[cfg(feature = "gen")]
pub use codegen::MirGenContext;

#[cfg(feature = "interp")]
mod interp;
#[cfg(feature = "interp")]
pub use interp::Val;

mod mem_file;
mod types;

#[cfg(any(test, doctest))]
mod tests;

/// The context for code generation, linking and interpreter.
///
/// Almost all MIR functionality requires an initialized context to work.
/// The context is not thread-safe.
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

#[cfg(feature = "io")]
unsafe extern "C-unwind" fn write_byte_callback(data: *mut libc::c_void, byte: u8) -> libc::c_int {
    let data = unsafe { &mut *data.cast::<Vec<_>>() };
    data.push(byte);
    1
}

#[cfg(feature = "io")]
unsafe extern "C-unwind" fn read_byte_callback(data: *mut libc::c_void) -> libc::c_int {
    let data = unsafe { &mut *data.cast::<&[u8]>() };
    match data.split_first() {
        Some((byte, rest)) => {
            *data = rest;
            (*byte).into()
        }
        None => libc::EOF,
    }
}

type ImportResolver = dyn Fn(&CStr) -> *mut c_void;

impl MirContext {
    /// Initialize a new MIR context.
    #[expect(clippy::missing_panics_doc, reason = "assertion")]
    pub fn new() -> Self {
        let ctx = ffi::MIR_init();
        unsafe { ffi::MIR_set_error_func(ctx, Some(MIRRS_error_handler_trampoline)) };
        Self {
            ctx: NonNull::new(ctx).expect("context must not be NULL"),
            module: Cell::new(None),
            func_item: Cell::new(None),
        }
    }

    /// Get the underlying pointer for FFI.
    pub fn as_raw(&self) -> *mut ffi::MIR_context {
        self.ctx.as_ptr()
    }

    /// Dump the content in a textual representation for human consumption.
    #[must_use]
    pub fn dump(&self) -> String {
        MemoryFile::with(|file| unsafe { ffi::MIR_output(self.as_raw(), file) }).1
    }

    /// Get the list of all modules in the context.
    ///
    /// This includes all modules created in this context, not necessarily loaded or linked.
    pub fn get_modules(&self) -> Vec<MirModuleRef<'_>> {
        let head = unsafe { (*ffi::MIR_get_module_list(self.as_raw())).head };
        std::iter::successors(NonNull::new(head), |m| unsafe {
            NonNull::new(m.as_ref().module_link.next)
        })
        .map(|module| unsafe { MirModuleRef::from_raw(module.as_ptr()) })
        .collect()
    }

    /// Serialize the context (including all modules) into bytes.
    ///
    /// The serialization format is stable across executions, but may not be stable across
    /// MIR versions.
    #[cfg(feature = "io")]
    pub fn serialize(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        unsafe {
            ffi::MIR_write_with_func(
                self.as_raw(),
                Some(write_byte_callback),
                ptr::from_mut(&mut buf).cast(),
            );
        }
        buf
    }

    /// Deserialize context or modules from bytes.
    ///
    /// It will create one or more modules in `bytes`. The deserialized modules will be as if they
    /// are created manually, not loaded or linked yet.
    ///
    /// # Safety
    /// `bytes` must be trusted and produced from previous serialization.
    ///
    /// # Panics
    ///
    /// Panic if there is any unfinished module.
    /// Panic from C if the `bytes` cannot be parsed or contain errors.
    #[cfg(feature = "io")]
    pub unsafe fn deserialize(&self, bytes: &[u8]) {
        assert!(
            self.module.get().is_none(),
            "must not have unfinished module on deserialization",
        );

        let mut bytes = bytes;
        unsafe {
            ffi::MIR_read_with_func(
                self.as_raw(),
                Some(read_byte_callback),
                ptr::from_mut(&mut bytes).cast(),
            );
        }
    }

    /// Create a new module and enter it.
    ///
    /// The MIR context is stateful. When entering a new module, it should be correctly finished
    /// before creating another module. See [`MirModuleBuilder`] for more details.
    ///
    /// # Panics
    ///
    /// Panic if there is any unfinished module.
    pub fn enter_new_module(&self, name: &CStr) -> MirModuleBuilder<'_> {
        assert!(self.module.get().is_none(), "already inside a module");
        let module = unsafe { ffi::MIR_new_module(self.as_raw(), name.as_ptr()) };
        self.module
            .set(Some(NonNull::new(module).expect("module must not be null")));
        MirModuleBuilder { ctx: self }
    }

    /// Load an MIR module for linking.
    pub fn load_module(&self, module: MirModuleRef<'_>) {
        unsafe { ffi::MIR_load_module(self.as_raw(), module.module.as_ptr()) };
    }

    /// Load an external name pointing to `addr` for linking.
    ///
    /// # Example
    /// ```
    /// # fn link(ctx: &mir::MirContext) {
    /// // let ctx: &MirContext;
    /// unsafe { ctx.load_external(c"memset", libc::memset as _) };
    /// # }
    /// ```
    ///
    /// # Safety
    ///
    /// `addr` must be a valid function pointer with prototype expected by generated code.
    pub unsafe fn load_external(&self, name: &CStr, addr: *mut c_void) {
        unsafe { ffi::MIR_load_external(self.as_raw(), name.as_ptr(), addr) };
    }

    /// Link loaded modules and external names with given custom interface and resolver.
    ///
    /// # Safety
    ///
    /// `set_interface` should be one of `MIR_set_*_interface`.
    /// `resolver` must return valid function pointers with prototype expected by generated code.
    pub unsafe fn link_modules_raw(
        &self,
        set_interface: Option<
            unsafe extern "C-unwind" fn(ctx: ffi::MIR_context_t, item: ffi::MIR_item_t),
        >,
        resolver: Option<&ImportResolver>,
    ) {
        unsafe extern "C-unwind" fn trampoline(
            data: *mut c_void,
            name: *const c_char,
        ) -> *mut c_void {
            let name = unsafe { CStr::from_ptr(name) };
            unsafe { (*data.cast::<&ImportResolver>())(name) }
        }

        // NB. Keep `resolver` alive on stack by taking a reference.
        let (resolver, arg) = match &resolver {
            Some(resolver) => (
                Some(trampoline as _),
                // NB. This is a pointer to fat reference to dyn Fn.
                ptr::from_ref(resolver).cast_mut().cast(),
            ),
            None => (None, null_mut()),
        };
        unsafe { ffi::MIR_link(self.as_raw(), set_interface, resolver, arg) }
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

/// A module reference.
#[derive(Debug, Clone, Copy)]
pub struct MirModuleRef<'ctx> {
    module: NonNull<ffi::MIR_module>,
    _marker: PhantomData<&'ctx MirContext>,
}

impl MirModuleRef<'_> {
    unsafe fn from_raw(raw: *mut ffi::MIR_module) -> Self {
        Self {
            module: NonNull::new(raw).expect("module must not be null"),
            _marker: PhantomData,
        }
    }

    /// Get the underlying pointer for FFI.
    #[must_use]
    pub fn as_raw(&self) -> *mut ffi::MIR_module {
        self.module.as_ptr()
    }

    /// Get the name of the module.
    #[must_use]
    pub fn name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.module.as_ref().name) }
    }

    /// Get the list of all items inside the module.
    #[must_use]
    pub fn get_items(&self) -> Vec<ItemRef<'_>> {
        let head = unsafe { self.module.as_ref().items.head };
        std::iter::successors(NonNull::new(head), |item| unsafe {
            NonNull::new(item.as_ref().item_link.next)
        })
        .map(|item| unsafe { ItemRef::from_raw(item.as_ptr()) })
        .collect()
    }

    /// Dump the content in a textual representation for human consumption.
    #[must_use]
    pub fn dump(&self, ctx: &MirContext) -> String {
        MemoryFile::with(|file| unsafe {
            ffi::MIR_output_module(ctx.as_raw(), file, self.as_raw());
        })
        .1
    }

    /// Serialize the module  into bytes.
    ///
    /// The serialization format is stable across executions, but may not be stable across
    /// MIR versions.
    #[cfg(feature = "io")]
    pub fn serialize(&self, ctx: &MirContext) -> Vec<u8> {
        let mut buf = Vec::new();
        unsafe {
            ffi::MIR_write_module_with_func(
                ctx.as_raw(),
                Some(write_byte_callback),
                self.as_raw(),
                ptr::from_mut(&mut buf).cast(),
            );
        }
        buf
    }
}

/// The currently building MIR module.
///
/// There can only be at most one unfinished module for each [`MirContext`].
/// It is strongly advised to explicitly call [`MirModuleBuilder::finish`] to finish the current
/// module for clarity and observe any error outside drop impl.
/// [`MirModuleBuilder`] will automatically finish it on drop.
#[derive(Debug)]
#[must_use = "module builder should be correctly finished by finish()"]
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
    /// Explicitly finish the module and return the module reference.
    ///
    /// # Panics
    ///
    /// Panic from C if the module content is malformed.
    #[expect(clippy::must_use_candidate, reason = "can be ignored")]
    pub fn finish(self) -> MirModuleRef<'ctx> {
        let module = self.ctx.module.get().expect("must be inside a module");
        drop(self);
        unsafe { MirModuleRef::from_raw(module.as_ptr()) }
    }

    fn as_raw_ctx(&self) -> *mut ffi::MIR_context {
        self.ctx.as_raw()
    }

    /// Add a new function prototype and return its reference.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names or invalid signature.
    #[must_use]
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

    /// Add a new import name and return its reference.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[must_use]
    pub fn add_import(&self, name: &CStr) -> ImportItemRef<'_> {
        let item =
            unsafe { ItemRef::from_raw(ffi::MIR_new_import(self.as_raw_ctx(), name.as_ptr())) };
        ImportItemRef(item)
    }

    /// Add a new export name and return its reference.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[expect(clippy::must_use_candidate, reason = "can be ignored")]
    pub fn add_export(&self, name: &CStr) -> ExportItemRef<'_> {
        let item =
            unsafe { ItemRef::from_raw(ffi::MIR_new_export(self.as_raw_ctx(), name.as_ptr())) };
        ExportItemRef(item)
    }

    /// Add a new forward declaration and return its reference.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[expect(clippy::must_use_candidate, reason = "can be ignored")]
    pub fn add_forward(&self, name: &CStr) -> ForwardItemRef<'_> {
        let item =
            unsafe { ItemRef::from_raw(ffi::MIR_new_forward(self.as_raw_ctx(), name.as_ptr())) };
        ForwardItemRef(item)
    }

    /// Add a new forward declaration and return its reference.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[must_use]
    pub fn add_data<'a>(&self, name: impl Into<Option<&'a CStr>>, data: &[u8]) -> DataItemRef<'_> {
        unsafe {
            DataItemRef(ItemRef::from_raw(ffi::MIR_new_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), CStr::as_ptr),
                Ty::U8.0,
                data.len(),
                data.as_ptr().cast(),
            )))
        }
    }

    /// Add a new data referencing another item and return its reference.
    ///
    /// The address of `ref_item` after linking plus `disp` is used to initialize the data.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[must_use]
    pub fn add_ref_data<'a>(
        &self,
        name: impl Into<Option<&'a CStr>>,
        ref_item: ItemRef<'_>,
        disp: i64,
    ) -> RefDataItemRef<'_> {
        unsafe {
            RefDataItemRef(ItemRef::from_raw(ffi::MIR_new_ref_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), CStr::as_ptr),
                ref_item.as_raw(),
                disp,
            )))
        }
    }

    /// Add a new data initialized by a function and return its reference.
    ///
    /// - Not all MIR functions can be used for expression data. The expression function should
    ///   have only one result, have no arguments, not use any call or any instruction with memory.
    /// - The expression function is called during linking and its result is used to initialize the
    ///   data.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names or unsupported functions.
    #[must_use]
    pub fn add_expr_data<'a>(
        &self,
        name: impl Into<Option<&'a CStr>>,
        expr_func: FuncItemRef<'_>,
    ) -> ExprDataItemRef<'_> {
        unsafe {
            ExprDataItemRef(ItemRef::from_raw(ffi::MIR_new_expr_data(
                self.as_raw_ctx(),
                name.into().map_or(null(), CStr::as_ptr),
                expr_func.as_raw(),
            )))
        }
    }

    /// Add a new data initialized by a label and return its reference.
    ///
    /// The addresses defined as `label[-base_label]+disp` where `base_label` can be `None`.
    /// `lref` can refers for labels the same function (this is checked during module load) and
    /// there is a warranty label addresses to be defined only at the beginning of the function
    /// execution.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[must_use]
    pub fn add_label_ref_data<'module>(
        &'module self,
        name: &CStr,
        label: Label<'module>,
        base_label: Option<Label<'module>>,
        disp: i64,
    ) -> LabelRefDataItemRef<'module> {
        unsafe {
            LabelRefDataItemRef(ItemRef::from_raw(ffi::MIR_new_lref_data(
                self.as_raw_ctx(),
                name.as_ptr(),
                label.0,
                base_label.map_or(null_mut(), |lbl| lbl.0),
                disp,
            )))
        }
    }

    /// Add a new writable memory segment and return its reference.
    ///
    /// # Panics
    ///
    /// Panic from C on duplicated names.
    #[must_use]
    pub fn add_bss<'a>(&self, name: impl Into<Option<&'a CStr>>, len: usize) -> BssItemRef<'_> {
        unsafe {
            BssItemRef(ItemRef::from_raw(ffi::MIR_new_bss(
                self.as_raw_ctx(),
                name.into().map_or(null(), CStr::as_ptr),
                len,
            )))
        }
    }

    /// Add a new function definition and enter into it.
    ///
    /// The MIR context is stateful and the previous function must be finished before creating
    /// another one.
    /// See more details in [`MirFuncBuilder`].
    ///
    /// # Panics
    ///
    /// Panic if there is any unfinished function.
    /// Panic from C on duplicated names or invalid signature.
    #[must_use]
    pub fn enter_new_function<'module>(
        &'module self,
        name: &CStr,
        rets: &[Ty],
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

/// The currently building MIR function.
///
/// There can only be at most one unfinished function for each [`MirContext`] inside one unfinished
/// module. It is strongly advised to explicitly call [`MirFuncBuilder::finish`] to finish the
/// current function for clarity and observe any error outside drop impl.
/// [`MirFuncBuilder`] will automatically finish it on drop.
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

impl<'module, 'ctx> MirFuncBuilder<'module, 'ctx> {
    /// Explicitly finish the function and return the function reference.
    ///
    /// # Panics
    ///
    /// Panic from C if the function content is malformed.
    #[expect(clippy::must_use_candidate, reason = "can be ignored")]
    pub fn finish(self) -> FuncItemRef<'ctx> {
        let func_item = self.ctx.func_item.get().expect("must be inside a function");
        drop(self);
        FuncItemRef(ItemRef(func_item, PhantomData))
    }

    /// Get the virtual register or given name.
    ///
    /// Function parameters are represented as pre-defined virtual registers of same names.
    #[must_use]
    pub fn get_reg(&self, name: &CStr) -> Reg {
        let reg = unsafe { ffi::MIR_reg(self.ctx.ctx.as_ptr(), name.as_ptr(), self.func.as_ptr()) };
        Reg(reg)
    }

    /// Create a new virtual register.
    #[must_use]
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

    /// Create a new unbound label.
    ///
    /// The label must be inserted later via [`InsnBuilder::label`].
    #[must_use]
    pub fn new_label(&self) -> Label<'module> {
        let insn = unsafe { ffi::MIR_new_label(self.ctx.ctx.as_ptr()) };
        Label(insn, PhantomData)
    }

    /// Append a new instruction to the function.
    pub fn ins(&self) -> FuncInstBuilder<'module, 'ctx> {
        FuncInstBuilder {
            ctx: self.ctx,
            _marker: PhantomData,
        }
    }
}

/// The instruction appender to an function.
///
/// See [`InsnBuilder`] for all instructions.
#[derive(Debug)]
#[must_use = "FuncInstBuilder does nothing unless calling an method"]
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
