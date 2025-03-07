use std::ffi::c_void;
use std::fmt;

use crate::{FuncItemRef, ImportResolver, MirContext, ffi};

/// The MIR context for native code generation.
pub struct MirGenContext {
    ctx: MirContext,
    #[cfg(feature = "gen-debug")]
    debug_file: std::cell::Cell<Option<crate::mem_file::MemoryFile>>,
}

impl fmt::Debug for MirGenContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MirGenContext")
            .field("ctx", &self.ctx)
            .finish_non_exhaustive()
    }
}

impl MirGenContext {
    /// Initialize the codegen context on top of an existing MIR context.
    pub fn new(ctx: MirContext) -> Self {
        unsafe { ffi::MIR_gen_init(ctx.ctx.as_ptr()) };
        Self {
            ctx,
            #[cfg(feature = "gen-debug")]
            debug_file: std::cell::Cell::new(None),
        }
    }

    /// Set the optimization level to use.
    ///
    /// See details in [MIR documentation][upstream-docs].
    ///
    /// [upstream-docs]: https://github.com/vnmakarov/mir/blob/v1.0.0/MIR.md#user-content-mir-generator-file-mir-genh
    pub fn set_opt_level(&self, level: u32) {
        unsafe { ffi::MIR_gen_set_optimize_level(self.ctx.ctx.as_ptr(), level) };
    }

    /// Enable internal debug logging with specific level.
    ///
    /// Logs will be collected in memory and can be retrieved by [`Self::get_debug_output`].
    #[cfg(feature = "gen-debug")]
    pub fn enable_debug(&self, level: libc::c_int) {
        let file = crate::mem_file::MemoryFile::new();
        unsafe {
            ffi::MIR_gen_set_debug_level(self.ctx.ctx.as_ptr(), level);
            ffi::MIR_gen_set_debug_file(self.ctx.ctx.as_ptr(), file.file());
        }
        self.debug_file.set(Some(file));
    }

    /// Retrieve the debug logs collected so far.
    ///
    /// Note: logs will not be cleared after the call. Memory can only be freed when destroyingthe
    /// context.
    ///
    /// # Panics
    ///
    /// Panics if debug logging is not enabled before.
    #[cfg(feature = "gen-debug")]
    pub fn get_debug_output(&self) -> String {
        let file = self.debug_file.take().expect("debug is not enabled");
        let s = file.get_data_string();
        self.debug_file.set(Some(file));
        s
    }

    /// Link loaded modules and external names, preparing to be codegen.
    ///
    /// # Panics
    ///
    /// Panic from C on unresolved names.
    pub fn link_modules_for_codegen(&self) {
        unsafe { self.link_modules_raw(Some(ffi::MIR_set_gen_interface), None) }
    }

    /// Link loaded modules and external names with custom resolver, preparing to be codegen.
    ///
    /// # Safety
    ///
    /// `resolver` must return valid function pointers with prototype expected by generated code,
    /// or `NULL` if unresolved.
    ///
    /// # Panics
    ///
    /// Panic from C on unresolved names.
    pub unsafe fn link_modules_for_codegen_with_resolver(&self, resolver: &ImportResolver) {
        unsafe { self.link_modules_raw(Some(ffi::MIR_set_gen_interface), Some(resolver)) }
    }

    /// Generate native code and return the function pointer to `func`.
    ///
    /// This function is idempotic, that is, once code is generated for `func`, all later calls
    /// will do nothing and simply return the same function pointer.
    pub fn codegen_func(&self, func: FuncItemRef<'_>) -> *mut c_void {
        unsafe { ffi::MIR_gen(self.ctx.ctx.as_ptr(), func.as_raw()) }
    }
}

impl Drop for MirGenContext {
    fn drop(&mut self) {
        #[cfg(feature = "gen-debug")]
        {
            if self.debug_file.get_mut().is_some() {
                unsafe {
                    ffi::MIR_gen_set_debug_file(self.ctx.ctx.as_ptr(), std::ptr::null_mut());
                }
            }
        }
        unsafe { ffi::MIR_gen_finish(self.ctx.ctx.as_ptr()) };
    }
}

impl std::ops::Deref for MirGenContext {
    type Target = MirContext;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}
