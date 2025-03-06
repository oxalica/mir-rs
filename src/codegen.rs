use std::cell::Cell;
use std::ffi::{c_int, c_void};
use std::fmt;

use crate::mem_file::MemoryFile;
use crate::{FuncItemRef, ImportResolver, MirContext, ffi};

pub struct MirGenContext {
    ctx: MirContext,
    debug_file: Cell<Option<MemoryFile>>,
}

impl fmt::Debug for MirGenContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MirGenContext")
            .field("ctx", &self.ctx)
            .finish_non_exhaustive()
    }
}

impl MirGenContext {
    pub fn new(ctx: MirContext) -> Self {
        unsafe { ffi::MIR_gen_init(ctx.ctx.as_ptr()) };
        Self {
            ctx,
            debug_file: Cell::new(None),
        }
    }

    pub fn set_opt_level(&self, level: u32) {
        unsafe { ffi::MIR_gen_set_optimize_level(self.ctx.ctx.as_ptr(), level) };
    }

    pub fn enable_debug(&self, level: c_int) {
        let file = MemoryFile::new();
        unsafe {
            ffi::MIR_gen_set_debug_level(self.ctx.ctx.as_ptr(), level);
            ffi::MIR_gen_set_debug_file(self.ctx.ctx.as_ptr(), file.file());
        }
        self.debug_file.set(Some(file));
    }

    pub fn get_debug_output(&self) -> String {
        let file = self.debug_file.take().expect("debug is not enabled");
        let s = file.get_data_string();
        self.debug_file.set(Some(file));
        s
    }

    pub fn link_modules_for_codegen(&self) {
        unsafe { self.link_modules(Some(ffi::MIR_set_gen_interface), None) }
    }

    /// # Safety
    /// `resolver` must return valid function pointers with matching prototype.
    pub unsafe fn link_modules_for_codegen_with_resolver(&self, resolver: &ImportResolver) {
        unsafe { self.link_modules(Some(ffi::MIR_set_gen_interface), Some(resolver)) }
    }

    pub fn codegen_func(&self, func: FuncItemRef<'_>) -> *mut c_void {
        unsafe { ffi::MIR_gen(self.ctx.ctx.as_ptr(), func.as_raw()) }
    }
}

impl Drop for MirGenContext {
    fn drop(&mut self) {
        if self.debug_file.get_mut().is_some() {
            unsafe {
                ffi::MIR_gen_set_debug_file(self.ctx.ctx.as_ptr(), std::ptr::null_mut());
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
