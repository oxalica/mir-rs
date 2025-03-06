use std::cell::Cell;
use std::ffi::{c_char, c_int, c_void};
use std::fmt;
use std::ptr::NonNull;

use libc::size_t;

use crate::{MirContext, MirFuncItem, ffi};

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
            ffi::MIR_gen_set_debug_file(self.ctx.ctx.as_ptr(), file.file.as_ptr());
        }
        self.debug_file.set(Some(file));
    }

    pub fn get_debug_output(&self) -> String {
        let file = self.debug_file.take().expect("debug is not enabled");
        let data = file.data();
        self.debug_file.set(Some(file));
        unsafe { String::from_utf8_lossy(&*data).into_owned() }
    }

    pub fn link_modules_for_codegen(&self) {
        unsafe {
            ffi::MIR_link(
                self.ctx.ctx.as_ptr(),
                Some(ffi::MIR_set_gen_interface),
                // TODO
                None,
            );
        }
    }

    pub fn codegen_func(&self, func_item: MirFuncItem<'_>) -> *mut c_void {
        unsafe { ffi::MIR_gen(self.ctx.ctx.as_ptr(), func_item.func_item.as_ptr()) }
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

#[derive(Debug)]
struct MemoryFile {
    file: NonNull<libc::FILE>,
    // Keep the address stable.
    buf_info: *mut (*mut c_char, size_t),
}

impl MemoryFile {
    fn new() -> Self {
        let buf_info = Box::into_raw(Box::new((std::ptr::null_mut(), 0)));
        let file = unsafe { libc::open_memstream(&mut (*buf_info).0, &mut (*buf_info).1) };
        let file = NonNull::new(file).expect("failed to open_memstream");
        Self { file, buf_info }
    }

    /// Get the underlying buffer.
    /// The returned slice is invalidated when any write is performed on the stream.
    fn data(&self) -> *const [u8] {
        unsafe { libc::fflush(self.file.as_ptr()) };
        let (buf_ptr, buf_len) = unsafe { *self.buf_info };
        std::ptr::slice_from_raw_parts(buf_ptr.cast(), buf_len)
    }
}

impl Drop for MemoryFile {
    fn drop(&mut self) {
        unsafe { libc::fclose(self.file.as_ptr()) };
        let (buf_ptr, _buf_len) = unsafe { *Box::from_raw(self.buf_info) };
        unsafe { libc::free(buf_ptr.cast()) };
    }
}
