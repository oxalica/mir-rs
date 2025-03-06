use std::ffi::c_char;
use std::ptr::NonNull;

use libc::size_t;

#[derive(Debug)]
pub(crate) struct MemoryFile {
    file: NonNull<libc::FILE>,
    // Keep the address stable.
    buf_info: *mut (*mut c_char, size_t),
}

impl MemoryFile {
    pub fn with<T>(f: impl FnOnce(*mut libc::FILE) -> T) -> (T, String) {
        let file = Self::new();
        let ret = f(file.file());
        let s = file.get_data_string();
        (ret, s)
    }

    pub fn new() -> Self {
        let buf_info = Box::into_raw(Box::new((std::ptr::null_mut(), 0)));
        let file = unsafe { libc::open_memstream(&mut (*buf_info).0, &mut (*buf_info).1) };
        let file = NonNull::new(file).expect("failed to open_memstream");
        Self { file, buf_info }
    }

    pub fn file(&self) -> *mut libc::FILE {
        self.file.as_ptr()
    }

    pub fn get_data_string(&self) -> String {
        String::from_utf8_lossy(unsafe { &*self.data() }).into_owned()
    }

    /// Get the underlying buffer.
    /// The returned slice is invalidated when any write is performed on the stream.
    pub fn data(&self) -> *const [u8] {
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
