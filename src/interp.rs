use std::fmt;

use paste::paste;

use crate::{FuncItemRef, ImportResolver, MirContext, ffi};

/// Untagged value for MIR interpreter.
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
                #[inline]
                fn from(v: $ty) -> Self {
                    // Fully initialize it.
                    let mut ret = Self::default();
                    ret.0.$name = v;
                    ret
                }
            }
        )*
        impl Val {
            paste! {
                $(
                    /// Get the value as a
                    #[doc = concat!("`", stringify!($ty), "`.")]
                    #[must_use]
                    #[inline]
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

impl MirContext {
    /// Link loaded modules and external names, preparing to be interpreted.
    ///
    /// # Panics
    ///
    /// Panic from C on unresolved names.
    pub fn link_modules_for_interpret(&self) {
        unsafe { self.link_modules_raw(Some(ffi::MIR_set_interp_interface), None) }
    }

    /// Link loaded modules and external names with custom resolver, preparing to be interpreted.
    ///
    /// # Safety
    ///
    /// `resolver` must return valid function pointers with prototype expected by generated code,
    /// or `NULL` if unresolved.
    ///
    /// # Panics
    ///
    /// Panic from C on unresolved names.
    pub unsafe fn link_modules_for_interpret_with_resolver(&self, resolver: &ImportResolver) {
        unsafe { self.link_modules_raw(Some(ffi::MIR_set_interp_interface), Some(resolver)) }
    }

    /// Execute a function using the interpreter.
    ///
    /// # Safety
    ///
    /// The types and lengths of arguments and results must match the function signature.
    ///
    /// # Panics
    ///
    /// Panic from C on (detectable) interpretation errors.
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
            );
        }
    }
}
