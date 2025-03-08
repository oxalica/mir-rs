//! Tests about lifetimes. Because compile_fail doctests can only exist in unit tests.
//! IR construction and codegen tests live in integration tests `../tests/smoke.rs`.
use std::mem::forget;

use rstest::rstest;

use crate::{InsnBuilder, MirContext};

/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// drop(ctx);
/// drop(mb);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let m = ctx.enter_new_module(c"m").finish();
/// drop(ctx);
/// drop(m);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let f = mb.enter_new_function(c"f", &[], &[]).finish();
/// mb.finish();
/// drop(ctx);
/// drop(f);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let f = mb.enter_new_function(c"f", &[], &[]).finish();
/// mb.finish();
/// drop(ctx);
/// drop(f);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let f = mb.enter_new_function(c"f", &[], &[]).finish();
/// let lbl = f.new_label();
/// mb.finish();
/// drop(ctx);
/// drop(lbl);
/// ```
///
fn _ctx_outlives() {}

/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let fb = mb.enter_new_function(c"f", &[], &[]);
/// mb.finish();
/// drop(fb);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let fb = mb.enter_new_function(c"f", &[], &[]);
/// let lbl = fb.new_label();
/// fb.finish();
/// mb.finish();
/// drop(lbl);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let fb = mb.enter_new_function(c"f", &[], &[]);
/// let lbl = fb.ins().new_label();
/// fb.finish();
/// mb.finish();
/// drop(lbl);
/// ```
///
/// ```compile_fail
/// let ctx = mir::MirContext::new();
/// let mb = ctx.enter_new_module(c"m");
/// let bss = ctx.new_bss(c"bss", 4);
/// mb.finish();
/// drop(bss);
/// ```
fn _module_outlives() {}

#[rstest]
#[case(false)]
#[case(true)]
#[should_panic = "already inside a module"]
fn double_enter_module(#[case] do_forget: bool) {
    let ctx = MirContext::new();
    let mb1 = ctx.enter_new_module(c"m1");
    if do_forget {
        forget(mb1);
    }
    let _mb2 = ctx.enter_new_module(c"m2");
}

#[rstest]
#[case(false)]
#[case(true)]
#[should_panic = "already inside a function"]
fn double_enter_func(#[case] do_forget: bool) {
    let ctx = MirContext::new();
    let mb = ctx.enter_new_module(c"m");
    let fb1 = mb.enter_new_function(c"f1", &[], &[]);
    if do_forget {
        forget(fb1);
    }
    let _fb2 = mb.enter_new_function(c"f1", &[], &[]);
}

#[test]
fn forget_cleanup() {
    let ctx = MirContext::new();
    let mb = ctx.enter_new_module(c"m");
    let fb = mb.enter_new_function(c"f", &[], &[]);
    forget(fb);
    forget(mb);
}

#[test]
fn label_is_module_global() {
    let ctx = MirContext::new();
    let mb = ctx.enter_new_module(c"m");
    let fb = mb.enter_new_function(c"f", &[], &[]);
    let lbl1 = fb.new_label();
    let lbl2 = fb.ins().new_label();
    fb.finish();
    let _ref = mb.add_label_ref_data(c"lref", lbl1, Some(lbl2), 0);
    let _ = lbl1;
    let _ = lbl2;
    mb.finish();
}
