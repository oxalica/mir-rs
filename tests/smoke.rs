use std::ffi::CStr;
use std::mem;

use mir::{FuncItemRef, InsnBuilderExt, MemOp, MirContext, MirGenContext, MirModule, Ty, Val};
use rstest::rstest;

#[test]
fn init() {
    let _ctx = MirContext::new();
}

fn build_add_module(ctx: &MirContext) -> (MirModule<'_>, FuncItemRef<'_>) {
    let mb = ctx.enter_new_module(c"module");
    let fb = mb.enter_new_function(c"add", &[Ty::I64], &[(c"a", Ty::I64), (c"b", Ty::I64)]);
    let a = fb.get_reg(c"a");
    let b = fb.get_reg(c"b");
    let ret = fb.new_local_reg(c"ret", Ty::I64);
    fb.ins().add(ret, a, b);
    fb.ins().ret(ret);
    let func_item = fb.finish();
    let module = mb.finish();
    (module, func_item)
}

#[test]
fn add_interp() {
    let ctx = MirContext::new();
    let (module, func_item) = build_add_module(&ctx);
    ctx.load_module(module);
    ctx.link_modules_for_interpret();
    let mut ret = [Val::default()];
    unsafe { ctx.interpret_unchecked(func_item, &mut ret, &[Val::from(40i64), Val::from(2i64)]) };
    assert_eq!(ret[0].as_i64(), 42);
}

#[test]
fn add_gen() {
    let ctx = MirGenContext::new(MirContext::new());
    let (module, func_item) = build_add_module(&ctx);
    ctx.enable_debug(1);
    ctx.set_opt_level(3);

    ctx.load_module(module);
    ctx.link_modules_for_codegen();

    let func_ptr = ctx.codegen_func(func_item);
    eprintln!("Debug output:\n{}", ctx.get_debug_output());

    let func_ptr = unsafe { mem::transmute::<*mut _, extern "C" fn(i64, i64) -> i64>(func_ptr) };
    let ret = func_ptr(40, 2);
    assert_eq!(ret, 42);
}

#[rstest]
#[case::interp_static_link(true, false)]
#[case::interp_dynamic_link(true, true)]
#[case::codegen_static_link(false, false)]
#[case::codegen_dynamic_link(false, true)]
fn euler_sieve(#[case] interp: bool, #[case] dyn_resolve: bool) {
    let ctx = MirGenContext::new(MirContext::new());
    let m = ctx.enter_new_module(c"sieve");

    let memset_proto = m.add_proto(
        c"memset_proto",
        &[],
        &[(c"ptr", Ty::P), (c"val", Ty::I32), (c"len", Ty::I64)],
    );
    let memset_import = m.add_import(c"memset");

    let f = m.enter_new_function(
        c"sieve",
        &[Ty::I64],
        &[(c"pr", Ty::I64), (c"lp", Ty::I64), (c"n", Ty::I64)],
    );
    {
        let pr = f.get_reg(c"pr");
        let lp = f.get_reg(c"lp");
        let n = f.get_reg(c"n");
        let pr_len = f.new_local_reg(c"pr_len", Ty::I64);
        f.ins().mov(pr_len, 0i64);

        let lp_byte_len = f.new_local_reg(c"lp_byte_len", Ty::I64);
        f.ins().mul(lp_byte_len, n, 4i64);
        f.ins().call(
            memset_proto,
            memset_import,
            [],
            [lp.into(), 0i64.into(), lp_byte_len.into()],
        );

        let i = f.new_local_reg(c"i", Ty::I64);
        f.ins().mov(i, 2i64);
        let loop1_entry = f.new_label();
        f.ins().jmp(loop1_entry);
        let loop1_start = f.ins().new_label();

        let not_prime = f.new_label();
        let lp_i = MemOp::new_base(Ty::I32, lp).index(i).scale(4);
        f.ins().bt(not_prime, lp_i);
        f.ins().mov(lp_i, i);
        let pr_last = MemOp::new_base(Ty::I32, pr).index(pr_len).scale(4);
        f.ins().mov(pr_last, i);
        f.ins().add(pr_len, pr_len, 1i64);
        f.ins().label(not_prime);

        let j = f.new_local_reg(c"j", Ty::I64);
        f.ins().mov(j, 0i64);
        let loop2_start = f.ins().new_label();
        let i_mul_pr_j = f.new_local_reg(c"tmp", Ty::I64);
        let pr_j = MemOp::new_base(Ty::I32, pr).index(j).scale(4);
        f.ins().mul(i_mul_pr_j, i, pr_j);
        let loop2_end = f.new_label();
        f.ins().bge(loop2_end, i_mul_pr_j, n);

        let lp_tgt = MemOp::new_base(Ty::I32, lp).index(i_mul_pr_j).scale(4);
        f.ins().mov(lp_tgt, pr_j);
        f.ins().beq(loop2_end, pr_j, lp_i);

        f.ins().add(j, j, 1i64);
        f.ins().jmp(loop2_start);
        f.ins().label(loop2_end);

        f.ins().add(i, i, 1i64);
        f.ins().label(loop1_entry);
        f.ins().blt(loop1_start, i, n);

        f.ins().ret(pr_len);
    }
    let func_sieve = f.finish();
    println!("MIR:\n{}", ctx.dump_func_item(func_sieve));
    let module = m.finish();

    let resolver = |name: &CStr| {
        if name == c"memset" {
            libc::memset as _
        } else {
            std::ptr::null_mut()
        }
    };

    const N: usize = 50;
    let mut pr = [0i32; N];
    let mut lp = [0i32; N];

    ctx.load_module(module);
    let pr_len = if interp {
        if dyn_resolve {
            unsafe { ctx.link_modules_for_interpret_with_resolver(&resolver) };
        } else {
            unsafe { ctx.load_external(c"memset", libc::memset as _) };
            ctx.link_modules_for_interpret();
        }

        let args = [
            Val::from(pr.as_mut_ptr() as i64),
            Val::from(lp.as_mut_ptr() as i64),
            Val::from(N as i64),
        ];
        let mut ret = [Val::default()];
        unsafe { ctx.interpret_unchecked(func_sieve, &mut ret, &args) };
        ret[0].as_u64() as usize
    } else {
        ctx.enable_debug(1);
        ctx.set_opt_level(3);
        if dyn_resolve {
            unsafe { ctx.link_modules_for_codegen_with_resolver(&resolver) };
        } else {
            unsafe { ctx.load_external(c"memset", libc::memset as _) };
            ctx.link_modules_for_codegen();
        }
        let sieve = ctx.codegen_func(func_sieve);
        println!("Debug output:\n{}", ctx.get_debug_output());

        type SieveTy = unsafe extern "C" fn(pr: *mut i32, lp: *mut i32, n: usize) -> usize;
        let sieve = unsafe { mem::transmute::<*mut _, SieveTy>(sieve) };
        unsafe { sieve(pr.as_mut_ptr(), lp.as_mut_ptr(), N) }
    };

    let primes = &pr[..pr_len];
    assert_eq!(
        primes,
        [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47],
    );
}
