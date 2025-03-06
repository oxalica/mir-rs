use mir::{InstBuilder, MirContext, MirFuncItem, MirGenContext, MirModule, Ty, Val};

#[test]
fn init() {
    let _ctx = MirContext::new();
}

fn build_add_module(ctx: &MirContext) -> (MirModule<'_>, MirFuncItem<'_>) {
    let mb = ctx.enter_new_module(c"module");
    let mut fb = mb.enter_new_function(c"add", &[Ty::I64], &[(c"a", Ty::I64), (c"b", Ty::I64)]);
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
    ctx.link_modules_for_interpreter();
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

    let func_ptr =
        unsafe { std::mem::transmute::<*mut _, extern "C" fn(i64, i64) -> i64>(func_ptr) };
    let ret = func_ptr(40, 2);
    assert_eq!(ret, 42);
}
