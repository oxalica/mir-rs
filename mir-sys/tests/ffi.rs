use std::ffi::CStr;

use expect_test::expect;
use mir_sys::*;

#[test]
fn init() {
    unsafe {
        let ctx = MIR_init();
        MIR_finish(ctx);
    }
}

fn collect_output<T>(func: impl FnOnce(*mut libc::FILE) -> T) -> (T, String) {
    unsafe {
        let (mut ptr, mut size) = (std::ptr::null_mut::<libc::c_char>(), 0usize);
        let file = libc::open_memstream(&mut ptr, &mut size);
        let ret = func(file);
        libc::fclose(file);
        let content = std::slice::from_raw_parts(ptr.cast::<u8>(), size);
        let collected = std::str::from_utf8(content).map(|s| s.to_owned());
        libc::free(ptr.cast());
        (ret, collected.unwrap().replace("\t", "  "))
    }
}

unsafe fn gen_add_i64(ctx: MIR_context_t, func_name: &CStr) -> MIR_item_t {
    unsafe {
        // Module lowering.
        let func_item = MIR_new_func(
            ctx,
            func_name.as_ptr(),
            1,
            [MIR_T_I64].as_mut_ptr(),
            2,
            MIR_T_I64,
            c"a".as_ptr(),
            MIR_T_I64,
            c"b".as_ptr(),
        );
        let func = (*func_item).u.func;
        let reg_arg1 = MIR_reg(ctx, c"a".as_ptr(), func);
        let reg_arg2 = MIR_reg(ctx, c"b".as_ptr(), func);
        let reg_ret = MIR_new_func_reg(ctx, func, MIR_T_I64, c"ret".as_ptr());
        MIR_append_insn(
            ctx,
            func_item,
            MIR_new_insn(
                ctx,
                MIR_ADD,
                MIR_new_reg_op(ctx, reg_ret),
                MIR_new_reg_op(ctx, reg_arg1),
                MIR_new_reg_op(ctx, reg_arg2),
            ),
        );
        MIR_append_insn(
            ctx,
            func_item,
            MIR_new_ret_insn(ctx, 1, MIR_new_reg_op(ctx, reg_ret)),
        );
        MIR_finish_func(ctx);
        func_item
    }
}

#[test]
fn add_interp() {
    unsafe {
        let ctx = MIR_init();
        let module = MIR_new_module(ctx, c"module".as_ptr());
        let func_item = gen_add_i64(ctx, c"add");
        MIR_finish_module(ctx);

        let ((), repr) = collect_output(|fout| {
            MIR_output_module(ctx, fout, module);
        });
        expect![[r#"
            module:  module
            add:  func  i64, i64:a, i64:b
              local  i64:ret

            # 2 args, 1 local, 0 globals
              add  ret, a, b
              ret  ret
              endfunc
              endmodule
        "#]]
        .assert_eq(&repr);

        let mut ret = MIR_val_t { i: 0 };
        let args = [MIR_val_t { i: 40 }, MIR_val_t { i: 2 }];
        MIR_interp_arr(ctx, func_item, &mut ret, 2, args.as_ptr().cast_mut());
        assert_eq!(ret.i, 42);
    }
}

#[test]
fn add_gen() {
    unsafe {
        let ctx = MIR_init();
        let module = MIR_new_module(ctx, c"module".as_ptr());
        let func_item = gen_add_i64(ctx, c"add");
        MIR_finish_module(ctx);

        MIR_gen_init(ctx);
        MIR_gen_set_optimize_level(ctx, 3);

        // Do not enable assembly dumping (level=2) which requires gcc, objcopy and objdump.
        // It will also print directly into stderr.
        MIR_gen_set_debug_level(ctx, 1);
        let (func_ptr, dbg_output) = collect_output(|fout| {
            MIR_gen_set_debug_file(ctx, fout);

            // Link modules.
            MIR_load_module(ctx, module);
            MIR_link(ctx, Some(MIR_set_gen_interface), None, std::ptr::null_mut());

            let func_ptr = MIR_gen(ctx, func_item);
            std::mem::transmute::<*mut libc::c_void, extern "C" fn(i64, i64) -> i64>(func_ptr)
        });
        eprintln!("{dbg_output}");
        assert_eq!(func_ptr(40, 2), 42);
        MIR_gen_finish(ctx);

        MIR_finish(ctx);
    }
}
