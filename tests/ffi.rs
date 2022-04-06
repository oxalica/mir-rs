use expect_test::expect;
use libc::c_char;
use mir::mir_sys::*;

macro_rules! cstr {
    ($s:literal) => {{
        const _: [(); 1] = [(); ($s[$s.len() - 1] == b'\0') as usize];
        $s.as_ptr() as *mut u8 as *mut c_char
    }};
}

#[test]
fn test_init() {
    unsafe {
        let ctx = MIR_init();
        MIR_finish(ctx);
    }
}

fn collect_output(func: impl FnOnce(*mut libc::FILE)) -> String {
    unsafe {
        let (mut ptr, mut size) = (std::ptr::null_mut::<libc::c_char>(), 0usize);
        let file = libc::open_memstream(&mut ptr, &mut size);
        func(file);
        libc::fclose(file);
        let content = std::slice::from_raw_parts(ptr.cast::<u8>(), size);
        let ret = std::str::from_utf8(content).map(|s| s.to_owned());
        libc::free(ptr.cast());
        ret.unwrap()
    }
}

#[test]
fn test_simple_add() {
    unsafe {
        let ctx = MIR_init();
        let module = MIR_new_module(ctx, cstr!(b"module\0"));

        let func_item = MIR_new_func(
            ctx,
            cstr!(b"add\0"),
            1,
            [MIR_T_I64].as_mut_ptr(),
            2,
            MIR_T_I64,
            cstr!(b"a\0"),
            MIR_T_I64,
            cstr!(b"b\0"),
        );
        let func = (*func_item).u.func;

        let reg_arg1 = MIR_reg(ctx, cstr!(b"a\0"), func);
        let reg_arg2 = MIR_reg(ctx, cstr!(b"b\0"), func);
        let reg_ret = MIR_new_func_reg(ctx, func, MIR_T_I64, cstr!(b"ret\0"));

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
        MIR_finish_module(ctx);

        let repr = collect_output(|fout| {
            MIR_output_module(ctx, fout, module);
        });
        expect![[r##"
            module:	module
            add:	func	i64, i64:a, i64:b
            	local	i64:ret
            # 2 args, 1 local
            	add	ret, a, b
            	ret	ret
            	endfunc
            	endmodule
        "##]]
        .assert_eq(&repr);

        MIR_finish(ctx);
    }
}
