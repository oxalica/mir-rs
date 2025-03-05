use std::env;

// NB: Sync with git submodule.
const GIT_COMMIT: &str = "v1.0.0";

fn main() {
    println!("cargo::rerun-if-changed=mir");

    if !cfg!(feature = "bundled") {
        todo!();
    }

    let mut build = cc::Build::new();

    if env::var("DEBUG").unwrap() == "false" {
        build.define("NDEBUG", "1");
        // WAIT: <https://github.com/vnmakarov/mir/pull/418>
        // build.define("MIR_NO_GEN_DEBUG", "1");
    }

    // See: <https://github.com/vnmakarov/mir/blob/v1.0.0/GNUmakefile#L61>
    build
        .define("GITCOMMIT", GIT_COMMIT)
        .pic(true)
        .flag("-std=gnu11")
        .flag("-Wno-abi")
        .flag("-fsigned-char")
        .flag_if_supported("-fno-tree-sra")
        .flag_if_supported("-fno-ipa-cp-clone")
        .include("mir")
        .file("mir/mir.c")
        .file("mir/mir-gen.c")
        .compile("mir");

    println!("cargo::rustc-link-lib=m");
    println!("cargo::rustc-link-lib=dl");

    #[cfg(feature = "__internal_generate_sys")]
    generate();
}

#[cfg(feature = "__internal_generate_sys")]
fn generate() {
    bindgen::Builder::default()
        .header_contents("wrapper.h", r#"#include "mir/mir.h""#)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Hide private funcs and types: <https://github.com/vnmakarov/mir/blob/v1.0.0/mir.h#L637>
        .allowlist_var("MIR_.*")
        .allowlist_type("MIR_.*")
        .blocklist_type("MIR_code_reloc|MIR_code_reloc_t")
        .allowlist_function("MIR_.*|_MIR_init|_MIR_get_api_version")
        // Do not expand libc `FILE`.
        .opaque_type("FILE")
        .blocklist_type("FILE")
        .raw_line("use libc::FILE;")
        // No va_list support.
        .blocklist_function("MIR_interp_arr_varg")
        .blocklist_type(".*va_list.*")
        // Fix f128 (long double) FFI-safety.
        .raw_line("#[derive(Debug, Clone, Copy)] #[repr(C, align(16))] pub struct c_longdouble([u64; 2]); use c_longdouble as u128;")
        // Manual tweaks.
        .blocklist_type("MIR_imm_t|MIR_mem_t|MIR_op_t.*|MIR_insn|MIR_val_t")
        .raw_line("use super::{MIR_op_t, MIR_insn, MIR_val_t};")
        .prepend_enum_name(false)
        .layout_tests(false)
        .generate()
        .expect("failed to bindgen mir.h")
        .write_to_file("src/bindings.rs")
        .expect("failed to write bindgen output");

    bindgen::Builder::default()
        .header_contents("wrapper.h", r#"#include "mir/mir-gen.h""#)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .blocklist_type(".*")
        .allowlist_function("MIR_gen.*|MIR_set.*gen_interface")
        .raw_line("use super::*;")
        .raw_line("use libc::FILE;")
        .generate()
        .expect("failed to bindgen mir-gen.h")
        .write_to_file("src/bindings_gen.rs")
        .expect("failed to write bindgen output");
}
