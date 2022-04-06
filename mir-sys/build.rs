use std::env;

fn main() {
    let mut build = cc::Build::new();

    if env::var("DEBUG").unwrap() == "false" {
        build.define("NDEBUG", "1");
    }

    if env::var("CARGO_FEATURE_PARALLEL_GEN").is_ok() {
        build.define("MIR_PARALLEL_GEN", "1");
    }

    if env::var("CARGO_FEATURE_C2MIR").is_ok() {
        build.file("mir/c2mir/c2mir.c");
    }

    build
        .flag("-std=gnu11")
        .flag("-Wno-abi")
        .flag("-fsigned-char")
        .flag_if_supported("-fno-tree-sra")
        .flag_if_supported("-fno-ipa-cp-clone")
        .include("mir")
        .file("mir/mir.c")
        .file("mir/mir-gen.c")
        .compile("mir");
}
