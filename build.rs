fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    cc::Build::new()
        .std("c11")
        .file("./mirrs_trampoline.c")
        .compile("mirrs_trampoline");
}
