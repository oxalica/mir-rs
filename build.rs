fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    cc::Build::new()
        .std("c23")
        .file("./trampoline.c")
        .compile("trampoline");
}
