fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    cc::Build::new()
        .std("c11")
        .file("./trampoline.c")
        .compile("trampoline");
}
