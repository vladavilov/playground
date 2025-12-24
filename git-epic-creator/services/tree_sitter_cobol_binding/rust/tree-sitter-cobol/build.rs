use std::path::PathBuf;

fn main() {
    let crate_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());

    // We deliberately compile the committed/generated parser sources to avoid
    // any runtime or build-time dependency on Node/tree-sitter-cli.
    let vendor_src = crate_dir
        .join("..")
        .join("..")
        .join("vendor")
        .join("tree-sitter-cobol")
        .join("src");

    println!("cargo:rerun-if-changed={}", vendor_src.join("parser.c").display());
    println!("cargo:rerun-if-changed={}", vendor_src.join("scanner.c").display());
    println!(
        "cargo:rerun-if-changed={}",
        vendor_src.join("tree_sitter").join("parser.h").display()
    );

    let mut build = cc::Build::new();
    build.include(&vendor_src);
    build.file(vendor_src.join("parser.c"));
    build.file(vendor_src.join("scanner.c"));
    build.flag_if_supported("-std=c11");
    build.warnings(false);
    build.compile("tree-sitter-cobol");
}


