fn main() {
    println!("cargo::rustc-check-cfg=cfg(hir_id_span)");
}
