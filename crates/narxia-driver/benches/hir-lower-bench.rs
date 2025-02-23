use std::path::PathBuf;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use narxia_driver::DriverCtx;
use narxia_hir::lower::LowerCtxt;
use narxia_src_db::SrcFile;
use narxia_syn::syntree::SynTree;

fn make_input(num: usize) -> String {
    r#"
    x = y
    x.y = z
    { let x = a; let y = b; x + y }
    f { it + a }
    f { it -> it + a }
    f { it + a }
    f { it -> it + a }
    f { it ->
        call();
        call2();
        it
    }
    f { it: i32 -> it + a }
    f { it: i32, it2: i32 -> it + it2 }
    f { it: i32, it2: i32, it3: i32 -> it + it2 + it3 }
    f() { it + a }
    f() { it -> it + a }
    f(a) { it -> it + b }
    a.f { it + a }
    a.f(a) { it + b }
    fn main() {
    }
    fn main() {
        let x = a
        let y = b
    }
    fn f<const V: Ty>(a: i32) {
    }
    fn f<T>(v: T) {
    }
    
    fn f<T, U>(v: T, w: U) {
    }
    
    fn f<T: Copy+Clone = i32>(v: T) {
    }
    fn input(a0: A0, a1: A1) {
    }
    
    fn input_with_defaults(a0: A0, a1: A1 = A1()) {
    }
    fn main() -> i32 { 0 }
    
    fn main() {
        for (i in f()) {
        }
    }
    for (i in j) {}
    
    if (a) b else c
    loop {}
    while (a == b) {}
    println("Hello world!")
    if (a == b) a else b
    let x: i32
    let x: i32 = a
    let x = a
    fn main() {
        loop {}
    }
    let x = 123_456_789 + 0b01_00 + 01_234_567 + 0x3_abc_def
    let x = a + b * c / d % x - y == e != f >= g.h * i[j[k]] <= l.m[n] / o.p.q[r] > s(t < u.v(w.x.y.z)) & a | b ^ c && d || e
    continue
    continue
    break
    break x
    return
    return x
    let x

    fn main() {
        while (a == b) {
        }
    }

    "#.repeat(num)
}

fn hir_lower(root: &SynTree, src_file: SrcFile, ctx: &DriverCtx) {
    let mut hir_map = ctx.db.get_global_ty_ctxt().hir_map_mut_ref();
    hir_map.__test_clean();
    let hir = narxia_hir::lower::lower_mod_def(
        &mut LowerCtxt {
            src_file,
            hir_map: &mut hir_map,
        },
        root.get_root(),
    );
}

fn criterion_benchmark(c: &mut Criterion) {
    let ctx = DriverCtx::initialize_in_test();
    c.bench_function("hirlower 10", |b| {
        let input = make_input(10);
        let file = narxia_driver::load_file(&ctx, PathBuf::from("input.nrx"), &input);
        ctx.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(Some(file));
        let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
        let root = tree.tree(&ctx.db).red();
        b.iter(|| hir_lower(black_box(&root), file, &ctx));
        ctx.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(None);
    });

    c.bench_function("hirlower 1000", |b| {
        let input = make_input(1000);
        let file = narxia_driver::load_file(&ctx, PathBuf::from("input.nrx"), &input);
        ctx.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(Some(file));
        let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
        let root = tree.tree(&ctx.db).red();
        b.iter(|| hir_lower(black_box(&root), file, &ctx));
        ctx.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(None);
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
