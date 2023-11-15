use criterion::{black_box, criterion_group, criterion_main, Criterion};

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

fn run_parser(input: &str) -> (narxia_syn::syntree::GreenTree, Vec<narxia_syn::parse_error::ParseError>) {
    let mut ts = narxia_syn::token_source::text_ts::TextTokenSource::new(input);
    let mut parser = narxia_syn::parser::Parser::new(&mut ts);
    parser.parse();
    let (node, errors) = parser.finish_to_tree();
    (node, errors)
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parser 1000", |b| {
        let input = make_input(1000);
        b.iter(|| run_parser(black_box(&input)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);