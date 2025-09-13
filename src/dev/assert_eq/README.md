# `assert_eq!` macro

This crate provides a custom `assert_eq!` macro that gives detailed error messages when comparing complex data structures. It supports nested structures, collections, and provides a path to the first point of difference.

A quick example:

```rust ,should_panic
# #[path = "src/check_panic_message.rs"] mod check_panic_message; use check_panic_message::check_panic_message; check_panic_message(|| {
use assert_eq::AssertEq;
#[derive(AssertEq, Debug)]
struct A {
    x: String,
    y: Vec<i32>,
}

#[derive(AssertEq, Debug)]
struct B {
    x: A,
    y: Vec<i32>,
}

let x = B {
    x: A {
        x: "hello".to_string(),
        y: vec![1, 2, 3],
    },
    y: vec![4, 5, 6],
};

let y = B {
    x: A {
        x: "hello".to_string(),
        y: vec![1, 2, 8],
    },
    y: vec![4, 5, 6],
};

assert_eq::assert_eq!(x, y);
# }, "at .x → .y → [2]\n  left: 3\n right: 8");
```

It is `no_std` compatible, but requires the `alloc` crate.
