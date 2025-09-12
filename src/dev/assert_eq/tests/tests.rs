use assert_eq::AssertEq;

#[test]
fn basic_eq() {
    let a = 1;
    let b = 1;
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "assertion `left == right` failed: at <root>\n  left: 1\n right: 2")]
fn basic_eq_fail() {
    let a = 1;
    let b = 2;
    assert_eq::assert_eq!(a, b);
}

#[test]
fn option_eq() {
    let a = Some(1);
    let b = Some(1);
    assert_eq::assert_eq!(a, b);
}

#[test]
fn option_none_eq() {
    let a: Option<i32> = None;
    let b: Option<i32> = None;
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "at <root>, left and right are different kinds of Option")]
fn option_eq_fail() {
    let a = Some(1);
    let b = None;
    assert_eq::assert_eq!(a, b);
}

#[test]
fn result_ok_eq() {
    let a: Result<i32, &str> = Ok(1);
    let b: Result<i32, &str> = Ok(1);
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "assertion `left == right` failed: at [Ok]\n  left: 1\n right: 2")]
fn result_ok_eq_fail() {
    let a: Result<i32, &str> = Ok(1);
    let b: Result<i32, &str> = Ok(2);
    assert_eq::assert_eq!(a, b);
}

#[test]
fn result_err_eq() {
    let a: Result<i32, &str> = Err("error");
    let b: Result<i32, &str> = Err("error");
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "assertion `left == right` failed: at [Err]\n  left: \"error1\"\n right: \"error2\""
)]
fn result_err_eq_fail() {
    let a: Result<i32, &str> = Err("error1");
    let b: Result<i32, &str> = Err("error2");
    assert_eq::assert_eq!(a, b);
}

#[test]
fn derived_struct_eq() {
    #[derive(AssertEq)]
    struct S {
        a: i32,
        b: String,
    }
    let a = S {
        a: 1,
        b: "hello".to_owned(),
    };
    let b = S {
        a: 1,
        b: "hello".to_owned(),
    };
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "assertion `left == right` failed: at .b\n  left: \"hello\"\n right: \"world\""
)]
fn derived_struct_eq_fail() {
    #[derive(AssertEq)]
    struct S {
        a: i32,
        b: String,
    }
    let a = S {
        a: 1,
        b: "hello".to_owned(),
    };
    let b = S {
        a: 1,
        b: "world".to_owned(),
    };
    assert_eq::assert_eq!(a, b);
}

#[test]
fn derived_tuple_struct_eq() {
    #[derive(AssertEq)]
    struct S(i32, String);
    let a = S(1, "hello".to_owned());
    let b = S(1, "hello".to_owned());
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "assertion `left == right` failed: at .1\n  left: \"hello\"\n right: \"world\""
)]
fn derived_tuple_struct_eq_fail() {
    #[derive(AssertEq)]
    struct S(i32, String);
    let a = S(1, "hello".to_owned());
    let b = S(1, "world".to_owned());
    assert_eq::assert_eq!(a, b);
}

#[test]
fn derived_enum_eq() {
    #[derive(AssertEq, Debug)]
    enum E {
        A { x: i32, y: String },
    }
    let a = E::A {
        x: 1,
        y: "hello".to_owned(),
    };
    let b = E::A {
        x: 1,
        y: "hello".to_owned(),
    };
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "Enum variants do not match, at <root>:\n  left: A { x: 1, y: \"hello\" }\n right: B(1, \"hello\")"
)]
fn derived_enum_eq_fail() {
    #[derive(AssertEq, Debug)]
    enum E {
        A { x: i32, y: String },
        B(i32, String),
    }
    let a = E::A {
        x: 1,
        y: "hello".to_owned(),
    };
    let b = E::B(1, "hello".to_owned());
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "assertion `left == right` failed: at [A] → .y\n  left: \"hello\"\n right: \"world\""
)]
fn derived_enum_eq_fail_inner() {
    #[derive(AssertEq, Debug)]
    enum E {
        A { x: i32, y: String },
    }
    let a = E::A {
        x: 1,
        y: "hello".to_owned(),
    };
    let b = E::A {
        x: 1,
        y: "world".to_owned(),
    };
    assert_eq::assert_eq!(a, b);
}

#[test]
fn array_eq() {
    let a = [1, 2, 3];
    let b = [1, 2, 3];
    assert_eq::assert_eq!(a, b);
}

#[test]
fn slice_eq() {
    let a = &[1, 2, 3];
    let b = &[1, 2, 3];
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "assertion `left == right` failed: at [2]\n  left: 3\n right: 4")]
fn slice_diff() {
    let a = &[1, 2, 3];
    let b = &[1, 2, 4];
    assert_eq::assert_eq!(a, b);
}

#[test]
fn slice_of_str_eq() {
    let a = &["a", "b", "c"];
    let b = &["a", "b", "c"];
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "assertion `left == right` failed: at [2]\n  left: \"c\"\n right: \"d\"")]
fn slice_of_str_diff() {
    let a = &["a", "b", "c"];
    let b = &["a", "b", "d"];
    assert_eq::assert_eq!(a, b);
}

#[test]
fn slice_of_struct_eq() {
    #[derive(AssertEq, Debug)]
    struct S {
        a: i32,
        b: String,
    }
    let a = &[S {
        a: 1,
        b: "hello".to_owned(),
    }];
    let b = &[S {
        a: 1,
        b: "hello".to_owned(),
    }];
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "assertion `left == right` failed: at [0] → .b\n  left: \"hello\"\n right: \"world\""
)]
fn slice_of_struct_diff() {
    #[derive(AssertEq, Debug)]
    struct S {
        a: i32,
        b: String,
    }
    let a = &[S {
        a: 1,
        b: "hello".to_owned(),
    }];
    let b = &[S {
        a: 1,
        b: "world".to_owned(),
    }];
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "assertion `left == right` failed: at [0] → .a\n  left: 0\n right: 1")]
fn slice_of_struct_diff_2() {
    #[derive(AssertEq, Debug)]
    struct S {
        a: i32,
        b: String,
    }
    let a = &[S {
        a: 0,
        b: "hello".to_owned(),
    }];
    let b = &[S {
        a: 1,
        b: "world".to_owned(),
    }];
    assert_eq::assert_eq!(a, b);
}

#[test]
fn generics() {
    #[derive(assert_eq::AssertEq)]
    struct FmtWrapper<T>(T);

    let a = FmtWrapper(1);
    let b = FmtWrapper(1);
    assert_eq::assert_eq!(a, b);
}

#[test]
fn generics_nested() {
    #[derive(assert_eq::AssertEq, Debug)]
    struct FmtWrapper<T>(T);

    #[derive(assert_eq::AssertEq, Debug)]
    struct Container<T> {
        a: FmtWrapper<T>,
        b: Option<FmtWrapper<T>>,
    }

    let a = Container {
        a: FmtWrapper(1),
        b: Some(FmtWrapper(2)),
    };
    let b = Container {
        a: FmtWrapper(1),
        b: Some(FmtWrapper(2)),
    };
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(
    expected = "assertion `left == right` failed: at .b → [Some] → .0\n  left: 2\n right: 3"
)]
fn generics_nested_fail() {
    #[derive(assert_eq::AssertEq, Debug)]
    struct FmtWrapper<T>(T);

    #[derive(assert_eq::AssertEq, Debug)]
    struct Container<T> {
        a: FmtWrapper<T>,
        b: Option<FmtWrapper<T>>,
    }

    let a = Container {
        a: FmtWrapper(1),
        b: Some(FmtWrapper(2)),
    };
    let b = Container {
        a: FmtWrapper(1),
        b: Some(FmtWrapper(3)),
    };
    assert_eq::assert_eq!(a, b);
}

#[test]
fn generics_in_slice() {
    #[derive(assert_eq::AssertEq, Debug)]
    struct FmtWrapper<T>(T);

    let a = &[FmtWrapper(1), FmtWrapper(2)];
    let b = &[FmtWrapper(1), FmtWrapper(2)];
    assert_eq::assert_eq!(a, b);
}

#[test]
#[should_panic(expected = "assertion `left == right` failed: at [1] → .0\n  left: 2\n right: 3")]
fn generics_in_slice_fail() {
    #[derive(assert_eq::AssertEq, Debug)]
    struct FmtWrapper<T>(T);

    let a = &[FmtWrapper(1), FmtWrapper(2)];
    let b = &[FmtWrapper(1), FmtWrapper(3)];
    assert_eq::assert_eq!(a, b);
}
