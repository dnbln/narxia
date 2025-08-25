use std::env;
use std::fmt::Write as _;
use std::fs;
use std::path::PathBuf;

use narxia_workspace::name_resolution_tests;
use narxia_workspace::parser_tests;
use narxia_workspace::ssa_tests;

fn main() {
    println!("cargo::rerun-if-changed=testData");
    println!("cargo::rerun-if-changed=build.rs");

    let parser_tests = parser_tests::collect_parser_tests().unwrap();
    let name_resolution_tests = name_resolution_tests::collect_name_resolution_tests().unwrap();
    let ssa_tests = ssa_tests::collect_ssa_tests().unwrap();

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    let mut parser_tests_out = String::from(
        r#"
"#
        .trim_start(),
    );
    for parser_test in parser_tests.iter() {
        let dir_name = parser_test.file_name().to_str().unwrap();
        let test_name = dir_name.replace("-", "_");
        writeln!(
            &mut parser_tests_out,
            r#"
#[test]
fn {test_name}() -> miette::Result<()> {{
    use miette::IntoDiagnostic;

    crate::run_test_main(narxia_workspace::parser_tests::load_parser_test({dir_name:?}).into_diagnostic()?)
}}

"#
        )
        .unwrap();
    }

    fs::write(out_dir.join("parser_tests.rs"), parser_tests_out).unwrap();

    let mut name_resolution_tests_out = String::from(
        r#"
"#
        .trim_start(),
    );
    for name_resolution_test in name_resolution_tests.iter() {
        let dir_name = name_resolution_test.file_name().to_str().unwrap();
        let test_name = dir_name.replace("-", "_");
        writeln!(
            &mut name_resolution_tests_out,
            r#"
#[test]
fn {test_name}() -> miette::Result<()> {{
    use miette::IntoDiagnostic;

    crate::run_test_main(narxia_workspace::name_resolution_tests::load_name_resolution_test({dir_name:?}).into_diagnostic()?)
}}

"#).unwrap();
    }

    fs::write(
        out_dir.join("name_resolution_tests.rs"),
        name_resolution_tests_out,
    )
    .unwrap();

    let mut ssa_tests_out = String::from(
        r#"
"#
        .trim_start(),
    );
    for ssa_test in ssa_tests.iter() {
        let dir_name = ssa_test.file_name().to_str().unwrap();
        let test_name = dir_name.replace("-", "_");
        writeln!(
            &mut ssa_tests_out,
            r#"
#[test]
fn {test_name}() -> miette::Result<()> {{
    use miette::IntoDiagnostic;

    crate::run_test_main(narxia_workspace::ssa_tests::load_ssa_test({dir_name:?}).into_diagnostic()?)
}}

"#
        )
        .unwrap();
    }

    fs::write(out_dir.join("ssa_tests.rs"), ssa_tests_out).unwrap();
}
