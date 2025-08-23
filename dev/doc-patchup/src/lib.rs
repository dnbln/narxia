pub extern crate doc_extract;

use std::fmt::Write;
use std::fs;
use std::io;
use std::path::Path;

use doc_extract::Session;

pub struct Code {
    pub before: String,
    pub after: String,
    pub write_doctests: bool,
    pub all_code_for_doctests: String,
}

pub async fn patchup_doc(session: &Session, before: String) -> Code {
    let mut after = String::new();
    let mut all_code_for_doctests = String::new();

    let mut current_doctest = String::new();
    let mut current_doctest_req_features = Vec::new();

    let mut previous_r = false;
    let mut in_doc_tooltip = false;
    let mut write_doctests = false;
    let mut in_code_block = false;

    for line in before.lines() {
        if let Some(l) = line.strip_prefix("## !!doctooltips ") {
            // Process the line
            let l = l.trim();
            if l.ends_with("-R") {
                previous_r = true;
                writeln!(&mut after, "{line}").unwrap();
                continue;
            }
            let docs = session
                .query_symbol(l)
                .await
                .expect("Failed to extract docs");

            writeln!(&mut after, "{line}").unwrap();
            writeln!(&mut after).unwrap();
            writeln!(&mut after, "<DocTooltip>").unwrap();
            writeln!(&mut after, "{}", patch_rust_lines(docs.contents)).unwrap();
            writeln!(&mut after, "</DocTooltip>").unwrap();
        } else if previous_r {
            let l = line
                .trim()
                .strip_prefix('`')
                .expect("Expected ` prefix")
                .strip_suffix('`')
                .expect("Expected ` suffix");

            let docs = session
                .query_symbol(l)
                .await
                .expect("Failed to extract docs");

            writeln!(&mut after, "{line}").unwrap();
            writeln!(&mut after).unwrap();
            writeln!(&mut after, "<DocTooltip>").unwrap();
            writeln!(&mut after, "{}", patch_rust_lines(docs.contents)).unwrap();
            writeln!(&mut after, "</DocTooltip>").unwrap();

            previous_r = false;
        } else if line == "<DocTooltip>" {
            if after.ends_with("\n\n") {
                after.pop();
            }
            in_doc_tooltip = true;
        } else if line == "</DocTooltip>" {
            in_doc_tooltip = false;
        } else if in_doc_tooltip {
            // Skip lines inside the doctooltips block
            continue;
        } else {
            writeln!(&mut after, "{line}").unwrap();
            if line == "```rust" {
                current_doctest.clear();
                current_doctest_req_features.clear();
                writeln!(&mut current_doctest, "```rust").unwrap();
                in_code_block = true;
            } else if line.starts_with("```rust !") {
                current_doctest.clear();
                current_doctest_req_features.clear();
                in_code_block = true;
                let tag = if line.ends_with("no_run") {
                    write_doctests = true;
                    "no_run"
                } else if line.ends_with("ignore") {
                    write_doctests = true;
                    "ignore"
                } else if line.ends_with("should_panic") {
                    write_doctests = true;
                    "should_panic"
                } else if line.ends_with("compile_fail") {
                    write_doctests = true;
                    "compile_fail"
                } else {
                    writeln!(&mut current_doctest, "```rust").unwrap();
                    continue;
                };

                writeln!(&mut current_doctest, "```rust,{tag}").unwrap();
            } else if line == "```" {
                in_code_block = !in_code_block;
                if in_code_block {
                    current_doctest.clear();
                    current_doctest_req_features.clear();
                    writeln!(&mut current_doctest, "{line}").unwrap();
                } else {
                    writeln!(&mut current_doctest, "{line}").unwrap();
                    if current_doctest_req_features.is_empty() {
                        for line in current_doctest.lines() {
                            writeln!(&mut all_code_for_doctests, "/// {line}").unwrap();
                        }
                    } else {
                        write!(&mut all_code_for_doctests, "#[cfg_attr(all(").unwrap();
                        for feature in &current_doctest_req_features {
                            write!(&mut all_code_for_doctests, "feature = {feature:?}, ").unwrap();
                        }
                        writeln!(&mut all_code_for_doctests, "), doc = r##########\"{current_doctest}\"##########)]").unwrap();
                    }
                }
            } else if in_code_block {
                if let Some(tail) = line.strip_prefix("// !tail ") {
                    write_doctests = true;
                    writeln!(&mut current_doctest, "Ok::<_, {tail}>(())").unwrap();
                } else if let Some(hidden) = line.strip_prefix("// !hidden ") {
                    write_doctests = true;
                    writeln!(&mut current_doctest, "{hidden}").unwrap();
                } else if let Some(feature) = line.strip_prefix("// !req-feature ") {
                    write_doctests = true;
                    current_doctest_req_features.push(feature.trim().to_owned());
                } else if line == "// !lints" {
                    writeln!(
                        &mut current_doctest,
                        "{}",
                        r#"
#![deny(unused_imports)]
"#
                        .trim_start()
                    )
                    .unwrap();
                } else {
                    writeln!(&mut current_doctest, "{line}").unwrap();
                }
            } else {
                writeln!(&mut all_code_for_doctests, "/// {line}").unwrap();
            }
        }
    }

    writeln!(&mut all_code_for_doctests, "struct Guide;").unwrap();

    Code {
        before,
        after,
        write_doctests,
        all_code_for_doctests,
    }
}

fn patch_rust_lines(contents: String) -> String {
    contents
        .lines()
        .map(|line| {
            if line.starts_with("```rust") {
                "```rust ,ignore".to_owned()
            } else if line.starts_with("```") {
                "```".to_owned()
            } else {
                line.to_owned()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(Debug, thiserror::Error)]
pub enum PerformEndError {
    #[error("IO error: {0}")]
    IO(#[from] io::Error),
    #[error("Changes detected")]
    ChangesDetected,
}

pub fn perform_end(code: &Code, p: &Path, check_mode: bool) -> Result<(), PerformEndError> {
    if code.write_doctests {
        let name = p.file_name().unwrap().to_str().unwrap();
        let new_name = format!(".{name}.doctests");
        fs::write(p.with_file_name(new_name), &code.all_code_for_doctests)?;
    }

    if check_mode {
        if code.before == code.after {
            return Ok(());
        } else {
            // eprintln!("Changes detected in {}", p.display());
            // eprintln!("Run `cargo nexus build-sys doc-patchup-rustdocs` to update the file.");
            // process::exit(1);
            return Err(PerformEndError::ChangesDetected);
        }
    } else if code.before != code.after {
        let new_path = p.with_extension("before.mdx");
        match fs::rename(p, &new_path) {
            Ok(_) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                // If the file doesn't exist, we will just write the new one
            }
            Err(e) => {
                return Err(PerformEndError::IO(e));
            }
        };
        fs::write(p, &code.after)?;
    } else {
        return Ok(());
    }

    Ok(())
}
