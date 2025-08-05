use std::env;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process;

use doc_extract::Session;

#[tokio::main]
async fn main() {
    let check_mode = env::args().skip(1).any(|arg| arg == "--check");
    let root_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let session = Session::new(&root_dir).await;

    let docs = root_dir.join("doc/docs/content/docs");

    for p in glob::glob(docs.join("**/*.mdx").to_str().unwrap())
        .expect("Failed to read glob pattern")
        .filter_map(Result::ok)
    {
        let before = fs::read_to_string(&p).unwrap();
        let mut after = String::new();
        let mut all_code_for_doctests = String::new();
        let mut previous_r = false;
        let mut in_doc_tooltip = false;
        let mut write_doctests = false;
        let mut in_code_block = false;

        for line in before.lines() {
            if let Some(l) = line.strip_prefix(" ## !!doctooltips ") {
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
                    writeln!(&mut all_code_for_doctests, "```rust").unwrap();
                    in_code_block = true;
                } else if line.starts_with("```rust !") {
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
                        writeln!(&mut all_code_for_doctests, "```rust").unwrap();
                        continue;
                    };

                    writeln!(&mut all_code_for_doctests, "```rust,{tag}").unwrap();
                } else if line == "```" {
                    in_code_block = !in_code_block;
                    writeln!(&mut all_code_for_doctests, "{}", line).unwrap();
                } else if in_code_block {
                    if let Some(tail) = line.strip_prefix("// !tail ") {
                        write_doctests = true;
                        writeln!(&mut all_code_for_doctests, "Ok::<_, {tail}>(())").unwrap();
                    } else if let Some(hidden) = line.strip_prefix("// !hidden ") {
                        write_doctests = true;
                        writeln!(&mut all_code_for_doctests, "{hidden}").unwrap();
                    } else if line == "// !lints" {
                        writeln!(
                            &mut all_code_for_doctests,
                            "{}",
                            r#"
#![deny(unused_imports)]
"#
                            .trim_start()
                        )
                        .unwrap();
                    } else {
                        writeln!(&mut all_code_for_doctests, "{}", line).unwrap();
                    }
                } else {
                    writeln!(&mut all_code_for_doctests, "{}", line).unwrap();
                }
            }
        }

        if check_mode {
            if after == before {
                println!("No changes for {}", p.display());
                continue;
            } else {
                eprintln!("Changes detected in {}", p.display());
                eprintln!("Run `cargo nexus build-sys doc-patchup-rustdocs` to update the file.");
                process::exit(1);
            }
        } else if after != before {
            let new_path = p.with_extension("before.mdx");
            fs::rename(&p, &new_path).expect("Failed to rename before file");
            println!("Updating {}", p.display());
            fs::write(&p, after).expect("Failed to write updated file");
        } else {
            println!("No changes for {}", p.display());
        }

        if write_doctests {
            let name = p.file_name().unwrap().to_str().unwrap();
            let new_name = format!(".{name}.doctests");
            fs::write(p.with_file_name(new_name), all_code_for_doctests)
                .expect("Failed to write doctests file");
        }
    }

    session.shutdown().await;
}

fn patch_rust_lines(contents: String) -> String {
    contents
        .lines()
        .map(|line| {
            if line.starts_with("```rust") {
                "```rust ,ignore".to_string()
            } else if line.starts_with("```") {
                "```".to_string()
            } else if line.starts_with("#") {
                format!(" {line}")
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
