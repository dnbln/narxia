use std::fmt::Write as _;
use std::fs;
use std::path::Path;

use doc_extract::Session;

#[tokio::main]
async fn main() {
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
        let mut previous_r = false;
        let mut in_doc_tooltip = false;

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
                writeln!(&mut after, "{}", docs.contents).unwrap();
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
                writeln!(&mut after, "{}", docs.contents).unwrap();
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
            }
        }

        if after != before {
            let new_path = p.with_extension("before.mdx");
            fs::rename(&p, &new_path).expect("Failed to rename before file");
            println!("Updating {}", p.display());
            fs::write(&p, after).expect("Failed to write updated file");
        } else {
            println!("No changes for {}", p.display());
        }
    }

    session.shutdown().await;
}
