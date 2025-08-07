use std::env;
use std::fs;
use std::path::Path;

use doc_extract::Session;
use doc_patchup::patchup_doc;
use doc_patchup::perform_end;

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
        let before = fs::read_to_string(&p).expect("Failed to read file");
        let code = patchup_doc(&session, before).await;

        if let Err(err) = perform_end(&code, &p, check_mode) {
            eprintln!("Error processing {}: {}", p.display(), err);
            break;
        }
    }

    session.shutdown().await;
}
