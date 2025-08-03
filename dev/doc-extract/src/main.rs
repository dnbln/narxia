use doc_extract::Session;
use serde_json::json;

#[tokio::main]
async fn main() {
    let args = std::env::args().skip(1);

    let working_directory = std::env::current_dir().expect("Unable to get current directory");
    let session = Session::new(working_directory).await;

    for target_name in args {
        let docs = session
            .query_symbol(&target_name)
            .await
            .expect("Failed to extract docs");

        let out = json!({
            "name": target_name,
            "doc": docs.contents,
        });

        println!("{}", serde_json::to_string(&out).unwrap());
    }

    session.shutdown().await;
}
