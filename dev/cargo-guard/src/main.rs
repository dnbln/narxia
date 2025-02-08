fn main() {
    eprintln!(
        r#"
Hello, and welcome to narxia! Given that the build
system is a bit complex, it's easy to get lost.
You probably just ran `cargo run`, as you would in
a normal Rust project. However, here we use a
custom cargo driver called nexus. Run
`cargo nexus --help` to get acquainted with it.
"#
    );

    std::process::exit(1);
}
