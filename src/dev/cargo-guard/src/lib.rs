#[cfg(test)]
mod tests {
    use std::env;

    #[test]
    fn guard() {
        if let Err(env::VarError::NotPresent) = env::var("NARXIA_TEST_GUARD") {
            panic!("NARXIA_TEST_GUARD not set; did you use `cargo nexus test` to run the tests?");
        }
    }
}
