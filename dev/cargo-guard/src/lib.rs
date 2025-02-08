#[cfg(test)]
mod tests {
    #[test]
    fn guard() {
        if let Err(std::env::VarError::NotPresent) = std::env::var("NARXIA_TEST_GUARD") {
            panic!("NARXIA_TEST_GUARD not set; did you use `cargo nexus test` to run the tests?");
        }
    }
}