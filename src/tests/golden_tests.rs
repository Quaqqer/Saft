#[cfg(test)]
mod tests_ {

    use goldentests::{TestConfig, TestResult};

    #[test]
    fn goldentests() -> TestResult<()> {
        let config = TestConfig::new("target/debug/saft", "tests/", "# ")?;
        config.run_tests()
    }
}
