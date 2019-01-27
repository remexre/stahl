use stahl_context::Context;
use std::path::PathBuf;

#[test]
fn regression_tests() {
    let mut ctx = Context::new(true, Some(PathBuf::from(".").into())).unwrap();
    stahl::run_script(&mut ctx, "tests/regression.stahl".into(), Vec::new()).unwrap();
}
