//! Tests we want to fail "first."

use stahl_context::Context;
use std::path::PathBuf;

#[test]
fn std_can_be_imported() {
    Context::new(true, Some(PathBuf::from(".").into())).unwrap();
}
