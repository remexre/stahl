//! Tests for code that should fail to compile.

use stahl_context::Context;
use stahl_errors::{PointLC, Position};
use std::path::PathBuf;

static TEST_CASES: &[(&str, Option<Position>, &str)] = &[
    /*
    ("tests/should_fail/bogus-elim.stahl", None, "TODO"),
    (
        "tests/should_fail/negative-position-in-ctor.stahl",
        Some(Position::SpanLC(PointLC(111, 6, 1), PointLC(116, 6, 6))),
        "Bad cannot appear in a negative position",
    ),
    (
        "tests/should_fail/two-plus-two.stahl",
        Some(Position::SpanLC(PointLC(111, 6, 1), PointLC(116, 6, 6))),
        "2 + 2 =/= 4",
    ),
    */
];

#[test]
fn should_fail() {
    let mut ctx = Context::new(true, Some(PathBuf::from(".").into())).unwrap();

    for (path, pos, msg) in TEST_CASES {
        let r = stahl::run_script(&mut ctx, path.into(), Vec::new());
        match r.err() {
            Some(e) => {
                assert_eq!(&e.to_string(), msg);
                assert_eq!(&e.loc.pos, pos);
            }
            None => panic!("{} should've failed, but didn't!", path),
        }
    }
}
