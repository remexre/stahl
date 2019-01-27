use stahl_context::Context;
use stahl_errors::{PointLC, Position};
use std::path::PathBuf;

#[test]
fn should_fail() {
    let mut ctx = Context::new(true, Some(PathBuf::from(".").into())).unwrap();

    let paths = &[
        (
            "tests/should_fail/two-plus-two.stahl",
            Some(Position::SpanLC(PointLC(111, 6, 1), PointLC(116, 6, 6))),
            "2 + 2 =/= 4",
        ),
        (
            "tests/should_fail/negative-position-in-ctor.stahl",
            Some(Position::SpanLC(PointLC(111, 6, 1), PointLC(116, 6, 6))),
            "Bad cannot appear in a negative position",
        ),
    ];

    for (path, pos, msg) in paths {
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
