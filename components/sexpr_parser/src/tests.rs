use crate::{parse_str, parse_str_one};
use proptest::prelude::*;
use stahl_errors::Location;
use stahl_util::SharedString;
use stahl_value::Value;

#[test]
fn comments() {
    let v = parse_str(
        r#"foo
; bar; baz
(quux
; xyzzy
foo-again)"#,
    )
    .unwrap();
    assert_eq!(
        v,
        vec![
            Value::Symbol(Location::default(), SharedString::from("foo")),
            Value::Cons(
                Location::default(),
                Box::new(Value::Symbol(
                    Location::default(),
                    SharedString::from("quux")
                )),
                Box::new(Value::Cons(
                    Location::default(),
                    Box::new(Value::Symbol(
                        Location::default(),
                        SharedString::from("foo-again")
                    )),
                    Box::new(Value::Nil(Location::default())),
                ))
            )
        ]
    );
}

proptest! {
    #[test]
    fn print_and_parse_is_idempotent(v in any::<Value>()) {
        let s = v.to_string();
        let v2 = parse_str_one(&s).unwrap();
        assert_eq!(v, v2);
    }
}
