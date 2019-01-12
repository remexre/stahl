use crate::{parse_str, Value};
use proptest::prelude::*;
use stahl_errors::Location;
use stahl_util::SharedString;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ValueArbitraryParams {
    pub depth: u32,
    pub max_size: u32,
    pub max_collection_size: u32,
}

impl Default for ValueArbitraryParams {
    fn default() -> ValueArbitraryParams {
        ValueArbitraryParams {
            depth: 8,
            max_size: 256,
            max_collection_size: 10,
        }
    }
}

impl Arbitrary for Value {
    type Parameters = ValueArbitraryParams;
    type Strategy = BoxedStrategy<Value>;

    fn arbitrary_with(params: ValueArbitraryParams) -> Self::Strategy {
        let leaf = prop_oneof![
            Just(Value::Nil(Location::default())),
            any::<isize>().prop_map(|n| Value::Int(Location::default(), n)),
            ".*".prop_map(|s| Value::String(Location::default(), SharedString::from(&s))),
            // TODO: Cover all symbols.
            "[A-Za-z*/:][0-9A-Za-z*+/:-]*"
                .prop_map(|s| Value::Symbol(Location::default(), SharedString::from(&s))),
            "[+-][0-9]*[A-Za-z*+/:-]+[0-9]*"
                .prop_map(|s| Value::Symbol(Location::default(), SharedString::from(&s))),
        ];
        leaf.prop_recursive(
            params.depth,
            params.max_size,
            params.max_collection_size,
            |inner| {
                (inner.clone(), inner)
                    .prop_map(|(h, t)| Value::Cons(Location::default(), Box::new(h), Box::new(t)))
            },
        )
        .boxed()
    }
}

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
        let v2 = s.parse::<Value>().unwrap();
        assert_eq!(v, v2);
    }
}
