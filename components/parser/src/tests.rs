use crate::{parse_str, Value};
use proptest::prelude::*;

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
            Just(Value::Nil),
            any::<isize>().prop_map(Value::Int),
            ".*".prop_map(Value::String),
            // TODO: Cover all symbols.
            "[A-Za-z*/:][0-9A-Za-z*+/:-]*".prop_map(Value::Symbol),
            "[+-][0-9]*[A-Za-z*+/:-]+[0-9]*".prop_map(Value::Symbol),
        ];
        leaf.prop_recursive(
            params.depth,
            params.max_size,
            params.max_collection_size,
            |inner| (inner.clone(), inner).prop_map(|(h, t)| Value::Cons(Box::new(h), Box::new(t))),
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
            Value::Symbol("foo".to_string()),
            Value::Cons(
                Box::new(Value::Symbol("quux".to_string())),
                Box::new(Value::Cons(
                    Box::new(Value::Symbol("foo-again".to_string())),
                    Box::new(Value::Nil),
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
