use crate::{parse_str, parse_str_one, Lexer, Token};
use proptest::prelude::*;
use stahl_errors::{Location, PointLC};
use stahl_value::Value;

proptest! {
    #[test]
    fn sexprs_parse(v in any::<Value>()) {
        let s = v.to_string();
        let v2 = parse_str_one(&s, Location::new()).unwrap();
        assert_eq!(v, v2);
    }
}

#[test]
fn edge_cases() {
    assert_eq!(
        Lexer::new("").collect::<Result<Vec<_>, _>>().unwrap(),
        vec![(PointLC(0, 1, 1), Token::Newline, PointLC(0, 1, 1))]
    );

    assert_eq!(
        Lexer::new("    ").collect::<Result<Vec<_>, _>>().unwrap(),
        vec![(PointLC(4, 1, 5), Token::Newline, PointLC(4, 1, 5))]
    );

    assert_eq!(
        Lexer::new(" \n").collect::<Result<Vec<_>, _>>().unwrap(),
        vec![(PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1)),]
    );
}

#[test]
fn simple() {
    let s = "a\n b\n c";

    let toks = Lexer::new(s).collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(
        toks,
        vec![
            (
                PointLC(0, 1, 1),
                Token::Symbol("a".into()),
                PointLC(1, 1, 2),
            ),
            (PointLC(2, 2, 1), Token::Indent, PointLC(3, 2, 2)),
            (
                PointLC(3, 2, 2),
                Token::Symbol("b".into()),
                PointLC(4, 2, 3),
            ),
            (
                PointLC(4, 2, 3),
                Token::Symbol("c".into()),
                PointLC(7, 3, 3),
            ),
            (PointLC(7, 3, 3), Token::Dedent, PointLC(7, 3, 3)),
        ]
    );

    let val = parse_str_one(s, Location::new()).unwrap();
    assert_eq!(
        val,
        Value::from_iter(vec![
            Value::Symbol(Location::new(), "a".into()),
            Value::Symbol(Location::new(), "b".into()),
            Value::Symbol(Location::new(), "c".into()),
        ])
    );
}

#[test]
fn group() {
    let s = "a\n group\n  b\n    c\n  d\n e";

    let toks = Lexer::new(s).collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(
        toks,
        vec![
            (
                PointLC(0, 1, 1),
                Token::Symbol("a".into()),
                PointLC(1, 1, 2),
            ),
            (PointLC(2, 2, 1), Token::Indent, PointLC(3, 2, 2)),
            (PointLC(3, 2, 2), Token::Group, PointLC(8, 2, 7)),
            (PointLC(9, 3, 1), Token::Indent, PointLC(11, 3, 3)),
            (
                PointLC(11, 3, 3),
                Token::Symbol("b".into()),
                PointLC(12, 3, 4),
            ),
            (PointLC(13, 4, 1), Token::Indent, PointLC(17, 4, 5),),
            (
                PointLC(17, 4, 5),
                Token::Symbol("c".into()),
                PointLC(18, 4, 6),
            ),
            (PointLC(19, 5, 1), Token::Dedent, PointLC(21, 5, 3)),
            (
                PointLC(21, 5, 3),
                Token::Symbol("d".into()),
                PointLC(22, 5, 4),
            ),
            (PointLC(23, 6, 1), Token::Dedent, PointLC(24, 6, 2)),
            (
                PointLC(24, 6, 2),
                Token::Symbol("e".into()),
                PointLC(25, 6, 3),
            ),
            (PointLC(25, 6, 3), Token::Dedent, PointLC(25, 6, 3)),
        ]
    );

    let val = parse_str_one(s, Location::new()).unwrap();
    assert_eq!(
        val,
        Value::from_iter(vec![
            Value::Symbol(Location::new(), "a".into()),
            Value::from_iter(vec![
                Value::from_iter(vec![
                    Value::Symbol(Location::new(), "b".into()),
                    Value::Symbol(Location::new(), "c".into()),
                ]),
                Value::Symbol(Location::new(), "d".into()),
            ]),
            Value::Symbol(Location::new(), "e".into()),
        ])
    );
}

#[test]
fn srfi_49_sparse_1() {
    let s = r#"
      define
        fac x
        if
         = x 0
         1
         * x
           fac
             - x 1

       let
        group
         foo
          + 1 2
         bar
          + 3 4
        + foo bar
    "#;

    let val = parse_str(&s, Location::new()).unwrap();
    assert_eq!(
        val,
        vec![
            Value::from_iter(vec![
                Value::Symbol(Location::new(), "define".into()),
                Value::from_iter(vec![
                    Value::Symbol(Location::new(), "fac".into()),
                    Value::Symbol(Location::new(), "x".into()),
                ]),
                Value::from_iter(vec![
                    Value::Symbol(Location::new(), "if".into()),
                    Value::from_iter(vec![
                        Value::Symbol(Location::new(), "=".into()),
                        Value::Symbol(Location::new(), "x".into()),
                        Value::Int(Location::new(), 0),
                    ]),
                    Value::Int(Location::new(), 1),
                    Value::from_iter(vec![
                        Value::Symbol(Location::new(), "*".into()),
                        Value::Symbol(Location::new(), "x".into()),
                        Value::from_iter(vec![
                            Value::Symbol(Location::new(), "fac".into()),
                            Value::from_iter(vec![
                                Value::Symbol(Location::new(), "-".into()),
                                Value::Symbol(Location::new(), "x".into()),
                                Value::Int(Location::new(), 1),
                            ]),
                        ]),
                    ]),
                ])
            ]),
            Value::from_iter(vec![
                Value::Symbol(Location::new(), "let".into()),
                Value::from_iter(vec![
                    Value::from_iter(vec![
                        Value::Symbol(Location::new(), "foo".into()),
                        Value::from_iter(vec![
                            Value::Symbol(Location::new(), "+".into()),
                            Value::Int(Location::new(), 1),
                            Value::Int(Location::new(), 2),
                        ]),
                    ]),
                    Value::from_iter(vec![
                        Value::Symbol(Location::new(), "bar".into()),
                        Value::from_iter(vec![
                            Value::Symbol(Location::new(), "+".into()),
                            Value::Int(Location::new(), 3),
                            Value::Int(Location::new(), 4),
                        ]),
                    ]),
                ]),
                Value::from_iter(vec![
                    Value::Symbol(Location::new(), "+".into()),
                    Value::Symbol(Location::new(), "foo".into()),
                    Value::Symbol(Location::new(), "bar".into()),
                ]),
            ])
        ]
    );
}
