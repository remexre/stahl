use crate::{
    lexer::{Lexer, Token},
    parse_str, parse_str_one,
};
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
        vec![(PointLC(0, 1, 1), Token::Newline, PointLC(0, 1, 1))],
        Lexer::new("").collect::<Result<Vec<_>, _>>().unwrap(),
    );

    assert_eq!(
        vec![(PointLC(4, 1, 5), Token::Newline, PointLC(4, 1, 5))],
        Lexer::new("    ").collect::<Result<Vec<_>, _>>().unwrap(),
    );

    assert_eq!(
        vec![(PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1))],
        Lexer::new(" \n").collect::<Result<Vec<_>, _>>().unwrap(),
    );

    assert_eq!(
        vec![(PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1))],
        Lexer::new(" \n;d").collect::<Result<Vec<_>, _>>().unwrap(),
    );

    assert_eq!(
        vec![
            (PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1)),
            (PointLC(2, 2, 1), Token::Newline, PointLC(5, 3, 1))
        ],
        Lexer::new(" \n;d\n")
            .collect::<Result<Vec<_>, _>>()
            .unwrap(),
    );
}

#[test]
fn simple_1() {
    let s = "a\n b\n c";

    assert_eq!(
        vec![
            (
                PointLC(0, 1, 1),
                Token::Symbol("a".into()),
                PointLC(1, 1, 2),
            ),
            (PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1)),
            (PointLC(2, 2, 1), Token::Indent, PointLC(3, 2, 2)),
            (
                PointLC(3, 2, 2),
                Token::Symbol("b".into()),
                PointLC(4, 2, 3),
            ),
            (PointLC(4, 2, 3), Token::Newline, PointLC(5, 3, 1)),
            (
                PointLC(6, 3, 2),
                Token::Symbol("c".into()),
                PointLC(7, 3, 3),
            ),
            (PointLC(7, 3, 3), Token::Newline, PointLC(7, 3, 3)),
            (PointLC(7, 3, 3), Token::Dedent, PointLC(7, 3, 3)),
        ],
        Lexer::new(s).collect::<Result<Vec<_>, _>>().unwrap(),
    );

    let val = parse_str_one(s, Location::new()).unwrap();
    assert_eq!(
        Value::from_iter(vec![
            Value::Symbol(Location::new(), "a".into()),
            Value::Symbol(Location::new(), "b".into()),
            Value::Symbol(Location::new(), "c".into()),
        ]),
        val,
    );
}

#[test]
fn simple_2() {
    let s = "a\n b\n  c";

    assert_eq!(
        vec![
            (
                PointLC(0, 1, 1),
                Token::Symbol("a".into()),
                PointLC(1, 1, 2),
            ),
            (PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1)),
            (PointLC(2, 2, 1), Token::Indent, PointLC(3, 2, 2)),
            (
                PointLC(3, 2, 2),
                Token::Symbol("b".into()),
                PointLC(4, 2, 3),
            ),
            (PointLC(4, 2, 3), Token::Newline, PointLC(5, 3, 1)),
            (PointLC(5, 3, 1), Token::Indent, PointLC(7, 3, 3)),
            (
                PointLC(7, 3, 3),
                Token::Symbol("c".into()),
                PointLC(8, 3, 4),
            ),
            (PointLC(8, 3, 4), Token::Newline, PointLC(8, 3, 4)),
            (PointLC(8, 3, 4), Token::Dedent, PointLC(8, 3, 4)),
            (PointLC(8, 3, 4), Token::Dedent, PointLC(8, 3, 4)),
        ],
        Lexer::new(s).collect::<Result<Vec<_>, _>>().unwrap(),
    );

    let val = parse_str_one(s, Location::new()).unwrap();
    assert_eq!(
        Value::from_iter(vec![
            Value::Symbol(Location::new(), "a".into()),
            Value::from_iter(vec![
                Value::Symbol(Location::new(), "b".into()),
                Value::Symbol(Location::new(), "c".into()),
            ]),
        ]),
        val,
    );
}

#[test]
fn simple_3() {
    let s = "a\n b\n  c\nd";

    assert_eq!(
        vec![
            (
                PointLC(0, 1, 1),
                Token::Symbol("a".into()),
                PointLC(1, 1, 2),
            ),
            (PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1)),
            (PointLC(2, 2, 1), Token::Indent, PointLC(3, 2, 2)),
            (
                PointLC(3, 2, 2),
                Token::Symbol("b".into()),
                PointLC(4, 2, 3),
            ),
            (PointLC(4, 2, 3), Token::Newline, PointLC(5, 3, 1)),
            (PointLC(5, 3, 1), Token::Indent, PointLC(7, 3, 3)),
            (
                PointLC(7, 3, 3),
                Token::Symbol("c".into()),
                PointLC(8, 3, 4),
            ),
            (PointLC(8, 3, 4), Token::Newline, PointLC(9, 4, 1)),
            (PointLC(9, 4, 1), Token::Dedent, PointLC(9, 4, 1)),
            (PointLC(9, 4, 1), Token::Dedent, PointLC(9, 4, 1)),
            (
                PointLC(9, 4, 1),
                Token::Symbol("d".into()),
                PointLC(10, 4, 2),
            ),
            (PointLC(10, 4, 2), Token::Newline, PointLC(10, 4, 2)),
        ],
        Lexer::new(s).collect::<Result<Vec<_>, _>>().unwrap(),
    );

    let val = parse_str(s, Location::new()).unwrap();
    assert_eq!(
        vec![
            Value::from_iter(vec![
                Value::Symbol(Location::new(), "a".into()),
                Value::from_iter(vec![
                    Value::Symbol(Location::new(), "b".into()),
                    Value::Symbol(Location::new(), "c".into()),
                ]),
            ]),
            Value::Symbol(Location::new(), "d".into()),
        ],
        val,
    );
}

#[test]
fn group() {
    // a
    //  group
    //   b
    //     c
    //   d
    let s = "a\n group\n  b\n    c\n  d\n e";

    assert_eq!(
        vec![
            (
                PointLC(0, 1, 1),
                Token::Symbol("a".into()),
                PointLC(1, 1, 2),
            ),
            (PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1)),
            (PointLC(2, 2, 1), Token::Indent, PointLC(3, 2, 2)),
            (PointLC(3, 2, 2), Token::Group, PointLC(8, 2, 7)),
            (PointLC(8, 2, 7), Token::Newline, PointLC(9, 3, 1)),
            (PointLC(9, 3, 1), Token::Indent, PointLC(11, 3, 3)),
            (
                PointLC(11, 3, 3),
                Token::Symbol("b".into()),
                PointLC(12, 3, 4),
            ),
            (PointLC(12, 3, 4), Token::Newline, PointLC(13, 4, 1),),
            (PointLC(13, 4, 1), Token::Indent, PointLC(17, 4, 5),),
            (
                PointLC(17, 4, 5),
                Token::Symbol("c".into()),
                PointLC(18, 4, 6),
            ),
            (PointLC(18, 4, 6), Token::Newline, PointLC(19, 5, 1)),
            (PointLC(19, 5, 1), Token::Dedent, PointLC(21, 5, 3)),
            (
                PointLC(21, 5, 3),
                Token::Symbol("d".into()),
                PointLC(22, 5, 4),
            ),
            (PointLC(22, 5, 4), Token::Newline, PointLC(23, 6, 1)),
            (PointLC(23, 6, 1), Token::Dedent, PointLC(24, 6, 2)),
            (
                PointLC(24, 6, 2),
                Token::Symbol("e".into()),
                PointLC(25, 6, 3),
            ),
            (PointLC(25, 6, 3), Token::Newline, PointLC(25, 6, 3)),
            (PointLC(25, 6, 3), Token::Dedent, PointLC(25, 6, 3)),
        ],
        Lexer::new(s).collect::<Result<Vec<_>, _>>().unwrap(),
    );

    let val = parse_str_one(s, Location::new()).unwrap();
    assert_eq!(
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
        ]),
        val,
    );
}

const SRFI49_SPARSE_1: &str = r#"
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

#[test]
fn srfi_49_sparse_1() {
    let val = parse_str(SRFI49_SPARSE_1, Location::new()).unwrap();
    assert_eq!(
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
        ],
        val,
    );
}

#[test]
fn let_thingy() {
    let s = r#"
let
  group
    foo
      + 1 2
    bar
      + 3 4
  + foo bar"#;
    assert_eq!(
        vec![
            Token::Newline,
            Token::Symbol("let".into()),
            Token::Newline,
            Token::Indent,
            Token::Group,
            Token::Newline,
            Token::Indent,
            Token::Symbol("foo".into()),
            Token::Newline,
            Token::Indent,
            Token::Symbol("+".into()),
            Token::Int(1),
            Token::Int(2),
            Token::Newline,
            Token::Dedent,
            Token::Symbol("bar".into()),
            Token::Newline,
            Token::Indent,
            Token::Symbol("+".into()),
            Token::Int(3),
            Token::Int(4),
            Token::Newline,
            Token::Dedent,
            Token::Dedent,
            Token::Symbol("+".into()),
            Token::Symbol("foo".into()),
            Token::Symbol("bar".into()),
            Token::Newline,
            Token::Dedent,
        ],
        Lexer::new(s).map(|r| r.unwrap().1).collect::<Vec<_>>()
    );
}
