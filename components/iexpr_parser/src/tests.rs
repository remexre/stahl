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
        vec![(PointLC(1, 1, 2), Token::Newline, PointLC(2, 2, 1))],
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
fn srfi_49_sparse_1_lex() {
    assert_eq!(
        vec![
            (PointLC(0, 1, 1), Token::Newline, PointLC(1, 2, 1)),
            (
                PointLC(1, 2, 1),
                Token::Symbol("define".into()),
                PointLC(7, 2, 7)
            ),
            (PointLC(7, 2, 7), Token::Newline, PointLC(8, 3, 1)),
            (PointLC(8, 3, 1), Token::Indent, PointLC(12, 3, 5)),
            (
                PointLC(12, 3, 5),
                Token::Symbol("fac".into()),
                PointLC(15, 3, 8)
            ),
            (
                PointLC(16, 3, 9),
                Token::Symbol("x".into()),
                PointLC(17, 3, 10)
            ),
            (PointLC(17, 3, 10), Token::Newline, PointLC(18, 4, 1)),
            (
                PointLC(22, 4, 5),
                Token::Symbol("if".into()),
                PointLC(24, 4, 7)
            ),
            (PointLC(24, 4, 7), Token::Newline, PointLC(25, 5, 1)),
            (PointLC(25, 5, 1), Token::Indent, PointLC(30, 5, 6)),
            (
                PointLC(30, 5, 6),
                Token::Symbol("=".into()),
                PointLC(31, 5, 7)
            ),
            (
                PointLC(32, 5, 8),
                Token::Symbol("x".into()),
                PointLC(33, 5, 9)
            ),
            (PointLC(34, 5, 10), Token::Int(0), PointLC(35, 5, 11)),
            (PointLC(35, 5, 11), Token::Newline, PointLC(36, 6, 1)),
            (PointLC(41, 6, 6), Token::Int(1), PointLC(42, 6, 7)),
            (PointLC(42, 6, 7), Token::Newline, PointLC(43, 7, 1)),
            (
                PointLC(48, 7, 6),
                Token::Symbol("*".into()),
                PointLC(49, 7, 7)
            ),
            (
                PointLC(50, 7, 8),
                Token::Symbol("x".into()),
                PointLC(51, 7, 9)
            ),
            (PointLC(51, 7, 9), Token::Newline, PointLC(52, 8, 1)),
            (PointLC(52, 8, 1), Token::Indent, PointLC(59, 8, 8)),
            (
                PointLC(59, 8, 8),
                Token::Symbol("fac".into()),
                PointLC(62, 8, 11)
            ),
            (PointLC(62, 8, 11), Token::Newline, PointLC(63, 9, 1)),
            (PointLC(63, 9, 1), Token::Indent, PointLC(72, 9, 10)),
            (
                PointLC(72, 9, 10),
                Token::Symbol("-".into()),
                PointLC(73, 9, 11)
            ),
            (
                PointLC(74, 9, 12),
                Token::Symbol("x".into()),
                PointLC(75, 9, 13)
            ),
            (PointLC(76, 9, 14), Token::Int(1), PointLC(77, 9, 15)),
            (PointLC(77, 9, 15), Token::Newline, PointLC(78, 10, 1)),
            (PointLC(78, 10, 1), Token::Dedent, PointLC(78, 10, 1)),
            (PointLC(78, 10, 1), Token::Dedent, PointLC(78, 10, 1)),
            (PointLC(78, 10, 1), Token::Dedent, PointLC(78, 10, 1)),
            (PointLC(78, 10, 1), Token::Dedent, PointLC(78, 10, 1)),
            (PointLC(78, 10, 1), Token::Newline, PointLC(79, 11, 1)),
            (
                PointLC(79, 11, 1),
                Token::Symbol("let".into()),
                PointLC(82, 11, 4)
            ),
            (PointLC(82, 11, 4), Token::Newline, PointLC(83, 12, 1)),
            (PointLC(83, 12, 1), Token::Indent, PointLC(86, 12, 4)),
            (PointLC(86, 12, 4), Token::Group, PointLC(91, 12, 9)),
            (PointLC(91, 12, 9), Token::Newline, PointLC(92, 13, 1)),
            (PointLC(92, 13, 1), Token::Indent, PointLC(96, 13, 5)),
            (
                PointLC(96, 13, 5),
                Token::Symbol("foo".into()),
                PointLC(99, 13, 8)
            ),
            (PointLC(99, 13, 8), Token::Newline, PointLC(100, 14, 1)),
            (PointLC(100, 14, 1), Token::Indent, PointLC(105, 14, 6)),
            (
                PointLC(105, 14, 6),
                Token::Symbol("+".into()),
                PointLC(106, 14, 7)
            ),
            (PointLC(107, 14, 8), Token::Int(1), PointLC(108, 14, 9)),
            (PointLC(109, 14, 10), Token::Int(2), PointLC(110, 14, 11)),
            (PointLC(110, 14, 11), Token::Newline, PointLC(111, 15, 1)),
            (PointLC(111, 15, 1), Token::Dedent, PointLC(115, 15, 5)),
            (
                PointLC(115, 15, 5),
                Token::Symbol("bar".into()),
                PointLC(118, 15, 8)
            ),
            (PointLC(118, 15, 8), Token::Newline, PointLC(119, 16, 1)),
            (
                PointLC(124, 16, 6),
                Token::Symbol("+".into()),
                PointLC(125, 16, 7)
            ),
            (PointLC(126, 16, 8), Token::Int(3), PointLC(127, 16, 9)),
            (PointLC(128, 16, 10), Token::Int(4), PointLC(129, 16, 11)),
            (PointLC(129, 16, 11), Token::Newline, PointLC(130, 17, 1)),
            (PointLC(130, 17, 1), Token::Dedent, PointLC(133, 17, 4)),
            (
                PointLC(133, 17, 4),
                Token::Symbol("+".into()),
                PointLC(134, 17, 5)
            ),
            (
                PointLC(135, 17, 6),
                Token::Symbol("foo".into()),
                PointLC(138, 17, 9)
            ),
            (
                PointLC(139, 17, 10),
                Token::Symbol("bar".into()),
                PointLC(142, 17, 13)
            ),
            (PointLC(142, 17, 13), Token::Newline, PointLC(143, 18, 1)),
            (PointLC(143, 18, 1), Token::Dedent, PointLC(143, 18, 1)),
            (PointLC(143, 18, 1), Token::Newline, PointLC(143, 18, 1))
        ],
        Lexer::new(SRFI49_SPARSE_1)
            .collect::<Result<Vec<_>, _>>()
            .unwrap(),
    );
}

#[test]
fn srfi_49_sparse_1_parse() {
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
