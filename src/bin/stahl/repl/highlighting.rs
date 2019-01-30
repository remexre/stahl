use crate::repl::Helper;
use ansi_term::{ANSIStrings, Color, Style};
use rustyline::highlight::Highlighter;
use stahl_errors::PointLC;
use stahl_parser::{Lexer, Token};
use std::borrow::Cow;

impl Highlighter for Helper<'_, '_, '_> {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut lexer = Lexer::new(line);
        let mut lexemes = Vec::new();
        while let Some(Ok(lexeme)) = lexer.next() {
            lexemes.push(lexeme);
        }

        let mut last = 0;
        let mut strs = lexemes
            .into_iter()
            .flat_map(|(PointLC(start, _, _), token, PointLC(end, _, _))| {
                let spacer = String::from_utf8(vec![' ' as u8; start - last]).unwrap();

                let style = match token {
                    Token::Int(_) => Color::Yellow.into(),
                    Token::String(_) => Color::Green.into(),
                    Token::Symbol(_) => Color::Blue.into(),
                    _ => Style::new(),
                };

                last = end;
                vec![
                    Style::new().paint(spacer),
                    style.paint(line[start..end].to_string()),
                ]
            })
            .collect::<Vec<_>>();
        strs.push(Color::Red.paint(line[last..].to_string()));
        Cow::Owned(ANSIStrings(&strs).to_string())
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned(Style::new().dimmed().paint(hint).to_string())
    }

    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        true
    }
}
