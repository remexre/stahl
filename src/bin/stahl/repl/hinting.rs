use crate::repl::Helper;
use rustyline::hint::Hinter;

impl Hinter for Helper<'_, '_, '_> {
    fn hint(&self, line: &str, _pos: usize) -> Option<String> {
        let mut open_parens = 0usize;
        let mut close_parens = 0usize;
        let mut quotes = 0usize;

        for c in line.chars() {
            match c {
                '(' => open_parens += 1,
                ')' => close_parens += 1,
                '"' => quotes += 1,
                _ => {}
            }
        }

        let mut hint = String::new();
        if quotes % 2 == 1 {
            hint.push('"');
        }
        for _ in 0..open_parens.saturating_sub(close_parens) {
            hint.push(')');
        }

        if hint.is_empty() {
            None
        } else {
            Some(hint)
        }
    }
}
