use crate::repl::Helper;
use rustyline::{completion::Completer, Result};
use stahl_errors::PointLC;
use stahl_parser::{Lexer, Token};

impl Completer for Helper<'_, '_, '_> {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize) -> Result<(usize, Vec<String>)> {
        Ok(Lexer::new(line)
            .take_while(|r| r.is_ok())
            .map(|r| r.unwrap())
            .find(|&(PointLC(s, _, _), _, PointLC(e, _, _))| s < pos && pos <= e)
            .and_then(|(PointLC(s, _, _), tok, _)| match tok {
                Token::Symbol(sym) => Some((s, self.completions(&sym[..pos - s]))),
                _ => None,
            })
            .unwrap_or_else(|| (pos, Vec::new())))
    }
}

impl Helper<'_, '_, '_> {
    fn completions(&self, partial: &str) -> Vec<String> {
        if partial.contains(':') {
            warn!("TODO: completion of global names");
        }

        self.0
            .borrow()
            .local_name_iter()
            .filter(|s| s.starts_with(partial))
            .map(|s| s.to_string())
            .collect()
    }
}
