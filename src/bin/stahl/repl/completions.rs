use crate::repl::Helper;
use rustyline::{completion::Completer, hint::Hinter, Result};
use stahl_errors::PointLC;
use stahl_parser::{Lexer, Token};

impl Completer for Helper<'_, '_, '_> {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize) -> Result<(usize, Vec<String>)> {
        let (p, cs) = self.completions(line, pos);

        // Special case: if we're at the end of the line and there are hinted characters, complete
        // one of them per tab.
        if pos == line.len() && cs.is_empty() {
            if let Some(mut s) = self.hint(line, pos) {
                s.drain(1..);
                return Ok((pos, vec![s]));
            }
        }

        Ok((p, cs))
    }
}

impl Helper<'_, '_, '_> {
    pub(crate) fn completions(&self, line: &str, pos: usize) -> (usize, Vec<String>) {
        Lexer::new(line)
            .take_while(|r| r.is_ok())
            .map(|r| r.unwrap())
            .find(|&(PointLC(s, _, _), _, PointLC(e, _, _))| s < pos && pos <= e)
            .and_then(|(PointLC(s, _, _), tok, _)| match tok {
                Token::Symbol(sym) => Some((s, self.completions_of(&sym[..pos - s]))),
                _ => None,
            })
            .unwrap_or_else(|| (pos, Vec::new()))
    }

    fn completions_of(&self, partial: &str) -> Vec<String> {
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
