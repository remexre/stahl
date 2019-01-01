//! The Stahl interpreter.
#![deny(missing_docs)]

use stahl_ast::Expr;
use stahl_context::ModContext;

/// An interpreter for Stahl. Note that this is a "slow" interpreter, i.e. one that operates
/// directly on the AST, rather than compiling to a more efficient intermediate representation.
#[derive(Debug)]
pub struct Interpreter<'a> {
    mod_ctx: &'a mut ModContext<'a>,
}

impl<'a> Interpreter<'a> {
    /// Creates an interpreter that wraps a module context.
    pub fn new(mod_ctx: &'a mut ModContext<'a>) -> Interpreter<'a> {
        Interpreter { mod_ctx }
    }

    /// Returns whether the given expression is normal.
    pub fn is_normal(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Call(_, func, args) => false, // TODO
            Expr::Const(_, val) => true,
            Expr::GlobalVar(_, name) => false,      // TODO
            Expr::Lam(_, args, body) => false,      // TODO
            Expr::LocalVar(_, name) => false,       // TODO
            Expr::Pi(_, args, body, effs) => false, // TODO
            Expr::Type(_) => true,
        }
    }
}
