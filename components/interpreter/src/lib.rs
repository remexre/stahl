//! The Stahl interpreter.
#![deny(missing_docs)]

use stahl_ast::Expr;
use stahl_context::ModContext;

/// An interpreter for Stahl. Note that this is a "slow" interpreter, i.e. one that operates
/// directly on the AST, rather than compiling to a more efficient intermediate representation.
#[derive(Debug)]
pub struct Interpreter<'a, 'l, 'c> {
    mod_ctx: &'a mut ModContext<'l, 'c>,
}

impl<'a, 'l, 'c> Interpreter<'a, 'l, 'c> {
    /// Creates an interpreter that wraps a module context.
    pub fn new(mod_ctx: &'a mut ModContext<'l, 'c>) -> Interpreter<'a, 'l, 'c> {
        Interpreter { mod_ctx }
    }

    /// Returns whether the given expression is normal.
    pub fn is_normal(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Call(_, _func, _args) => false,      // TODO
            Expr::GlobalVar(_, _name) => false,        // TODO
            Expr::Lam(_, _args, _body) => false,       // TODO
            Expr::LocalVar(_, _name) => false,         // TODO
            Expr::Pi(_, _args, _body, _effs) => false, // TODO
            Expr::Const(_, _) | Expr::Type(_) | Expr::TypeOfTypeOfTypes(_) => true,
        }
    }
}
