//! The Stahl interpreter.
#![deny(missing_docs)]

#[macro_use]
extern crate stahl_errors;

use stahl_ast::{Decl, Expr, Intrinsic, Literal};
use stahl_context::Context;
use stahl_util::SharedString;
use std::sync::Arc;

/// An interpreter for Stahl. Note that this is a "slow" interpreter, i.e. one that operates
/// directly on the AST, rather than compiling to a more efficient intermediate representation.
#[derive(Debug)]
pub struct Interpreter<'c> {
    ctx: &'c Context,
    env: Vec<(SharedString, Arc<Expr>)>,
}

impl<'c> Interpreter<'c> {
    /// Creates an interpreter that wraps a module context.
    pub fn new(ctx: &'c Context) -> Interpreter<'c> {
        Interpreter {
            ctx,
            env: Vec::new(),
        }
    }

    /// Evalutates the given expression.
    pub fn eval(&mut self, mut expr: Arc<Expr>) -> Arc<Expr> {
        loop {
            if self.is_normal(&expr) {
                return expr;
            }
            expr = self.eval_step(expr);
        }
    }

    /// The actual evaluator.
    fn eval_step(&mut self, expr: Arc<Expr>) -> Arc<Expr> {
        match *expr {
            Expr::Const(_, _) | Expr::Intrinsic(_, _) | Expr::Lam(_, _, _) => {
                warn!("{} is already normal; this may be a bug.", expr);
                expr
            }
            Expr::GlobalVar(_, ref name) => match self.ctx.get_decl(name.clone(), false).unwrap() {
                (_, Decl::Def(_, _, _, expr)) => expr.clone(),
                (_, decl) => panic!("{} is not a def", decl),
            },
            Expr::LocalVar(_, ref name) => {
                for (n, expr) in self.env.iter().cloned().rev() {
                    if n == &*name {
                        return expr.clone();
                    }
                }
                panic!("Undefined local variable: {}", name)
            }
            Expr::Pi(_, _, _, _) => {
                unimplemented!("TODO Pi normalization");
            }
            Expr::Call(ref loc, ref func, ref call_args) => {
                let func = self.eval(func.clone());
                let call_args = call_args
                    .iter()
                    .map(|a| self.eval(a.clone()))
                    .collect::<Vec<_>>();
                match *func {
                    Expr::Lam(_, ref args, ref body) => {
                        if args.len() != call_args.len() {
                            panic!(
                                "{} is being passed {} args but expects {}",
                                func,
                                call_args.len(),
                                args.len()
                            );
                        }

                        let old_env_len = self.env.len();
                        self.env.extend(args.iter().cloned().zip(call_args));

                        let mut body = body.clone();
                        let last = body.pop().expect("An empty lambda slipped in").2;

                        for (name, _, expr, _) in body {
                            let value = self.eval(expr);
                            if let Some(name) = name {
                                self.env.push((name, value));
                            }
                        }

                        let val = self.eval(last);
                        self.env.truncate(old_env_len);
                        val
                    }
                    Expr::Intrinsic(_, Intrinsic::Tag(_)) => {
                        Arc::new(Expr::Call(loc.clone(), func, call_args))
                    }
                    Expr::Intrinsic(_, Intrinsic::FixnumAdd) => match &*call_args {
                        [l, r] => match (&**l, &**r) {
                            (
                                Expr::Const(_, Literal::Int(_, l)),
                                Expr::Const(_, Literal::Int(_, r)),
                            ) => {
                                Arc::new(Expr::Const(loc.clone(), Literal::Int(loc.clone(), l + r)))
                            }
                            _ => panic!("Type error in call to +"),
                        },
                        _ => panic!("Invalid argn in call to +"),
                    },
                    _ => panic!("{} is not callable", func),
                }
            }
        }
    }

    /// Returns whether the given expression is normal.
    fn is_normal(&mut self, expr: &Expr) -> bool {
        match expr {
            Expr::Const(_, _) | Expr::Intrinsic(_, _) | Expr::Lam(_, _, _) => true,
            Expr::GlobalVar(_, _) | Expr::LocalVar(_, _) => false,
            Expr::Call(_, func, args) => {
                if args.iter().all(|a| self.is_normal(a)) {
                    match &**func {
                        Expr::Intrinsic(_, Intrinsic::Tag(_)) => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }
            Expr::Pi(_, _, _, _) => unimplemented!("TODO Pi normalization"),
        }
    }
}
