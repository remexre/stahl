use crate::split_vec::SplitVec;
use stahl_ast::{Effects, Expr};
use stahl_errors::Location;
use stahl_util::SharedString;

/// A path to the root of an AST expression tree, for use in a zipper.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Zipper {
    /// The currently focused expression, or `None` if there was a panic while modifying the
    /// function.
    expr: Option<Expr>,

    /// The path upwards.
    pub path: Vec<ZipperPathNode>,
}

impl Zipper {
    /// Creates a zipper around the given expression.
    pub fn new(expr: Expr) -> Zipper {
        Zipper {
            expr: Some(expr),
            path: Vec::new(),
        }
    }

    /// Returns the expression currently under focus, taking it out.
    fn take_expr(&mut self) -> Expr {
        self.expr.take().unwrap()
    }

    /// Returns a reference to the expression currently under focus.
    pub fn expr(&self) -> &Expr {
        self.expr.as_ref().unwrap()
    }

    /// Returns a mutable reference to the expression currently under focus.
    pub fn expr_mut(&mut self) -> &mut Expr {
        self.expr.as_mut().unwrap()
    }

    /// Returns whether the zipper is at the top of the expression tree.
    pub fn at_top(&self) -> bool {
        self.path.is_empty()
    }

    /// Fully zips up the zipper, returning the expression.
    pub fn into_expr(mut self) -> Expr {
        while self.at_top() {
            self.go_up();
        }
        self.take_expr()
    }

    /// Goes up one level, panicking if we're already at the top.
    pub fn go_up(&mut self) {
        let path_head = self
            .path
            .pop()
            .expect("Cannot go up from top of expression");
        let expr = self.take_expr();
        self.expr = Some(path_head.rebuild(Box::new(expr)));
    }

    /// Descends to the argument of the currently selected call expression. Panics if there aren't
    /// enough arguments, or if the expression isn't a call.
    pub fn go_to_call_arg(&mut self, argn: usize) {
        let expr = self.take_expr();
        if let Expr::Call(loc, func, args) = expr {
            let (expr, args) = SplitVec::new(args, argn);
            self.path.push(ZipperPathNode::CallArgs(loc, func, args));
            self.expr = Some(*expr);
        } else {
            panic!("go_to_call_arg on non-call expression {}", expr)
        }
    }

    /// Descends to the function of the currently selected call expression. Panics if the
    /// expression isn't a call.
    pub fn go_to_call_func(&mut self) {
        let expr = self.take_expr();
        if let Expr::Call(loc, func, args) = expr {
            self.path.push(ZipperPathNode::CallFunc(loc, args));
            self.expr = Some(*func);
        } else {
            panic!("go_to_call_func on non-call expression {}", expr)
        }
    }

    /// Descends to the expression of the currently selected lambda expression's body. Panics if the
    /// expression isn't a lambda.
    pub fn go_to_lam_expr(&mut self, exprn: usize) {
        let expr = self.take_expr();
        if let Expr::Lam(loc, args, body) = expr {
            let ((name, ty, expr, effs), body) = SplitVec::new(body, exprn);
            self.path
                .push(ZipperPathNode::LamExpr(loc, args, name, ty, effs, body));
            self.expr = Some(*expr);
        } else {
            panic!("go_to_lam_expr on non-lambda expression {}", expr)
        }
    }

    /// Descends to the type of the currently selected lambda expression's body. Panics if the
    /// expression isn't a lambda.
    pub fn go_to_lam_ty(&mut self, exprn: usize) {
        let expr = self.take_expr();
        if let Expr::Lam(loc, args, body) = expr {
            let ((name, ty, expr, effs), body) = SplitVec::new(body, exprn);
            self.path
                .push(ZipperPathNode::LamTy(loc, args, name, expr, effs, body));
            self.expr = Some(*ty);
        } else {
            panic!("go_to_lam_ty on non-lambda expression {}", expr)
        }
    }

    /// Descends to the argument of the currently selected pi expression. Panics if the expression
    /// isn't a pi.
    pub fn go_to_pi_arg(&mut self, argn: usize) {
        let expr = self.take_expr();
        if let Expr::Pi(loc, args, ret, effs) = expr {
            let ((name, arg), args) = SplitVec::new(args, argn);
            self.path
                .push(ZipperPathNode::PiArg(loc, name, args, ret, effs));
            self.expr = Some(*arg);
        } else {
            panic!("go_to_pi_arg on non-pi expression {}", expr)
        }
    }

    /// Descends to the return of the currently selected pi expression. Panics if the expression
    /// isn't a pi.
    pub fn go_to_pi_return(&mut self) {
        let expr = self.take_expr();
        if let Expr::Pi(loc, args, ret, effs) = expr {
            self.path.push(ZipperPathNode::PiRet(loc, args, effs));
            self.expr = Some(*ret);
        } else {
            panic!("go_to_pi_return on non-pi expression {}", expr)
        }
    }
}

/// A node in the path to the root of an AST expression tree, for use in a zipper.
#[derive(Derivative)]
#[derivative(Debug)]
pub enum ZipperPathNode {
    /// The argument position of a call expression.
    CallArgs(
        #[derivative(Debug = "ignore")] Location,
        Box<Expr>,
        SplitVec<Box<Expr>>,
    ),

    /// The function position of a call expression.
    CallFunc(#[derivative(Debug = "ignore")] Location, Vec<Box<Expr>>),

    /// An expression in the body of a lambda.
    LamExpr(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Option<SharedString>,
        Box<Expr>,
        Effects,
        SplitVec<(Option<SharedString>, Box<Expr>, Box<Expr>, Effects)>,
    ),

    /// The type of an expression in the body of a lambda.
    LamTy(
        #[derivative(Debug = "ignore")] Location,
        Vec<SharedString>,
        Option<SharedString>,
        Box<Expr>,
        Effects,
        SplitVec<(Option<SharedString>, Box<Expr>, Box<Expr>, Effects)>,
    ),

    /// An argument to a pi type.
    PiArg(
        #[derivative(Debug = "ignore")] Location,
        SharedString,
        SplitVec<(SharedString, Box<Expr>)>,
        Box<Expr>,
        Effects,
    ),

    /// The return type of a pi type.
    PiRet(
        #[derivative(Debug = "ignore")] Location,
        Vec<(SharedString, Box<Expr>)>,
        Effects,
    ),
}

impl ZipperPathNode {
    /// Inserts the given expression into the correct position.
    fn rebuild(self, expr: Box<Expr>) -> Expr {
        match self {
            ZipperPathNode::CallArgs(loc, func, args) => Expr::Call(loc, func, args.reunify(expr)),
            ZipperPathNode::CallFunc(loc, args) => Expr::Call(loc, expr, args),
            ZipperPathNode::LamExpr(loc, args, name, ty, effs, body) => {
                Expr::Lam(loc, args, body.reunify((name, ty, expr, effs)))
            }
            ZipperPathNode::LamTy(loc, args, name, body_expr, effs, body) => {
                Expr::Lam(loc, args, body.reunify((name, expr, body_expr, effs)))
            }
            ZipperPathNode::PiArg(loc, name, args, body, effs) => {
                Expr::Pi(loc, args.reunify((name, expr)), body, effs)
            }
            ZipperPathNode::PiRet(loc, args, effs) => Expr::Pi(loc, args, expr, effs),
        }
    }
}
