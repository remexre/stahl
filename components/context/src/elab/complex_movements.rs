use crate::elab::{
    zipper::{Zipper, ZipperPathNode},
    UnifExpr,
};

impl Zipper {
    /// Goes "left," returning whether this was successful. If `false` is returned, the position
    /// will be unchanged.
    pub fn go_left(&mut self) -> bool {
        if self.at_top() {
            return false;
        }

        match self.path[self.path.len() - 1] {
            ZipperPathNode::CallArgs(_, _, ref args) => {
                let l = args.left().len();
                self.go_up();
                if l == 0 {
                    self.go_left()
                } else {
                    self.go_to_call_arg(l - 1);
                    true
                }
            }
            ZipperPathNode::CallFunc(_, ref args) => {
                self.go_up();
                self.go_left()
            }
            ZipperPathNode::LamExpr(_, _, _, _, _, ref body) => {
                let n = body.left().len();
                self.go_up();
                self.go_to_lam_ty(n);
                true
            }
            ZipperPathNode::LamTy(_, _, _, _, _, ref body) => unimplemented!(),
            ZipperPathNode::PiArg(_, _, ref args, ref body, _) => {
                let l = args.left().len();
                self.go_up();
                if l == 0 {
                    self.go_left()
                } else {
                    self.go_to_pi_arg(l - 1);
                    true
                }
            }
            ZipperPathNode::PiRet(_, _, _) => unimplemented!(),
        }
    }

    /// Goes "right," returning whether this was successful. If `false` is returned, the position
    /// will be unchanged.
    pub fn go_right(&mut self) -> bool {
        if self.at_top() {
            return false;
        }

        match self.path[self.path.len() - 1] {
            ZipperPathNode::CallArgs(_, _, ref args) => {
                let (l, r) = args.both_lens();
                self.go_up();
                if r == 0 {
                    self.go_right()
                } else {
                    self.go_to_call_arg(l + 1);
                    true
                }
            }
            ZipperPathNode::CallFunc(_, ref args) => {
                let niliadic = args.is_empty();
                self.go_up();
                if niliadic {
                    self.go_right()
                } else {
                    self.go_to_call_arg(0);
                    true
                }
            }
            ZipperPathNode::LamExpr(_, _, _, _, _, ref body) => unimplemented!(),
            ZipperPathNode::LamTy(_, _, _, _, _, ref body) => {
                let n = body.left().len();
                self.go_up();
                self.go_to_lam_expr(n);
                true
            }
            ZipperPathNode::PiArg(_, _, ref args, ref body, _) => {
                let (l, r) = args.both_lens();
                self.go_up();
                if r == 0 {
                    self.go_to_pi_return();
                } else {
                    self.go_to_pi_arg(l + 1);
                }
                true
            }
            ZipperPathNode::PiRet(_, _, _) => {
                self.go_up();
                self.go_right()
            }
        }
    }

    /// Goes down, preferring the leftmost possiblity, returning false if it is not possible.
    pub fn go_down(&mut self) -> bool {
        match self.expr() {
            UnifExpr::Call(_, _, _) => unimplemented!(),
            UnifExpr::Lam(_, _, _) => unimplemented!(),
            UnifExpr::Pi(_, _, _, _) => {
                self.go_to_pi_arg(0);
                true
            }
            UnifExpr::Const(_, _)
            | UnifExpr::GlobalVar(_, _)
            | UnifExpr::LocalVar(_, _)
            | UnifExpr::Type(_)
            | UnifExpr::UnifVar(_, _) => false,
        }
    }

    /// Goes to the leftmost hole, returning whether it was possible. If not, returns `false`, and
    /// is at the rightmost leaf.
    pub fn go_to_leftmost_hole(&mut self) -> bool {
        self.go_to_top();
        loop {
            while !self.at_leaf() {
                self.go_down();
            }

            while !self.at_hole() {
                unimplemented!("{}", self)
            }
            return true;
        }
    }

    /// Goes to the top.
    pub fn go_to_top(&mut self) {
        while !self.at_top() {
            self.go_up();
        }
    }
}
