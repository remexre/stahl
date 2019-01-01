//! The most basic tactic functions that are not plain zipper traversals.

use crate::{
    elab::hole,
    types::{UnifEffs, UnifExpr},
    zipper::Zipper,
};
use stahl_errors::Location;
use stahl_util::SharedString;
use std::rc::Rc;

impl Zipper {
    /// Introduces variables into the context via a lambda expression. This may shadow other
    /// variables.
    pub fn intros(&mut self, loc: Location, vars: Vec<SharedString>) {
        self.modify_expr(|expr| {
            Rc::new(UnifExpr::Lam(
                loc.clone(),
                vars,
                vec![(None, hole(loc), expr, UnifEffs::any())],
            ))
        });
        self.go_to_lam_expr(0);
    }

    /// Introduces variables into the context via a pi expression. This may shadow other
    /// variables. Note that there will additional holes created to the left of the currently
    /// focused expression, for the type of each new argument.
    pub fn intros_pi(&mut self, loc: Location, vars: Vec<SharedString>) {
        self.modify_expr(|expr| {
            let vars = vars.into_iter().map(|v| (v, hole(loc.clone()))).collect();
            Rc::new(UnifExpr::Pi(loc, vars, expr, UnifEffs::any()))
        });
        self.go_to_pi_return();
    }

    /// Replaces the currently focused expression, warning if it is not a hole.
    pub fn fill(&mut self, expr: Rc<UnifExpr>) {
        self.modify_expr(|prev| {
            match *prev {
                UnifExpr::UnifVar(_, _) => {}
                _ => warn!("Filling non-hole {}", prev),
            }

            expr
        })
    }
}
