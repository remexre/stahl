use crate::{elab::reify, mod_ctx::ModContext, zipper::Zipper};
use stahl_ast::Decl;
use stahl_errors::{Location, Result};
use stahl_util::{SharedString, Taker};
use std::{rc::Rc, thread::panicking};

/// A single `def`'s portion of the context.
#[derive(Debug)]
pub struct DefContext<'m, 'l, 'c> {
    pub(crate) module: Taker<&'m mut ModContext<'l, 'c>>,
    pub(crate) loc: Taker<Location>,
    pub(crate) name: Taker<SharedString>,
    pub(crate) type_zipper: Taker<Zipper>,
    pub(crate) expr_zipper: Taker<Zipper>,
}

impl<'m, 'l: 'm, 'c: 'l> DefContext<'m, 'l, 'c> {
    /// Adds this `def` to the module.
    pub fn finish(mut self) -> Result<&'m mut ModContext<'l, 'c>> {
        let loc = self.loc.take();
        let name = self.name.take();

        let mut ty = Rc::new(self.type_zipper.take().into_expr());
        let mut expr = Rc::new(self.expr_zipper.take().into_expr());
        self.module
            .unify_ty_expr(&mut ty, &mut expr, &mut Vec::new())?;

        let expr = reify(&expr)?;
        let ty = reify(&ty)?;
        self.module.add(Decl::Def(loc, name, ty, expr))?;
        Ok(self.module.take())
    }

    /// Discards this def. Use this instead of just dropping in the case where the `def` should not
    /// be added!
    pub fn discard(mut self) {
        self.loc.take();
        self.name.take();
        self.type_zipper.take();
        self.expr_zipper.take();
    }

    /// Gets the zipper for the type.
    pub fn expr_zipper(&mut self) -> &mut Zipper {
        &mut *self.expr_zipper
    }

    /// Gets the zipper for the type.
    pub fn type_zipper(&mut self) -> &mut Zipper {
        &mut *self.type_zipper
    }
}

impl Drop for DefContext<'_, '_, '_> {
    fn drop(&mut self) {
        if !(panicking()
            || self.module.taken()
            || self.loc.taken()
            || self.name.taken()
            || self.type_zipper.taken()
            || self.expr_zipper.taken())
        {
            panic!("Dropping a DefContext that has not called .finish() or .discard()");
        }
    }
}
