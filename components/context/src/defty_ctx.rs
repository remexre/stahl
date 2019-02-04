use crate::{
    elab::reify,
    inductive::{ctor_type, elim_type, elim_value},
    mod_ctx::ModContext,
    types::UnifExpr,
    zipper::Zipper,
};
use stahl_ast::{Decl, Effects, Expr, FQName, Intrinsic};
use stahl_errors::{Location, Result};
use stahl_util::{genint, SharedString, Taker};
use std::{rc::Rc, sync::Arc, thread::panicking};

/// A single `defty`'s portion of the context, while defining the kind of the type.
#[derive(Debug)]
pub struct DefTyKindContext<'m, 'l, 'c> {
    pub(crate) module: Taker<&'m mut ModContext<'l, 'c>>,
    pub(crate) loc: Taker<Location>,
    pub(crate) name: Taker<SharedString>,
    pub(crate) kind_zipper: Taker<Zipper>,
}

impl<'m, 'l: 'm, 'c: 'l> DefTyKindContext<'m, 'l, 'c> {
    /// Marks the kind of the type complete, allowing constructors to be added to the `defty`.
    pub fn finish(mut self) -> Result<DefTyCtorsContext<'m, 'l, 'c>> {
        let mut ty = UnifExpr::hole(self.kind_zipper.expr().loc());
        let mut kind = Rc::new(self.kind_zipper.take().into_expr());
        self.module
            .unify_ty_expr(&mut ty, &mut kind, &mut Vec::new())?;
        let kind = reify(&kind)?;
        let ty_args = match &*kind {
            Expr::Pi(_, args, ret, effs) if !args.is_empty() => {
                if !effs.0.is_empty() {
                    raise!(@kind.loc(), "Invalid kind for {}: {}", *self.name, kind);
                }

                match **ret {
                    Expr::Intrinsic(_, Intrinsic::Type) => {}
                    _ => raise!(@kind.loc(), "Invalid kind for {}: {}", *self.name, kind),
                }

                args.iter()
                    .cloned()
                    .map(|(name, ty)| {
                        let name = if name.is_anon() { None } else { Some(name) };
                        (name, ty)
                    })
                    .collect()
            }
            Expr::Intrinsic(_, Intrinsic::Type) => vec![],
            _ => raise!(@kind.loc(), "Invalid kind for {}: {}", *self.name, kind),
        };

        Ok(DefTyCtorsContext {
            module: self.module.take().into(),
            loc: self.loc.take().into(),
            name: self.name.take().into(),
            ty_args: ty_args.into(),
            ctors: Vec::new().into(),
        })
    }

    /// Discards this `defty`. Use this instead of just dropping in the case where the `defty`
    /// should not be added!
    pub fn discard(mut self) {
        self.module.take();
        self.loc.take();
        self.name.take();
        self.kind_zipper.take();
    }

    /// Gets the zipper for the kind of the type.
    pub fn kind_zipper(&mut self) -> &mut Zipper {
        &mut *self.kind_zipper
    }
}

impl Drop for DefTyKindContext<'_, '_, '_> {
    fn drop(&mut self) {
        if !(panicking()
            || self.module.taken()
            || self.loc.taken()
            || self.name.taken()
            || self.kind_zipper.taken())
        {
            panic!("Dropping a DefTyCtorsContext that has not called .finish() or .discard()");
        }
    }
}

/// A single `defty`'s portion of the context, while defining the constructors of the type.
#[derive(Debug)]
pub struct DefTyCtorsContext<'m, 'l, 'c> {
    pub(crate) module: Taker<&'m mut ModContext<'l, 'c>>,
    loc: Taker<Location>,
    name: Taker<SharedString>,
    pub(crate) ty_args: Taker<Vec<(Option<SharedString>, Arc<Expr>)>>,
    ctors: Taker<
        Vec<(
            Location,
            SharedString,
            Vec<(SharedString, Arc<Expr>)>,
            Vec<Arc<Expr>>,
        )>,
    >,
}

impl<'m, 'l: 'm, 'c: 'l> DefTyCtorsContext<'m, 'l, 'c> {
    /// Adds this `defty` to the module.
    pub fn finish(mut self) -> Result<&'m mut ModContext<'l, 'c>> {
        let nonindex_args = self.nonindex_args();
        let kind = self.kind();
        let loc = self.loc.take();
        let name = self.name.take();
        let ty_args = self.ty_args.take();
        let mut ctors = self.ctors.take();

        let lib_name = self.module.lib_name.clone();
        let mod_name = self.module.mod_name.clone();

        let ty_name = FQName(lib_name.clone(), mod_name.clone(), name.clone());
        let ty = Arc::new(Expr::Atom(loc.clone(), ty_name.clone(), kind.clone()));
        self.module
            .add(Decl::Def(loc.clone(), name.clone(), kind, ty))?;

        for ctor in &mut ctors {
            let ctor_args = &mut ctor.2;
            ctor_args.splice(..0, nonindex_args.clone());

            let ty = ctor_type(ty_name.clone(), ctor);
            let ctor_name = FQName(lib_name.clone(), mod_name.clone(), ctor.1.clone());
            let atom = Arc::new(Expr::Atom(loc.clone(), ctor_name, ty.clone()));
            self.module
                .add(Decl::Def(loc.clone(), ctor.1.clone(), ty, atom))?;
        }

        self.module.add(Decl::Def(
            loc.clone(),
            format!("elim-{}", name).into(),
            elim_type(
                lib_name.clone(),
                mod_name.clone(),
                loc.clone(),
                name.clone(),
                ty_args.clone(),
                ctors.clone(),
            ),
            elim_value(
                lib_name,
                mod_name,
                loc,
                name,
                ty_args.clone(),
                ctors.clone(),
            ),
        ))?;

        Ok(self.module.take())
    }

    /// Discards this `defty`. Use this instead of just dropping in the case where the `defty`
    /// should not be added!
    pub fn discard(mut self) {
        self.module.take();
        self.loc.take();
        self.name.take();
        self.ty_args.take();
        self.ctors.take();
    }

    /// Returns the kind of the type being defined.
    fn kind(&self) -> Arc<Expr> {
        let ty = Arc::new(Expr::Intrinsic(self.loc.clone(), Intrinsic::Type));
        if self.ty_args.is_empty() {
            ty
        } else {
            let args = self
                .ty_args
                .iter()
                .cloned()
                .map(|(name, arg)| {
                    let name = name.unwrap_or_else(SharedString::gensym_anon);
                    (name, arg)
                })
                .collect();
            Arc::new(Expr::Pi(self.loc.clone(), args, ty, Effects::default()))
        }
    }

    /// Returns the fully-qualified name of the type being defined.
    fn name(&self) -> FQName {
        FQName(
            self.module.lib_name.clone(),
            self.module.mod_name.clone(),
            self.name.clone(),
        )
    }

    /// Returns the arguments that are *not* index arguments.
    fn nonindex_args(&self) -> Vec<(SharedString, Arc<Expr>)> {
        self.ty_args
            .iter()
            .cloned()
            .filter_map(|(n, e)| n.map(|n| (n, e)))
            .collect::<Vec<_>>()
    }

    /// Returns the UnifExpr corresponding to the type being defined.
    fn ty(&self) -> Rc<UnifExpr> {
        Rc::new(UnifExpr::Atom(
            self.loc.clone(),
            self.name(),
            Rc::new(self.kind().into()),
        ))
    }

    /// Begins creating a constructor.
    pub fn create_ctor<'d>(
        &'d mut self,
        name: SharedString,
        loc: Location,
    ) -> CtorContext<'d, 'm, 'l, 'c> {
        let zipper = Zipper::new(Rc::new(UnifExpr::UnifVar(self.loc.clone(), genint())));
        CtorContext {
            defty: self.into(),
            loc: loc.into(),
            name: name.into(),
            zipper: zipper.into(),
        }
    }

    /// Runs the given closure with a `DefContext`, cleaning up afterwards.
    pub fn with_ctor<'d, F>(
        &'d mut self,
        name: SharedString,
        loc: Location,
        f: F,
    ) -> Result<&'d mut DefTyCtorsContext<'m, 'l, 'c>>
    where
        F: FnOnce(&mut CtorContext<'d, 'm, 'l, 'c>) -> Result<()>,
    {
        let mut ctor_ctx = self.create_ctor(name, loc);
        match f(&mut ctor_ctx) {
            Ok(()) => ctor_ctx.finish(),
            Err(err) => {
                ctor_ctx.discard();
                Err(err)
            }
        }
    }
}

impl Drop for DefTyCtorsContext<'_, '_, '_> {
    fn drop(&mut self) {
        if !(panicking()
            || self.module.taken()
            || self.loc.taken()
            || self.name.taken()
            || self.ty_args.taken()
            || self.ctors.taken())
        {
            panic!("Dropping a DefTyCtorsContext that has not called .finish() or .discard()");
        }
    }
}

/// A single constructor in a `defty`'s portion of the context.
#[derive(Debug)]
pub struct CtorContext<'d, 'm, 'l, 'c> {
    defty: Taker<&'d mut DefTyCtorsContext<'m, 'l, 'c>>,
    loc: Taker<Location>,
    name: Taker<SharedString>,
    zipper: Taker<Zipper>,
}

impl<'d, 'm: 'd, 'l: 'm, 'c: 'l> CtorContext<'d, 'm, 'l, 'c> {
    /// Adds this constructor to the `defty`.
    pub fn finish(mut self) -> Result<&'d mut DefTyCtorsContext<'m, 'l, 'c>> {
        let loc = self.loc.take();
        let name = self.name.take();
        let ty_name = self.defty.name();

        let mut ctor_kind = Rc::new(UnifExpr::Intrinsic(
            self.zipper.expr().loc(),
            Intrinsic::Type,
        ));
        let mut ctor_type = Rc::new(self.zipper.take().into_expr());
        let mut env = vec![(
            self.defty.name.clone(),
            Rc::new(self.defty.kind().into()),
            Some(self.defty.ty()),
        )];
        env.extend(
            self.defty
                .nonindex_args()
                .into_iter()
                .map(|(name, ty)| (name, Rc::new(ty.into()), None)),
        );
        self.defty
            .module
            .unify_ty_expr(&mut ctor_kind, &mut ctor_type, &mut env)?;
        let ctor_type = reify(&ctor_type)?;
        let (ctor_args, ret) = match &*ctor_type {
            Expr::Pi(_, args, ret, effs) if !args.is_empty() => {
                if !effs.0.is_empty() {
                    raise!(@ctor_type.loc(), "Invalid type for constructor {}: {}", name, ctor_type);
                }

                let ctor_args = args.clone();
                warn!("TODO: Positivity check");

                (ctor_args, &**ret)
            }
            Expr::Call(_, _, _) | Expr::Atom(_, _, _) => (Vec::new(), &*ctor_type),
            _ => raise!(@ctor_type.loc(), "Invalid type for constructor {}: {}", name, ctor_type),
        };
        let ty_args = match ret {
            Expr::Call(_, func, args) if !args.is_empty() => match **func {
                Expr::Atom(_, ref name, _) if name == &ty_name => {
                    warn!("TODO: Check that index arguments are free");
                    args.clone()
                }
                _ => {
                    raise!(@ctor_type.loc(), "Invalid type for constructor {}: {}", name, ctor_type)
                }
            },
            Expr::Atom(_, name, _) if name == &ty_name => Vec::new(),
            _ => raise!(@ctor_type.loc(), "Invalid type for constructor {}: {}", name, ctor_type),
        };

        self.defty.ctors.push((loc, name, ctor_args, ty_args));
        Ok(self.defty.take())
    }

    /// Discards this constructor. Use this instead of just dropping in the case where the
    /// constructor should not be added!
    pub fn discard(mut self) {
        self.defty.take();
        self.name.take();
        self.zipper.take();
    }

    /// Gets the zipper for the type of the constructor.
    pub fn zipper(&mut self) -> &mut Zipper {
        &mut *self.zipper
    }
}

impl Drop for CtorContext<'_, '_, '_, '_> {
    fn drop(&mut self) {
        if !(panicking() || self.defty.taken() || self.name.taken() || self.zipper.taken()) {
            panic!("Dropping a CtorContext that has not called .finish() or .discard()");
        }
    }
}
