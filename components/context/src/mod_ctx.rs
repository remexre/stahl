use crate::{
    def_ctx::DefContext,
    defty_ctx::{DefTyCtorsContext, DefTyKindContext},
    lib_ctx::LibContext,
    types::UnifExpr,
    zipper::Zipper,
};
use log::info;
use stahl_ast::{Decl, FQName, Intrinsic};
use stahl_cst::Decl as CstDecl;
use stahl_errors::{Location, Result};
use stahl_modules::Module;
use stahl_util::{genint, SharedString, Taker};
use std::{
    ops::{Deref, DerefMut},
    rc::Rc,
    thread::panicking,
};

/// A single module's portion of the context.
#[derive(Debug)]
pub struct ModContext<'l, 'c> {
    pub(crate) library: Taker<&'l mut LibContext<'c>>,
    pub(crate) module: Taker<Module>,
}

impl<'l, 'c: 'l> ModContext<'l, 'c> {
    /// Adds this module to the library.
    pub fn finish(mut self) -> Result<&'l mut LibContext<'c>> {
        self.library.insert_mod(self.module.take())?;
        Ok(self.library.take())
    }

    /// Discards this module. Use this instead of just dropping in the case where the module should
    /// not be added!
    pub fn discard(mut self) {
        self.module.take();
    }

    /// Adds a new declaration.
    pub fn add(&mut self, decl: Decl) -> Result<()> {
        let name = decl.name();
        if self.module.decls.iter().any(|d| d.name() == name) {
            raise!(
                @decl.loc(),
                "Module {}:{} already declares {}",
                self.library.library.name,
                self.module.mod_name,
                name
            )
        }

        self.module.decls.push(decl);
        Ok(())
    }

    /// Adds a new CST declaration.
    pub fn add_cst_decl(&mut self, decl: CstDecl) -> Result<()> {
        info!("Adding {}", decl);
        match decl {
            CstDecl::Def(loc, name, ty, expr) => {
                let (expr, ty) = self.elab(&expr, &ty)?;
                self.add(Decl::Def(loc, name, ty, expr))
            }
            CstDecl::DefEff(loc, _name, _arg, _ret) => todo!(@loc),
            CstDecl::DefEffSet(loc, name, effs) => {
                let effs = effs
                    .into_iter()
                    .map(|eff| match self.resolve(eff) {
                        Ok((name, Decl::DefEff(_, _, _, _))) => Ok(name),
                        Ok((_name, Decl::DefEffSet(_, _, _))) => todo!(@loc.clone()),
                        Ok((name, Decl::Def(_, _, _, _))) => {
                            raise!(@loc.clone(), "{} is a value, not an effect", name)
                        }
                        Err(err) => Err(err),
                    })
                    .collect::<Result<_>>()?;
                self.add(Decl::DefEffSet(loc, name, effs))
            }
            CstDecl::DefTy(loc, ty_name, kind, ctors) => {
                let mut locals = Vec::new();
                let mut kind = self.cst_to_unif(&kind, &mut locals)?;
                self.normalize(&mut kind, None);
                let is_simple = if let UnifExpr::Intrinsic(_, Intrinsic::Type) = *kind {
                    true
                } else {
                    false
                };
                let ty_name_var = Rc::new(UnifExpr::LocalVar(loc.clone(), ty_name.clone()));

                self.with_defty(
                    loc,
                    ty_name.clone(),
                    |defty_kind_ctx| {
                        defty_kind_ctx.kind_zipper().fill(kind);
                        Ok(())
                    },
                    |defty_ctors_ctx| {
                        locals.push(ty_name);
                        for (loc, name, ty) in ctors {
                            let ctor_ty = match ty {
                                Some(ty) => defty_ctors_ctx.module.cst_to_unif(&ty, &mut locals)?,
                                None => {
                                    if is_simple {
                                        ty_name_var.clone()
                                    } else {
                                        raise!(@loc, concat!("Cannot use simple constructor ",
                                                             "syntax for a parameterized type"))
                                    }
                                }
                            };
                            defty_ctors_ctx.with_ctor(name, loc, |ctor_ctx| {
                                ctor_ctx.zipper().fill(ctor_ty);
                                Ok(())
                            })?;
                        }
                        Ok(())
                    },
                )?;
                Ok(())
            }
        }
    }

    /// Begins creating a `def`.
    pub fn create_def<'m>(
        &'m mut self,
        loc: Location,
        name: SharedString,
    ) -> DefContext<'m, 'l, 'c> {
        let type_zipper = Zipper::new(Rc::new(UnifExpr::UnifVar(loc.clone(), genint())));
        let expr_zipper = Zipper::new(Rc::new(UnifExpr::UnifVar(loc.clone(), genint())));
        DefContext {
            module: self.into(),
            loc: loc.into(),
            name: name.into(),
            type_zipper: type_zipper.into(),
            expr_zipper: expr_zipper.into(),
        }
    }

    /// Begins creating a `defty`.
    pub fn create_defty<'m>(
        &'m mut self,
        loc: Location,
        name: SharedString,
    ) -> DefTyKindContext<'m, 'l, 'c> {
        let kind_zipper = Zipper::new(Rc::new(UnifExpr::UnifVar(loc.clone(), genint())));
        DefTyKindContext {
            module: self.into(),
            loc: loc.into(),
            name: name.into(),
            kind_zipper: kind_zipper.into(),
        }
    }

    /// Runs the given closure with a `DefContext`, cleaning up afterwards.
    pub fn with_def<'m, F>(
        &'m mut self,
        loc: Location,
        name: SharedString,
        f: F,
    ) -> Result<&'m mut ModContext<'l, 'c>>
    where
        F: FnOnce(&mut DefContext<'m, 'l, 'c>) -> Result<()>,
    {
        let mut def_ctx = self.create_def(loc, name);
        match f(&mut def_ctx) {
            Ok(()) => def_ctx.finish(),
            Err(err) => {
                def_ctx.discard();
                Err(err)
            }
        }
    }

    /// Runs the given closure with a `DefTyKindContext`, then another with the corresponding
    /// `DefTyCtorsContext`, cleaning up afterwards.
    pub fn with_defty<'m, F1, F2>(
        &'m mut self,
        loc: Location,
        name: SharedString,
        f1: F1,
        f2: F2,
    ) -> Result<&'m mut ModContext<'l, 'c>>
    where
        F1: FnOnce(&mut DefTyKindContext<'m, 'l, 'c>) -> Result<()>,
        F2: FnOnce(&mut DefTyCtorsContext<'m, 'l, 'c>) -> Result<()>,
    {
        let mut defty_kind_ctx = self.create_defty(loc, name);
        match f1(&mut defty_kind_ctx) {
            Ok(()) => {
                let mut defty_ctors_ctx = defty_kind_ctx.finish()?;
                match f2(&mut defty_ctors_ctx) {
                    Ok(()) => defty_ctors_ctx.finish(),
                    Err(err) => {
                        defty_ctors_ctx.discard();
                        Err(err)
                    }
                }
            }
            Err(err) => {
                defty_kind_ctx.discard();
                Err(err)
            }
        }
    }

    /// Returns the decl referred to by the given global name.
    pub fn get_decl(&self, name: FQName) -> Option<(FQName, &Decl)> {
        if self.module.lib_name == name.0 && self.mod_name == name.1 {
            for decl in &self.module.decls {
                if decl.name() == &name.2 {
                    return Some((name, decl));
                }
            }
            None
        } else {
            self.library.get_decl(name)
        }
    }

    /// Returns an iterator over names that are locally visible.
    pub fn local_name_iter<'m>(&'m self) -> impl 'm + Iterator<Item = SharedString> {
        self.imports
            .values()
            .flat_map(|l| l.values())
            .flat_map(|m| m.iter())
            .cloned()
            .chain(self.decls.iter().map(|d| d.name()))
    }

    /// Resolves the name of a definition in the module context, returning its declaration.
    pub fn resolve(&self, name: SharedString) -> Result<(FQName, &Decl)> {
        if !name.contains(':') {
            match self.module.resolve(name.clone()) {
                Some(name) => {
                    if name.0 == self.module.lib_name {
                        if name.1 == self.module.mod_name {
                            match self.decls.iter().find(|decl| decl.name() == &name.2) {
                                Some(decl) => Ok((name, decl)),
                                None => panic!("resolve() lied about {} being local", name),
                            }
                        } else {
                            if let Some(m) = self.library.library.mods.get(&name.1) {
                                if let Some(decl) =
                                    m.decls.iter().find(|decl| decl.name() == &name.2)
                                {
                                    if true {
                                        Ok((name, decl))
                                    } else {
                                        raise!("{}:{} does not export {}", name.0, name.1, name.2)
                                    }
                                } else {
                                    for (lib_name, lib_imps) in m.imports.iter() {
                                        for (mod_name, mod_imps) in lib_imps.iter() {
                                            if mod_imps.contains(&name.2) {
                                                let name = FQName(
                                                    lib_name.clone(),
                                                    mod_name.clone(),
                                                    name.2.clone(),
                                                );
                                                return self
                                                    .get_decl(name.clone())
                                                    .ok_or_else(|| err!("{} doesn't exist", name));
                                            }
                                        }
                                    }
                                    raise!("{}:{} does not declare {}", name.0, name.1, name.2)
                                }
                            } else {
                                raise!("Cannot find {} in non-existent module", name);
                            }
                        }
                    } else {
                        self.library.context.get_decl(name.clone(), true)
                    }
                }
                None => raise!("No definition for {} exists", name),
            }
        } else {
            let name = name.parse::<FQName>()?;
            self.get_decl(name.clone())
                .ok_or_else(|| err!("No definition for {} exists", name))
        }
    }

    /// Adds an import for the prelude, if it exists. If `strict` is true, an error will be
    /// returned if the library does not import `std`.
    pub fn add_prelude_import(&mut self, strict: bool) -> Result<()> {
        if let Some(std) = self.library.context.std() {
            if self.library.deps.get(&std.0).as_ref() == Some(&&std) {
                let exports = self
                    .library
                    .context
                    .prelude_exports()
                    .ok_or_else(|| err!("Couldn't find prelude module"))?
                    .clone();
                self.imports
                    .entry(std)
                    .or_default()
                    .entry("prelude".into())
                    .or_insert_with(|| exports);
            } else if strict {
                raise!(
                    "{} does not import the standard library ({})",
                    self.library.name,
                    std
                )
            }
        } else if strict {
            raise!(
                "No standard library for {} import prelude from",
                self.name()
            )
        }

        Ok(())
    }
}

impl Deref for ModContext<'_, '_> {
    type Target = Module;
    fn deref(&self) -> &Module {
        &*self.module
    }
}

impl DerefMut for ModContext<'_, '_> {
    fn deref_mut(&mut self) -> &mut Module {
        &mut *self.module
    }
}

impl Drop for ModContext<'_, '_> {
    fn drop(&mut self) {
        if !(panicking() || self.library.taken() || self.module.taken()) {
            panic!("Dropping a ModContext that has not called .finish() or .discard()");
        }
    }
}
