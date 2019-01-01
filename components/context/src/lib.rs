//! The context of the compiler and interpreter.
//!
//! This also contains elaboration logic.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod elab;
mod split_vec;

use crate::elab::{reify, unify_ty_expr};
pub use crate::elab::{UnifExpr, Zipper};
use owning_ref::OwningRefMut;
use stahl_ast::Decl;
use stahl_cst::{Decl as CstDecl, Expr as CstExpr};
use stahl_errors::{Location, Result};
use stahl_modules::{Library, Module};
use stahl_util::{genint, SharedString};
use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
    thread::panicking,
};

/// The context in which compilation and interpretation occur.
#[derive(Debug, Default)]
pub struct Context {
    libs: HashMap<(SharedString, u16, u16, u32), Library>,
}

impl Context {
    /// Creates a new `Context`.
    pub fn new() -> Context {
        Context::default()
    }

    /// Creates a context for the library with the given name and version.
    pub fn create_lib(
        &mut self,
        name: SharedString,
        major: u16,
        minor: u16,
        patch: u32,
    ) -> Result<LibContext> {
        let key = (name.clone(), major, minor, patch);
        if self.libs.contains_key(&key) {
            raise!(
                "Library {}-{}-{}-{} already exists",
                name,
                major,
                minor,
                patch
            )
        }
        self.libs.insert(
            key.clone(),
            Library {
                mods: HashMap::new(),
            },
        );
        let library = OwningRefMut::new(self).map_mut(|ctx| ctx.libs.get_mut(&key).unwrap());
        Ok(LibContext { key, library })
    }
}

/// A single library's portion of the context.
#[derive(Debug)]
pub struct LibContext<'a> {
    key: (SharedString, u16, u16, u32),
    library: OwningRefMut<&'a mut Context, Library>,
}

impl LibContext<'_> {
    /// Creates a context for the module with the given name.
    pub fn create_mod(
        &mut self,
        name: SharedString,
        exports: Vec<SharedString>,
        imports: HashMap<SharedString, HashMap<SharedString, Vec<SharedString>>>,
    ) -> Result<ModContext> {
        self.insert_mod(
            name,
            Module {
                exports,
                imports,
                decls: Vec::new(),
            },
        )
    }

    /// Inserts the given module, creating an entry for it in the context.
    pub fn insert_mod(&mut self, name: SharedString, module: Module) -> Result<ModContext> {
        if self.library.mods.contains_key(&name) {
            raise!(
                "Module {}-{}-{}-{}/{} already exists",
                self.key.0,
                self.key.1,
                self.key.2,
                self.key.3,
                name
            )
        }

        // TODO: Process imports, etc.
        self.library.mods.insert(name.clone(), module);
        let module =
            OwningRefMut::new(&mut *self.library).map_mut(|lib| lib.mods.get_mut(&name).unwrap());
        Ok(ModContext {
            lib_name: self.key.clone(),
            mod_name: name,
            module,
        })
    }
}

/// A single module's portion of the context.
#[derive(Debug)]
pub struct ModContext<'a> {
    lib_name: (SharedString, u16, u16, u32),
    mod_name: SharedString,
    module: OwningRefMut<&'a mut Library, Module>,
}

impl<'lib> ModContext<'lib> {
    /// Adds a new declaration.
    pub fn add(&mut self, decl: Decl) -> Result<()> {
        let name = decl.name();
        if self.module.decls.iter().any(|decl| decl.name() == name) {
            raise!(
                @decl.loc(),
                "Module {}-{}-{}-{}/{} already declares {}",
                self.lib_name.0,
                self.lib_name.1,
                self.lib_name.2,
                self.lib_name.3,
                self.mod_name,
                name
            )
        }

        self.module.decls.push(decl);
        Ok(())
    }

    /// Adds a new CST declaration.
    pub fn add_cst_decl(&mut self, decl: CstDecl) -> Result<()> {
        match decl {
            CstDecl::Def(loc, name, ty, expr) => self.add_def(loc, name, ty, expr),
            CstDecl::DefEff(loc, name, arg, ret) => self.add_defeff(loc, name, arg, ret),
        }
    }

    /// Adds a value definition to the module context.
    pub fn add_def(
        &mut self,
        loc: Location,
        name: SharedString,
        ty: Arc<CstExpr>,
        expr: Arc<CstExpr>,
    ) -> Result<()> {
        let (expr, ty) = self.elab(&expr, &ty)?;
        todo!(@loc)
    }

    /// Adds an effect definition to the module context.
    pub fn add_defeff(
        &mut self,
        loc: Location,
        name: SharedString,
        arg: Arc<CstExpr>,
        ret: Option<Arc<CstExpr>>,
    ) -> Result<()> {
        todo!(@loc, "{} {}", name, arg)
    }

    /// Begins creating a def with a known (wildcard-free) type.
    pub fn create_def<'a>(
        &'a mut self,
        loc: Location,
        name: SharedString,
    ) -> Result<DefContext<'a, 'lib>> {
        let type_zipper = Zipper::new(Rc::new(UnifExpr::UnifVar(loc.clone(), genint())));
        let expr_zipper = Zipper::new(Rc::new(UnifExpr::UnifVar(loc.clone(), genint())));
        Ok(DefContext {
            mod_ctx: self,
            loc: Some(loc),
            name: Some(name),
            type_zipper: Some(type_zipper),
            expr_zipper: Some(expr_zipper),
        })
    }

    /// Resolves a name in the module context.
    pub fn resolve(&self, name: &str) -> Result<&CstDecl> {
        todo!("resolve {}", name)
    }
}

impl Deref for ModContext<'_> {
    type Target = Module;
    fn deref(&self) -> &Module {
        &self.module
    }
}

impl DerefMut for ModContext<'_> {
    fn deref_mut(&mut self) -> &mut Module {
        &mut self.module
    }
}

/// A single def's portion of the context.
#[derive(Debug)]
pub struct DefContext<'m, 'l> {
    mod_ctx: &'m mut ModContext<'l>,
    loc: Option<Location>,
    name: Option<SharedString>,
    type_zipper: Option<Zipper>,
    expr_zipper: Option<Zipper>,
}

impl DefContext<'_, '_> {
    /// Gets the zipper for the type, immutably.
    pub fn expr_zipper(&mut self) -> &mut Zipper {
        self.expr_zipper.as_mut().unwrap()
    }

    /// Gets the zipper for the type, immutably.
    pub fn type_zipper(&mut self) -> &mut Zipper {
        self.type_zipper.as_mut().unwrap()
    }

    /// Adds this def to the module.
    pub fn finish(mut self) -> Result<()> {
        let loc = self.loc.take().unwrap();
        let name = self.name.take().unwrap();

        let mut ty = Rc::new(self.type_zipper.take().unwrap().into_expr());
        let mut expr = Rc::new(self.expr_zipper.take().unwrap().into_expr());
        unify_ty_expr(&mut ty, &mut expr)?;

        let expr = Box::new(reify(&expr)?);
        let ty = Box::new(reify(&ty)?);
        self.mod_ctx.add(Decl::Def(loc, name, ty, expr))
    }

    /// Discards this def. Use this instead of just dropping in the case where the def should not
    /// be added!
    pub fn discard(mut self) {
        self.loc.take().unwrap();
        self.name.take().unwrap();
        self.type_zipper.take().unwrap();
        self.expr_zipper.take().unwrap();
    }
}

impl Drop for DefContext<'_, '_> {
    fn drop(&mut self) {
        if !panicking()
            && (self.loc.is_some()
                || self.name.is_some()
                || self.type_zipper.is_some()
                || self.expr_zipper.is_some())
        {
            panic!("Dropping a DefContext that has not called .finish() or .discard()");
        }
    }
}
