//! The context of the compiler and interpreter.
//!
//! This also contains elaboration logic.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod elab;

use crate::elab::{reify, unify_ty_expr};
pub use crate::elab::{UnifExpr, Zipper};
use stahl_ast::{Decl, Expr, FQName};
use stahl_cst::{Decl as CstDecl, Expr as CstExpr};
use stahl_errors::{Location, Result};
use stahl_modules::{Library, Module};
use stahl_util::{genint, SharedString, Taker};
use std::{
    collections::{HashMap, HashSet},
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
    ) -> LibContext {
        let lib_name = SharedString::from(format!("{}-{}-{}-{}", name, major, minor, patch));
        LibContext {
            context: self.into(),
            key: (name, major, minor, patch).into(),
            library: Library {
                name: lib_name,
                mods: HashMap::new(),
            }
            .into(),
        }
    }

    /// Inserts the given library, creating an entry for it in the context.
    pub fn insert_lib(
        &mut self,
        name: SharedString,
        major: u16,
        minor: u16,
        patch: u32,
        library: Library,
    ) -> Result<()> {
        let key = (name, major, minor, patch);
        if self.libs.contains_key(&key) {
            raise!(
                "Library {}-{}-{}-{} already exists",
                key.0,
                key.1,
                key.2,
                key.3
            )
        }

        // TODO: Check imports, exports, etc.
        self.libs.insert(key, library);
        Ok(())
    }

    /// Returns the type of the given fully qualified name.
    pub fn get_type_of(&self, name: FQName) -> Result<Expr> {
        unimplemented!()
    }
}

/// A single library's portion of the context.
#[derive(Debug)]
pub struct LibContext<'c> {
    context: Taker<&'c mut Context>,
    key: Taker<(SharedString, u16, u16, u32)>,
    library: Taker<Library>,
}

impl<'c> LibContext<'c> {
    /// Adds this library to the context.
    pub fn finish(mut self) -> Result<&'c mut Context> {
        let (name, major, minor, patch) = self.key.take();
        self.context
            .insert_lib(name, major, minor, patch, self.library.take())?;
        Ok(self.context.take())
    }

    /// Discards this library. Use this instead of just dropping in the case where the library
    /// should not be added!
    pub fn discard(mut self) {
        self.library.take();
    }

    /// Creates a context for the module with the given name.
    pub fn create_mod<'l>(
        &'l mut self,
        name: SharedString,
        exports: HashSet<SharedString>,
        imports: HashMap<SharedString, HashMap<SharedString, HashSet<SharedString>>>,
    ) -> ModContext<'l, 'c> {
        let lib_name = self.library.name.clone();
        ModContext {
            library: self.into(),
            module: Module {
                lib_name,
                mod_name: name,
                exports,
                imports,
                decls: vec![],
            }
            .into(),
        }
    }

    /// Inserts the given module, creating an entry for it in the context.
    pub fn insert_mod(&mut self, module: Module) -> Result<()> {
        if self.library.mods.contains_key(&module.mod_name) {
            raise!(
                "Module {}/{} already exists",
                self.library.name,
                module.mod_name
            )
        }

        // TODO: Check imports, exports, etc.
        self.library.mods.insert(module.mod_name.clone(), module);
        Ok(())
    }
}

impl Drop for LibContext<'_> {
    fn drop(&mut self) {
        if !(panicking() || self.context.taken() || self.key.taken() || self.library.taken()) {
            panic!("Dropping a LibContext that has not called .finish() or .discard()");
        }
    }
}

/// A single module's portion of the context.
#[derive(Debug)]
pub struct ModContext<'l, 'c> {
    library: Taker<&'l mut LibContext<'c>>,
    module: Taker<Module>,
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
        if self.module.decls.iter().any(|decl| decl.name() == name) {
            raise!(
                @decl.loc(),
                "Module {}/{} already declares {}",
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
        match decl {
            CstDecl::Def(loc, name, ty, expr) => self.add_def(loc, name, ty, expr),
            CstDecl::DefEff(loc, name, arg, ret) => self.add_defeff(loc, name, arg, ret),
        }
    }

    /// Adds a value definition to the module context.
    pub fn add_def(
        &mut self,
        loc: Location,
        _name: SharedString,
        ty: Arc<CstExpr>,
        expr: Arc<CstExpr>,
    ) -> Result<()> {
        let (_expr, _ty) = self.elab(&expr, &ty)?;
        todo!(@loc)
    }

    /// Adds an effect definition to the module context.
    pub fn add_defeff(
        &mut self,
        loc: Location,
        name: SharedString,
        arg: Arc<CstExpr>,
        _ret: Option<Arc<CstExpr>>,
    ) -> Result<()> {
        todo!(@loc, "{} {}", name, arg)
    }

    /// Begins creating a def with a known (wildcard-free) type.
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

    /// Resolves the name of a definition the module context, returning its type.
    pub fn resolve(&self, name: SharedString) -> Result<(FQName, Rc<UnifExpr>)> {
        match self.module.resolve(name.clone()) {
            Some(name) => self
                .library
                .context
                .get_type_of(name.clone())
                .map(|ty| (name, Rc::new((&ty).into()))),
            None => raise!("No definition for {} exists", name),
        }
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

/// A single def's portion of the context.
#[derive(Debug)]
pub struct DefContext<'m, 'l, 'c> {
    module: Taker<&'m mut ModContext<'l, 'c>>,
    loc: Taker<Location>,
    name: Taker<SharedString>,
    type_zipper: Taker<Zipper>,
    expr_zipper: Taker<Zipper>,
}

impl<'m, 'l: 'm, 'c: 'l> DefContext<'m, 'l, 'c> {
    /// Adds this def to the module.
    pub fn finish(mut self) -> Result<&'m mut ModContext<'l, 'c>> {
        let loc = self.loc.take();
        let name = self.name.take();

        let mut ty = Rc::new(self.type_zipper.take().into_expr());
        let mut expr = Rc::new(self.expr_zipper.take().into_expr());
        unify_ty_expr(&mut ty, &mut expr)?;

        let expr = reify(&expr)?;
        let ty = reify(&ty)?;
        self.module.add(Decl::Def(loc, name, ty, expr))?;
        Ok(self.module.take())
    }

    /// Discards this def. Use this instead of just dropping in the case where the def should not
    /// be added!
    pub fn discard(mut self) {
        self.loc.take();
        self.name.take();
        self.type_zipper.take();
        self.expr_zipper.take();
    }

    /// Gets the zipper for the type, immutably.
    pub fn expr_zipper(&mut self) -> &mut Zipper {
        &mut *self.expr_zipper
    }

    /// Gets the zipper for the type, immutably.
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
