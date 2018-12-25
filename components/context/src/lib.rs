//! The context of the compiler and interpreter.
//!
//! This is more or less just the data structures, without much logic.

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod split_vec;
mod zipper;

use owning_ref::OwningRefMut;
use stahl_ast::{Decl, Expr};
use stahl_cst::{Decl as CstDecl, Effect as CstEffect, Expr as CstExpr};
use stahl_errors::{Location, Result};
use stahl_modules::{Library, Module};
use stahl_util::SharedString;
use std::{collections::HashMap, ops::Deref, sync::Arc};

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
                name.as_ref(),
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
                self.key.0.as_ref(),
                self.key.1,
                self.key.2,
                self.key.3,
                name.as_ref()
            )
        }
        self.library.mods.insert(name.clone(), module);
        let module =
            OwningRefMut::new(&mut *self.library).map_mut(|lib| lib.mods.get_mut(&name).unwrap());
        Ok(ModContext {
            lib: self.key.clone(),
            name,
            module,
        })
    }
}

/// A single module's portion of the context.
#[derive(Debug)]
pub struct ModContext<'a> {
    lib: (SharedString, u16, u16, u32),
    name: SharedString,
    module: OwningRefMut<&'a mut Library, Module>,
}

impl ModContext<'_> {
    /// Adds a new declaration.
    pub fn add(&mut self, decl: CstDecl) -> Result<()> {
        let name = decl.name();
        if self.module.decls.iter().any(|decl| decl.name() == name) {
            raise!(
                "Module {}-{}-{}-{}/{} already declares {}",
                self.lib.0.as_ref(),
                self.lib.1,
                self.lib.2,
                self.lib.3,
                self.name.as_ref(),
                name.as_ref()
            )
        }

        match decl {
            CstDecl::Def(loc, name, ty, expr) => self.add_def(loc, name, ty, expr),
            CstDecl::DefEff(loc, CstEffect(name, arg, ret)) => self.add_defeff(loc, name, arg, ret),
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
        todo!("{} {} {}", name.as_ref(), ty, expr)
    }

    /// Adds an effect definition to the module context.
    pub fn add_defeff(
        &mut self,
        loc: Location,
        name: SharedString,
        arg: Arc<CstExpr>,
        ret: Option<Arc<CstExpr>>,
    ) -> Result<()> {
        todo!("{} {}", name.as_ref(), arg)
    }

    /// Resolves a name in the module context.
    pub fn resolve(&self, name: SharedString) -> Result<&CstDecl> {
        todo!()
    }
}

impl Deref for ModContext<'_> {
    type Target = Module;
    fn deref(&self) -> &Module {
        self.module.deref()
    }
}
