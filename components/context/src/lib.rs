//! The context of the compiler and interpreter.
//!
//! This should be your main interface to the compiler.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod builtins;
mod def_ctx;
mod defty_ctx;
mod elab;
mod inductive;
mod lib_ctx;
mod mod_ctx;
mod types;
mod zipper;

pub use crate::{
    def_ctx::DefContext,
    defty_ctx::{DefTyCtorsContext, DefTyKindContext},
    lib_ctx::LibContext,
    mod_ctx::ModContext,
    types::{UnifEffs, UnifExpr},
    zipper::Zipper,
};
use stahl_ast::{Decl, FQName, LibName};
use stahl_errors::{Result, ResultExt};
use stahl_modules::{Library, Module};
use stahl_util::{SharedPath, SharedString};
use std::collections::{HashMap, HashSet};

/// The context in which compilation and interpretation occur.
#[derive(Debug)]
pub struct Context {
    libs: HashMap<LibName, Library>,

    /// The paths searched when looking for libraries.
    pub search_paths: Vec<SharedPath>,

    /// The name of the standard library, if it has been loaded.
    std: Option<LibName>,
}

impl Context {
    /// Creates a new `Context`.
    pub fn new<II>(load_std: bool, search_paths: II) -> Result<Context>
    where
        II: IntoIterator<Item = SharedPath>,
    {
        let mut ctx = Context {
            libs: HashMap::new(),
            search_paths: search_paths.into_iter().collect(),
            std: None,
        };
        builtins::add_to(&mut ctx, load_std)?;
        Ok(ctx)
    }

    /// Creates a context for the library with the given name and version.
    pub fn create_lib(
        &mut self,
        name: LibName,
        deps: HashMap<SharedString, LibName>,
        path: Option<SharedPath>,
    ) -> LibContext {
        LibContext {
            context: self.into(),
            library: Library {
                name,
                deps,
                mods: HashMap::new(),
                path,
            }
            .into(),
        }
    }

    /// Loads the library with the given name from the search path.
    pub fn load_lib(&mut self, name: LibName) -> Result<()> {
        if self.libs.contains_key(&name) {
            return Ok(());
        }

        let lib = Library::load(name.clone(), &self.search_paths)?;
        for dep in lib.deps.values() {
            self.load_lib(dep.clone())
                .chain(|| err!("When loading dependency of {}:", name))?;
        }
        self.finish_loading_and_insert_lib(lib)?;
        Ok(())
    }

    /// Loads the library with the highest version and the given name from the search path,
    /// Returns the name of the library loaded.
    pub fn load_lib_highest_version(&mut self, name: SharedString) -> Result<LibName> {
        let lib = Library::load_highest_version(name.clone(), &self.search_paths)?;
        for dep in lib.deps.values() {
            self.load_lib(dep.clone())
                .chain(|| err!("When loading dependency of {}:", name))?;
        }
        let name = lib.name.clone();
        self.finish_loading_and_insert_lib(lib)?;
        Ok(name)
    }

    /// Loads the modules from the given library.
    fn finish_loading_and_insert_lib(&mut self, mut lib: Library) -> Result<()> {
        // Insert a dependency on std, if std has been loaded.
        if let Some(ref std) = self.std {
            lib.deps.entry("std".into()).or_insert_with(|| std.clone());
        }

        let mut mods = lib.find_modules()?;
        self.with_lib(lib.name, lib.deps, lib.path, |lib_ctx| {
            while let Some(name) = mods.keys().cloned().next() {
                lib_ctx.load_module_recursively(&mut mods, name)?;
            }
            Ok(())
        })
        .map(|_| ())
    }

    /// Runs the given closure with a `LibContext`, cleaning up afterwards.
    pub fn with_lib<'c, F>(
        &'c mut self,
        name: LibName,
        deps: HashMap<SharedString, LibName>,
        path: Option<SharedPath>,
        f: F,
    ) -> Result<&'c mut Context>
    where
        F: FnOnce(&mut LibContext<'c>) -> Result<()>,
    {
        let mut lib_ctx = self.create_lib(name, deps, path);
        match f(&mut lib_ctx) {
            Ok(()) => lib_ctx.finish(),
            Err(err) => {
                lib_ctx.discard();
                Err(err)
            }
        }
    }

    /// Inserts the given library, creating an entry for it in the context.
    pub fn insert_lib(&mut self, library: Library) -> Result<()> {
        if self.libs.contains_key(&library.name) {
            raise!("Library {} already exists", library.name)
        }

        // TODO: Check imports, exports, etc.
        self.libs.insert(library.name.clone(), library);
        Ok(())
    }

    /// Returns the declaration referenced by the given fully qualified name. If `enforce_exports`
    /// is false, the decl is returned regardless of whether it is public.
    pub fn get_decl(&self, name: FQName, enforce_exports: bool) -> Result<(FQName, &Decl)> {
        let library = self
            .libs
            .get(&name.0)
            .ok_or_else(|| err!("No library {} exists", name.0))?;
        let module = library.mods.get(&name.1).ok_or_else(|| {
            if name.1 == "" {
                err!("No module {} exists", name.0)
            } else {
                err!("No module {}:{} exists", name.0, name.1)
            }
        })?;

        if enforce_exports && !module.exports.contains(&name.2) {
            if name.1 == "" {
                raise!("{} does not export {}", name.0, name.2)
            } else {
                raise!("{}:{} does not export {}", name.0, name.1, name.2)
            }
        }

        for decl in &module.decls {
            if decl.name() == &name.2 {
                return Ok((name, decl));
            }
        }

        for (lib_name, library) in module.imports.iter() {
            for (mod_name, module) in library.iter() {
                if module.contains(&name.2) {
                    return self.get_decl(
                        FQName(lib_name.clone(), mod_name.clone(), name.2.clone()),
                        enforce_exports,
                    );
                }
            }
        }

        if name.1 == "" {
            raise!("{} does not define {}", name.0, name.2)
        } else {
            raise!("{}:{} does not define {}", name.0, name.1, name.2)
        }
    }

    /// Returns the module given by the given library name and module name.
    pub fn get_module(&self, lib_name: LibName, mod_name: SharedString) -> Result<&Module> {
        let lib = self
            .libs
            .get(&lib_name)
            .ok_or_else(|| err!("No library exists named {}", lib_name))?;
        lib.mods
            .get(&mod_name)
            .ok_or_else(|| err!("No module exists named {}", mod_name))
    }

    /// Returns the name of the standard library, if it has been loaded.
    pub fn std(&self) -> Option<LibName> {
        self.std.clone()
    }

    /// Returns the exports of the prelude module, if one exists.
    pub fn prelude_exports(&self) -> Option<&HashSet<SharedString>> {
        let m = self.get_module(self.std()?, "prelude".into()).ok()?;
        Some(&m.exports)
    }
}
