//! The context of the compiler and interpreter.
//!
//! This should be your main interface to the compiler.
#![deny(missing_docs)]

#[macro_use]
extern crate derivative;
#[macro_use]
extern crate stahl_errors;

mod builtins;
mod elab;
mod types;
mod zipper;

use crate::elab::reify;
pub use crate::{
    types::{UnifEffs, UnifExpr},
    zipper::Zipper,
};
use stahl_ast::{Decl, FQName, LibName};
use stahl_cst::Decl as CstDecl;
use stahl_errors::{Location, Result, ResultExt};
use stahl_modules::{Library, Module};
use stahl_parser::parse_file;
use stahl_util::{genint, SharedPath, SharedString, Taker};
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    rc::Rc,
    thread::panicking,
};

/// The context in which compilation and interpretation occur.
#[derive(Debug)]
pub struct Context {
    libs: HashMap<LibName, Library>,

    /// The paths searched when looking for libraries.
    pub search_paths: Vec<SharedPath>,
}

impl Context {
    /// Creates a new `Context`.
    pub fn new<II, P>(search_paths: II) -> Context
    where
        II: IntoIterator<Item = P>,
        P: Into<SharedPath>,
    {
        let mut ctx = Context {
            libs: HashMap::new(),
            search_paths: search_paths.into_iter().map(Into::into).collect(),
        };
        builtins::add_to(&mut ctx);
        ctx
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
    fn finish_loading_and_insert_lib(&mut self, lib: Library) -> Result<()> {
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
    pub fn get_decl(&self, name: FQName, enforce_exports: bool) -> Result<&Decl> {
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
                return Ok(decl);
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
}

/// A single library's portion of the context.
#[derive(Debug)]
pub struct LibContext<'c> {
    context: Taker<&'c mut Context>,
    library: Taker<Library>,
}

impl<'c> LibContext<'c> {
    /// Adds this library to the context.
    pub fn finish(mut self) -> Result<&'c mut Context> {
        self.context.insert_lib(self.library.take())?;
        Ok(self.context.take())
    }

    /// Discards this library. Use this instead of just dropping in the case where the library
    /// should not be added!
    pub fn discard(mut self) {
        self.context.take();
        self.library.take();
    }

    /// Creates a context for the module with the given name.
    pub fn create_mod<'l>(
        &'l mut self,
        name: SharedString,
        exports: HashSet<SharedString>,
        imports: HashMap<SharedString, HashMap<SharedString, HashSet<SharedString>>>,
    ) -> Result<ModContext<'l, 'c>> {
        let imports = imports
            .into_iter()
            .map(|(lib_name, lib_imports)| {
                if lib_name == self.library.name.0 {
                    Ok((self.library.name.clone(), lib_imports))
                } else if let Some(name) = self.library.deps.get(&lib_name) {
                    Ok((name.clone(), lib_imports))
                } else {
                    raise!(
                        "Library {} has no dependency on {}, so it can't import from it",
                        self.library.name,
                        lib_name
                    )
                }
            })
            .collect::<Result<_>>()?;

        let lib_name = self.library.name.clone();
        Ok(ModContext {
            library: self.into(),
            module: Module {
                lib_name,
                mod_name: name,
                exports,
                imports,
                decls: vec![],
            }
            .into(),
        })
    }

    /// Runs the given closure with a `ModContext`, cleaning up afterwards.
    pub fn with_mod<'l, F>(
        &'l mut self,
        name: SharedString,
        exports: HashSet<SharedString>,
        imports: HashMap<SharedString, HashMap<SharedString, HashSet<SharedString>>>,
        f: F,
    ) -> Result<&'l mut LibContext<'c>>
    where
        F: FnOnce(&mut ModContext<'l, 'c>) -> Result<()>,
    {
        let mut mod_ctx = self.create_mod(name, exports, imports)?;
        match f(&mut mod_ctx) {
            Ok(()) => mod_ctx.finish(),
            Err(err) => {
                mod_ctx.discard();
                Err(err)
            }
        }
    }

    /// Inserts the given module, creating an entry for it in the context.
    pub fn insert_mod(&mut self, module: Module) -> Result<()> {
        if self.library.mods.contains_key(&module.mod_name) {
            raise!(
                "Module {}:{} already exists",
                self.library.name,
                module.mod_name
            )
        }

        // TODO: Check imports, exports, etc.
        self.library.mods.insert(module.mod_name.clone(), module);
        Ok(())
    }

    /// Loads a module, and all of the modules from the same library that it depends on. Used
    /// during library loading.
    pub fn load_module_recursively(
        &mut self,
        mods: &mut HashMap<SharedString, SharedPath>,
        name: SharedString,
    ) -> Result<()> {
        let path = mods.remove(&name).unwrap();
        let loc = Location::new().path(path.clone());

        // Parse the module.
        let vals = parse_file(path.clone())?;
        let (mod_name, exports, imports, decls) =
            Module::from_values(vals, Location::new().path(path.clone()))?;
        let l = self.name.0.len();
        let name_ok = if name == "" {
            mod_name == self.name.0
        } else {
            mod_name.len() >= l + 1
                && &mod_name[..l] == self.name.0.as_str()
                && &mod_name[l..l + 1] == ":"
                && &mod_name[l + 1..] == name.as_str()
        };
        if !name_ok {
            let name = format!("{}:{}", self.name.0, name);
            raise!(@loc, "Expected module to be named {:?}, found {:?}",
                name, mod_name);
        }

        // Load all the dependencies.
        if let Some(internal_imports) = imports.get(&self.name.0) {
            for mod_name in internal_imports.keys() {
                if *mod_name == name {
                    raise!(@loc, "Module {}:{} cannot import itself!", self.name.0, name)
                } else if mods.contains_key(mod_name) {
                    self.load_module_recursively(mods, mod_name.clone())?;
                } else if !self.mods.contains_key(mod_name) {
                    raise!(@loc, "Module {}:{} was imported but not found, or was part of a cycle",
                           self.name.0, mod_name);
                }
            }
        }

        // Load the module in.
        self.with_mod(name, exports, imports, |mod_ctx| {
            decls
                .into_iter()
                .map(|decl| mod_ctx.add_cst_decl(decl))
                .collect::<Result<_>>()
        })?;

        Ok(())
    }
}

impl Deref for LibContext<'_> {
    type Target = Library;
    fn deref(&self) -> &Library {
        &*self.library
    }
}

impl DerefMut for LibContext<'_> {
    fn deref_mut(&mut self) -> &mut Library {
        &mut *self.library
    }
}

impl Drop for LibContext<'_> {
    fn drop(&mut self) {
        if !(panicking() || self.context.taken() || self.library.taken()) {
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
                        Ok((name, Decl::DefEffSet(_, _, _))) => todo!(@loc.clone()),
                        Ok((name, Decl::Def(_, _, _, _))) => {
                            raise!(@loc.clone(), "{} is a value, not an effect", name)
                        }
                        Ok((name, Decl::DefTy(_, _, _, _))) => {
                            raise!(@loc.clone(), "{} is a type, not an effect", name)
                        }
                        Err(err) => Err(err),
                    })
                    .collect::<Result<_>>()?;
                self.add(Decl::DefEffSet(loc, name, effs))
            }
            CstDecl::DefTy(loc, _name, _ty_args, _ctors) => todo!(@loc),
        }
    }

    /// Begins creating a def.
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

    /// Returns the decl referred to by the given global name.
    pub fn get_decl(&self, name: FQName) -> Option<&Decl> {
        if self.module.lib_name == name.0 {
            let module = if self.module.mod_name == name.1 {
                &self.module
            } else {
                self.library.mods.get(&name.1)?
            };

            for decl in &module.decls {
                if decl.name() == name.2 {
                    return Some(decl);
                }
            }
            None
        } else {
            self.library.context.get_decl(name, true).ok()
        }
    }

    /// Resolves the name of a definition the module context, returning its declaration.
    pub fn resolve(&self, name: SharedString) -> Result<(FQName, &Decl)> {
        if !name.contains(':') {
            match self.module.resolve(name.clone()) {
                Some(name) => {
                    if name.0 == self.module.lib_name {
                        if name.1 == self.module.mod_name {
                            match self.decls.iter().find(|decl| decl.name() == name.2) {
                                Some(decl) => Ok((name, decl)),
                                None => panic!("resolve() lied about {} being local", name),
                            }
                        } else {
                            if let Some(m) = self.library.library.mods.get(&name.1) {
                                match m.decls.iter().find(|decl| decl.name() == name.2) {
                                    Some(decl) => {
                                        if true {
                                            Ok((name, decl))
                                        } else {
                                            raise!(
                                                "{}:{} does not export {}",
                                                name.0,
                                                name.1,
                                                name.2
                                            )
                                        }
                                    }
                                    None => {
                                        raise!("{}:{} does not declare {}", name.0, name.1, name.2)
                                    }
                                }
                            } else {
                                raise!("Cannot find {} in non-existent module", name);
                            }
                        }
                    } else {
                        self.library
                            .context
                            .get_decl(name.clone(), true)
                            .map(|decl| (name, decl))
                    }
                }
                None => raise!("No definition for {} exists", name),
            }
        } else {
            let name = name.parse::<FQName>()?;
            match self.get_decl(name.clone()) {
                Some(decl) => Ok((name, decl)),
                None => raise!("No definition for {} exists", name),
            }
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
        self.module.unify_ty_expr(&mut ty, &mut expr)?;

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
