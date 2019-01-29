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
mod inductive;
mod types;
mod zipper;

use crate::{
    elab::reify,
    inductive::{ctor_type, elim_type, elim_value},
};
pub use crate::{
    types::{UnifEffs, UnifExpr},
    zipper::Zipper,
};
use log::info;
use stahl_ast::{Decl, Effects, Expr, FQName, Intrinsic, LibName};
use stahl_cst::Decl as CstDecl;
use stahl_errors::{Location, Result, ResultExt};
use stahl_modules::{Library, Module};
use stahl_parser::parse_file;
use stahl_util::{genint, SharedPath, SharedString, Taker};
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
    thread::panicking,
};

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
            raise!("Module {} already exists", module.name())
        }

        let mut decl_names = module
            .decls
            .iter()
            .map(|d| d.name())
            .collect::<HashSet<_>>();
        for (lib_name, mods) in &module.imports {
            for (mod_name, names) in mods {
                for name in names {
                    if decl_names.contains(name) {
                        raise!(
                            "Module {} imports {}, but that already exists in the module",
                            module.name(),
                            name
                        );
                    }

                    let name = FQName(lib_name.clone(), mod_name.clone(), name.clone());
                    self.get_decl(name.clone()).ok_or_else(|| {
                        err!(
                            "Module {} can't import non-existent {}",
                            module.name(),
                            name
                        )
                    })?;
                    decl_names.insert(name.2);
                }
            }
        }

        for name in &module.exports {
            if !decl_names.contains(name) {
                raise!(
                    "Module {} exports {}, but does not define it",
                    module.name(),
                    name
                );
            }
        }

        self.library.mods.insert(module.mod_name.clone(), module);
        Ok(())
    }

    /// Returns the decl referred to by the given global name.
    pub fn get_decl(&self, name: FQName) -> Option<(FQName, &Decl)> {
        if self.name == name.0 {
            let module = self.mods.get(&name.1)?;

            for decl in &module.decls {
                if decl.name() == &name.2 {
                    return Some((name, decl));
                }
            }

            for (lib_name, mods) in &module.imports {
                for (mod_name, names) in mods {
                    for imp_name in names {
                        if imp_name == &name.2 {
                            return self.get_decl(FQName(
                                lib_name.clone(),
                                mod_name.clone(),
                                imp_name.clone(),
                            ));
                        }
                    }
                }
            }
            None
        } else {
            self.context.get_decl(name, true).ok()
        }
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
            mod_ctx.add_prelude_import(false)?;
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

    /// Resolves the name of a definition the module context, returning its declaration.
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

/// A single `def`'s portion of the context.
#[derive(Debug)]
pub struct DefContext<'m, 'l, 'c> {
    module: Taker<&'m mut ModContext<'l, 'c>>,
    loc: Taker<Location>,
    name: Taker<SharedString>,
    type_zipper: Taker<Zipper>,
    expr_zipper: Taker<Zipper>,
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

/// A single `defty`'s portion of the context, while defining the kind of the type.
#[derive(Debug)]
pub struct DefTyKindContext<'m, 'l, 'c> {
    module: Taker<&'m mut ModContext<'l, 'c>>,
    loc: Taker<Location>,
    name: Taker<SharedString>,
    kind_zipper: Taker<Zipper>,
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
    module: Taker<&'m mut ModContext<'l, 'c>>,
    loc: Taker<Location>,
    name: Taker<SharedString>,
    ty_args: Taker<Vec<(Option<SharedString>, Arc<Expr>)>>,
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
        let kind = self.kind();
        let loc = self.loc.take();
        let name = self.name.take();
        let ty_args = self.ty_args.take();
        let ctors = self.ctors.take();

        let lib_name = self.module.lib_name.clone();
        let mod_name = self.module.mod_name.clone();

        let ty_name = FQName(lib_name.clone(), mod_name.clone(), name.clone());
        let ty = Arc::new(Expr::Atom(loc.clone(), ty_name.clone(), kind.clone()));
        self.module
            .add(Decl::Def(loc.clone(), name.clone(), kind, ty))?;

        for ctor in &ctors {
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
