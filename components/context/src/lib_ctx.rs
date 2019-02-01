use crate::{mod_ctx::ModContext, Context};
use stahl_ast::{Decl, FQName};
use stahl_errors::{Location, Result};
use stahl_modules::{Library, Module};
use stahl_parser::parse_file;
use stahl_util::{SharedPath, SharedString, Taker};
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    thread::panicking,
};

/// A single library's portion of the context.
#[derive(Debug)]
pub struct LibContext<'c> {
    pub(crate) context: Taker<&'c mut Context>,
    pub(crate) library: Taker<Library>,
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
