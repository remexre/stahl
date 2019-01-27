//! The Stahl module system.
#![deny(missing_docs)]

#[macro_use]
extern crate log;
#[macro_use]
extern crate stahl_errors;

mod from_values;
mod load;

use crate::load::lib_stald_iter;
use stahl_ast::{Decl, FQName, LibName};
use stahl_errors::{Location, Result};
use stahl_util::{SharedPath, SharedString};
use std::{
    collections::{HashMap, HashSet},
    fs::read_dir,
    path::Path,
};

/// A library.
#[derive(Debug)]
pub struct Library {
    /// The name and version of the library.
    pub name: LibName,

    /// The versioned names of the libraries this library depends on.
    pub deps: HashMap<SharedString, LibName>,

    /// The modules in the library.
    pub mods: HashMap<SharedString, Module>,

    /// The path of the library, if any.
    pub path: Option<SharedPath>,
}

impl Library {
    /// Loads the library with the given name and version, without populating its modules.
    pub fn load(name: LibName, search_paths: &[SharedPath]) -> Result<Library> {
        lib_stald_iter(name.0.as_str(), search_paths)
            .filter(|(_, n, _)| *n == name)
            .next()
            .map(|(path, _, deps)| Library {
                name: name.clone(),
                deps,
                mods: HashMap::new(),
                path: Some(path),
            })
            .ok_or_else(|| err!("Couldn't find library {}", name))
    }

    /// Loads the library with the given name, without populating its modules.
    pub fn load_highest_version(
        name: SharedString,
        search_paths: &[SharedPath],
    ) -> Result<Library> {
        lib_stald_iter(name.as_str(), search_paths)
            .max_by_key(|&(_, LibName(_, major, minor, patch), _)| (major, minor, patch))
            .map(|(path, name, deps)| Library {
                name,
                deps,
                mods: HashMap::new(),
                path: Some(path),
            })
            .ok_or_else(|| err!("Couldn't find library {}", name))
    }

    /// Finds the modules of the library.
    pub fn find_modules(&self) -> Result<HashMap<SharedString, SharedPath>> {
        fn find_modules_in(
            dir: &Path,
            mod_name: &mut String,
            mods: &mut HashMap<SharedString, SharedPath>,
        ) -> Result<()> {
            for entry in read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                let stem = path.file_stem().and_then(|s| s.to_str()).ok_or_else(|| {
                    err!(@Location::new().path(path.clone().into()), "{:?} is not a valid name",
                            path.file_stem())
                })?;
                let old_len = mod_name.len();
                mod_name.push(':');
                *mod_name += stem;
                if entry.file_type()?.is_dir() {
                    find_modules_in(&entry.path(), mod_name, mods)?;
                } else if path.extension() == Some("stahl".as_ref()) {
                    mods.insert((&mod_name as &str).into(), path.into());
                }
                mod_name.truncate(old_len);
            }
            Ok(())
        }

        let path = self
            .path
            .clone()
            .expect("Can't populate a library without its path!");
        let mut mods = HashMap::new();
        for entry in read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if entry.file_type()?.is_dir() {
                let mut name = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string())
                    .ok_or_else(|| {
                        err!(@Location::new().path(path.clone().into()), "{:?} is not a valid name",
                            path.file_stem())
                    })?;
                find_modules_in(&entry.path(), &mut name, &mut mods)?;
            } else if path.extension() == Some("stahl".as_ref()) {
                if path.file_stem() == Some(self.name.0.as_ref()) {
                    mods.insert("".into(), path.into());
                } else {
                    let mod_name = path.file_stem().and_then(|s| s.to_str()).ok_or_else(|| {
                        err!(@Location::new().path(path.clone().into()), "{:?} is not a valid name",
                            path.file_stem())
                    })?;
                    mods.insert(mod_name.into(), path.into());
                }
            }
        }
        Ok(mods)
    }
}

/// A module.
#[derive(Debug)]
pub struct Module {
    /// The name and version of the library the module is from.
    pub lib_name: LibName,

    /// The name of the module.
    pub mod_name: SharedString,

    /// The values exported from the module.
    pub exports: HashSet<SharedString>,

    /// The module's imports.
    pub imports: HashMap<LibName, HashMap<SharedString, HashSet<SharedString>>>,

    /// The declarations in the module.
    pub decls: Vec<Decl>,
}

impl Module {
    /// Returns the name of the module.
    pub fn name(&self) -> SharedString {
        if self.mod_name == "" {
            self.lib_name.to_string().into()
        } else {
            format!("{}:{}", self.lib_name, self.mod_name).into()
        }
    }

    /// Resolves a local name inside this module.
    pub fn resolve(&self, name: SharedString) -> Option<FQName> {
        assert!(!name.contains(':'));

        // Check declarations in the module.
        for decl in &self.decls {
            if decl.names().contains(&name) {
                return Some(FQName(self.lib_name.clone(), self.mod_name.clone(), name));
            }
        }

        for (lib_name, mods) in &self.imports {
            for (mod_name, names) in mods {
                for n in names {
                    if n == &name {
                        return Some(FQName(lib_name.clone(), mod_name.clone(), name));
                    }
                }
            }
        }

        None
    }
}
