//! The Stahl module system.
#![deny(missing_docs)]

#[macro_use]
extern crate stahl_errors;

mod from_values;
mod load;

use crate::load::load_lib_stahld;
use stahl_ast::{Decl, FQName, LibName};
use stahl_errors::Result;
use stahl_util::{SharedPath, SharedString};
use std::collections::{HashMap, HashSet};

/// A library.
#[derive(Debug)]
pub struct Library {
    /// The name and version of the library.
    pub name: LibName,

    /// The versioned names of the libraries this library depends on.
    pub dep_versions: HashMap<SharedString, LibName>,

    /// The modules in the library.
    pub mods: HashMap<SharedString, Module>,

    /// The path of the library, if any.
    pub path: Option<SharedPath>,
}

impl Library {
    /// Loads the `lib.stahld` file for a given `LibName`. Note that the `mods` field is
    /// unpopulated.
    pub fn load(name: LibName, search_paths: &[SharedPath]) -> Result<Library> {
        load_lib_stahld(name.clone(), search_paths).map(|(path, dep_versions)| Library {
            name,
            dep_versions,
            mods: HashMap::new(),
            path: Some(path),
        })
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
    /// Resolves a local name inside this module.
    pub fn resolve(&self, name: SharedString) -> Option<FQName> {
        assert!(!name.contains(':'));

        // Check declarations in the module.
        for decl in &self.decls {
            if decl.name() == name {
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
