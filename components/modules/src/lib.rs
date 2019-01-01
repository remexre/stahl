//! The Stahl module system.
#![deny(missing_docs)]

#[macro_use]
extern crate stahl_errors;

mod from_values;

use maplit::{hashmap, hashset};
use stahl_ast::{Decl, Expr, FQName};
use stahl_errors::Location;
use stahl_util::SharedString;
use std::collections::{HashMap, HashSet};

/// A library.
#[derive(Debug)]
pub struct Library {
    /// The name of the library.
    pub name: SharedString,

    /// The modules in the library.
    pub mods: HashMap<SharedString, Module>,
}

/// A module.
#[derive(Debug)]
pub struct Module {
    /// The name of the library the module is from.
    pub lib_name: SharedString,

    /// The name of the module.
    pub mod_name: SharedString,

    /// The values exported from the module.
    pub exports: HashSet<SharedString>,

    /// The module's imports.
    pub imports: HashMap<SharedString, HashMap<SharedString, HashSet<SharedString>>>,

    /// The declarations in the module.
    pub decls: Vec<Decl>,
}

impl Module {
    /// Creates a module containing the compiler intrinsics.
    pub fn intrinsics(lib_name: SharedString, mod_name: SharedString) -> Module {
        let loc = Location::new().name("compiler builtin".into());

        let type_name = SharedString::from("type");
        let type_type = Expr::TypeOfTypeOfTypes(loc.clone());
        let type_expr = Expr::Type(loc.clone());

        Module {
            lib_name,
            mod_name,
            exports: hashset! {type_name.clone()},
            imports: hashmap! {},
            decls: vec![Decl::Def(loc, type_name, type_type, type_expr)],
        }
    }

    /// Resolves a name inside this module. This may be a local name (a string that does not
    /// contain `/`, or is literally just `/`), or a global name (a string with more than one
    /// character that contains `/`).
    pub fn resolve(&self, name: SharedString) -> Option<FQName> {
        if name != "/" && name.contains('/') {
            unimplemented!("Convert {} to a FQName", name)
        } else {
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
}
