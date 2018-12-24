//! The Stahl module system.

#[macro_use]
extern crate stahl_errors;

mod from_values;

use stahl_ast::Decl;
use stahl_util::SharedString;
use std::collections::HashMap;

/// A library.
#[derive(Debug)]
pub struct Library {
    /// The modules in the library.
    pub mods: HashMap<SharedString, Module>,
}

/// A module.
#[derive(Debug)]
pub struct Module {
    /// The values exported from the module.
    pub exports: Vec<SharedString>,

    /// The module's imports.
    pub imports: HashMap<SharedString, HashMap<SharedString, Vec<SharedString>>>,

    /// The declarations in the module.
    pub decls: Vec<Decl>,
}
