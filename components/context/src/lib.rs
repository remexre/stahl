//! The context of the compiler and interpreter.
//!
//! This is more or less just the data structures, without much logic.

#[macro_use]
extern crate stahl_errors;

use owning_ref::OwningRefMut;
use stahl_ast::Decl;
use stahl_errors::Result;
use stahl_util::SharedString;
use std::collections::HashMap;

/// The context in which compilation and interpretation occur.
#[derive(Debug, Default)]
pub struct Context {
    libs: HashMap<(SharedString, u16, u16, u32), HashMap<SharedString, Vec<Decl>>>,
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
        let mods = OwningRefMut::new(self).map_mut(|ctx| ctx.libs.get_mut(&key).unwrap());
        Ok(LibContext { key, mods })
    }
}

/// A single library's portion of the context.
#[derive(Debug)]
pub struct LibContext<'a> {
    key: (SharedString, u16, u16, u32),
    mods: OwningRefMut<&'a mut Context, HashMap<SharedString, Vec<Decl>>>,
}

impl LibContext<'_> {
    /// Creates a context for the module with the given name.
    pub fn create_mod(&mut self, name: SharedString) -> Result<&mut ModContext> {
        if self.mods.contains_key(&name) {
            raise!(
                "Module {}-{}-{}-{}/{} already exists",
                self.key.0.as_ref(),
                self.key.1,
                self.key.2,
                self.key.3,
                name.as_ref()
            )
        }
        unimplemented!();
    }
}

/// A single module's portion of the context.
#[derive(Debug)]
pub struct ModContext<'a> {
    lib: (SharedString, u16, u16, u32),
    name: SharedString,
    decls: OwningRefMut<&'a mut Context, Vec<Decl>>,
}

impl ModContext<'_> {
    /// Adds a new declaration.
    pub fn add(&mut self, decl: Decl) -> Result<()> {
        let name = decl.name();
        if self.decls.iter().any(|decl| decl.name() == name) {
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
        self.decls.push(decl);
        Ok(())
    }

    /// Resolves a name in the module context.
    pub fn resolve(&self, name: SharedString) -> Result<&Decl> {
        todo!()
    }
}
