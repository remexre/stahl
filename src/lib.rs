//! A dependently typed Lisp with algebraic effects.
//!
//! This library contains higher-level functions; lower-level functionality is available in the
//! crates this one reexports.

pub use stahl_ast as ast;
pub use stahl_context as context;
pub use stahl_cst as cst;
pub use stahl_errors as errors;
pub use stahl_interpreter as interpreter;
pub use stahl_modules as modules;
pub use stahl_parser as parser;
pub use stahl_util as util;
pub use stahl_value as value;

use maplit::hashmap;
use stahl_ast::{Expr, FQName, LibName};
use stahl_context::Context;
use stahl_errors::{raise, Location, Result};
use stahl_interpreter::Interpreter;
use stahl_modules::Module;
use stahl_parser::parse_file;
use stahl_util::SharedPath;
use std::{path::PathBuf, sync::Arc};

/// Runs a package's main function.
pub fn run_package(ctx: &mut Context, main: String, _args: Vec<String>) -> Result<()> {
    let main = ctx.load_lib_highest_version(main.into())?;
    let main = FQName(main, "".into(), "main".into());

    // TODO: Type-check main.
    // TODO: Pass args.
    let loc = Location::new().name("the runtime".into());
    let call_to_main = Arc::new(Expr::Call(
        loc.clone(),
        Arc::new(Expr::GlobalVar(loc, main)),
        vec![],
    ));

    let mut interpreter = Interpreter::new(&ctx);
    let out = interpreter.eval(call_to_main);
    println!("{}", out);

    Ok(())
}

/// Runs a file as a script.
pub fn run_script(ctx: &mut Context, main: PathBuf, _args: Vec<String>) -> Result<()> {
    let main = SharedPath::from(main);
    let vals = parse_file(main.clone())?;
    let (mod_name, exports, imports, decls) =
        Module::from_values(vals, Location::new().path(main.clone()))?;

    if mod_name != "main" {
        raise!(@Location::new().path(main), "A script module must be named main");
    }

    let std = ctx.std().unwrap();
    ctx.with_lib(
        LibName("main".into(), 0, 0, 0),
        hashmap! { std.0.clone() => std },
        None,
        |lib_ctx| {
            lib_ctx
                .with_mod("".into(), exports, imports, |mod_ctx| {
                    mod_ctx.add_prelude_import(true)?;
                    decls
                        .into_iter()
                        .map(|decl| mod_ctx.add_cst_decl(decl))
                        .collect::<Result<_>>()
                })
                .map(|_| ())
        },
    )?;

    // TODO: Type-check main.
    // TODO: Pass args.
    let loc = Location::new().name("the runtime".into());
    let call_to_main = Arc::new(Expr::Call(
        loc.clone(),
        Arc::new(Expr::GlobalVar(loc, "main-0-0-0:main".parse().unwrap())),
        vec![],
    ));

    let mut interpreter = Interpreter::new(&ctx);
    let out = interpreter.eval(call_to_main);
    println!("{}", out);

    Ok(())
}
