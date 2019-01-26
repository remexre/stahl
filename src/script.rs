use maplit::hashmap;
use stahl_ast::{Expr, LibName};
use stahl_context::Context;
use stahl_errors::{Location, Result};
use stahl_interpreter::Interpreter;
use stahl_modules::Module;
use stahl_parser::parse_file;
use stahl_util::SharedPath;
use std::{path::PathBuf, sync::Arc};

/// Runs a file as a script.
pub fn run(mut ctx: Context, main: PathBuf, _args: Vec<String>) -> Result<()> {
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
