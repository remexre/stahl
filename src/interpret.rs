use stahl_ast::{Expr, FQName};
use stahl_context::Context;
use stahl_errors::{Location, Result};
use stahl_interpreter::Interpreter;
use std::sync::Arc;

/// Runs a package's main function.
pub fn run(mut ctx: Context, main: String, _args: Vec<String>) -> Result<()> {
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
