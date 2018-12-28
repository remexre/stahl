use rustyline::{
    config::{Config, EditMode},
    Editor,
};
use stahl_context::{Context, ModContext};
use stahl_cst::Expr as CstExpr;
use stahl_errors::{Location, Result};
use stahl_parser::parse_str;
use stahl_util::SharedString;

/// Runs the REPL.
pub fn run() -> Result<()> {
    let config = Config::builder()
        .auto_add_history(true)
        .edit_mode(EditMode::Vi)
        .build();
    let mut rl = Editor::<()>::with_config(config);

    let history_path = dirs::data_dir().map(|path| path.join("stahl").join("history"));
    if let Some(path) = history_path.as_ref() {
        if path.exists() {
            if let Err(e) = rl.load_history(path) {
                error!("When loading history: {}", e);
            }
        }
    }

    let mut ctx = Context::new();
    let mut lib_ctx = ctx.create_lib(SharedString::from("#repl#"), 0, 0, 0)?;
    let mut mod_ctx =
        lib_ctx.create_mod(SharedString::from(""), vec![], vec![].into_iter().collect())?;
    while let Ok(line) = rl.readline("\u{03bb}> ") {
        if let Err(e) = run_line(&mut mod_ctx, line) {
            error!("{}", e);
        }
    }

    if let Some(path) = history_path.as_ref() {
        if let Some(dir) = path.parent() {
            if let Err(e) = std::fs::create_dir_all(dir) {
                error!("When creating data directory: {}", e);
            }
        }
        if let Err(e) = rl.save_history(path) {
            error!("When saving history: {}", e);
        }
    }

    Ok(())
}

fn run_line(mod_ctx: &mut ModContext, line: String) -> Result<()> {
    let exprs = parse_str(&line)?;
    for expr in exprs {
        println!("{}", expr);
        let expr = CstExpr::from_value_unnamed(&expr, "the REPL")?;
        let (expr, ty) = mod_ctx.elab(&expr, &CstExpr::Hole(Location::default()))?;
        println!("expr = {}", expr);
        println!("  ty = {}", ty);

        // let interp = Interpreter::new(&mut mod_ctx);
        // println!("{}", interp.is_normal(&expr));
    }

    Ok(())
}
