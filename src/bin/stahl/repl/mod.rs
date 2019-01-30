mod completions;
mod highlighting;
mod hinting;

use maplit::{hashmap, hashset};
use rustyline::{CompletionType, Config, EditMode, Editor};
use stahl_ast::LibName;
use stahl_context::{Context, ModContext};
use stahl_cst::Expr as CstExpr;
use stahl_errors::{Location, Result};
use stahl_parser::parse_str;
use std::cell::RefCell;

/// Runs the REPL.
pub fn run(ctx: &mut Context, main: Option<String>) -> Result<()> {
    let std = ctx.std().unwrap();
    let mut import_libs = hashmap! { std.0.clone() => std };
    let mut import_mods = hashmap! {};
    if let Some(main) = main {
        let i = main.find(':').unwrap_or_else(|| main.len());
        let library = &main[..i];
        let module = if let Some(i) = main.find(':') {
            &main[i + 1..]
        } else {
            ""
        };

        let main = ctx.load_lib_highest_version(library.into())?;
        let exports = ctx.get_module(main.clone(), module.into())?.exports.clone();
        import_libs.insert(main.0.clone(), main.clone());
        import_mods.insert(
            main.0,
            hashmap! {
                "".into() => exports,
            },
        );
    }

    let mut lib_ctx = ctx.create_lib(LibName("#repl#".into(), 0, 0, 0), import_libs, None);
    let mut mod_ctx = lib_ctx.create_mod("".into(), hashset! {}, import_mods)?;
    mod_ctx.add_prelude_import(true)?;

    let mod_ctx = RefCell::new(mod_ctx);

    let config = Config::builder()
        .auto_add_history(true)
        .edit_mode(EditMode::Emacs)
        .completion_type(CompletionType::List)
        .build();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(Helper(&mod_ctx)));

    let history_path = dirs::data_dir().map(|path| path.join("stahl").join("history"));
    if let Some(path) = history_path.as_ref() {
        if path.exists() {
            if let Err(e) = rl.load_history(path) {
                error!("When loading history: {}", e);
            }
        }
    }

    while let Ok(line) = rl.readline("\u{03bb}> ") {
        if let Err(e) = run_line(&mut *mod_ctx.borrow_mut(), &line) {
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

    mod_ctx.into_inner().discard();
    lib_ctx.discard();

    Ok(())
}

fn run_line(mod_ctx: &mut ModContext, line: &str) -> Result<()> {
    let loc = Location::new().name("the REPL".into());

    let exprs = parse_str(&line, loc.clone())?;
    for expr in exprs {
        let expr = CstExpr::from_value_unnamed(&expr, "the REPL")?;
        let (expr, ty) = mod_ctx.elab(&expr, &CstExpr::Hole(loc.clone()))?;
        println!("expr = {}", expr);
        println!("type = {}", ty);

        // let interp = Interpreter::new(&mut mod_ctx);
        // println!("{}", interp.is_normal(&expr));
    }

    Ok(())
}

struct Helper<'m, 'l, 'c>(&'m RefCell<ModContext<'l, 'c>>);

impl rustyline::Helper for Helper<'_, '_, '_> {}
