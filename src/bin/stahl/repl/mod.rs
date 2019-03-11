mod completions;
mod highlighting;
mod hinting;

use maplit::{hashmap, hashset};
use rustyline::{CompletionType, Config, EditMode, Editor};
use stahl_ast::{Decl, LibName};
use stahl_context::{Context, ModContext};
use stahl_cst::Expr as CstExpr;
use stahl_errors::{raise, Location, Result};
use stahl_parser::parse_str_one;
use stahl_util::SharedString;
use std::{cell::RefCell, sync::Arc};

/// A command entered at the REPL.
#[derive(Debug)]
enum ReplCommand {
    /// Evaluates a value, showing its type and normal form.
    Eval(Arc<CstExpr>),

    /// Quits the REPL.
    Quit,

    /// Returns the type of an expression.
    TypeOf(Arc<CstExpr>),

    /// Returns the type of a variable.
    TypeOfName(SharedString),
}

impl ReplCommand {
    fn parse_str(s: &str, loc: Location) -> Result<ReplCommand> {
        let s = s.trim();

        if s.starts_with(":q") {
            Ok(ReplCommand::Quit)
        } else if s.starts_with(":t ") {
            parse_str_one(&s[3..], loc)
                .and_then(|val| CstExpr::from_value_unnamed(&val, "the REPL"))
                .map(ReplCommand::TypeOf)
        } else if s.starts_with(":tname ") {
            Ok(ReplCommand::TypeOfName(s[7..].into()))
        } else {
            parse_str_one(s, loc)
                .and_then(|val| CstExpr::from_value_unnamed(&val, "the REPL"))
                .map(ReplCommand::Eval)
        }
    }
}

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
        match run_line(&mut *mod_ctx.borrow_mut(), &line) {
            Ok(true) => {}
            Ok(false) => break,
            Err(e) => error!("{}", e),
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

fn run_line(mod_ctx: &mut ModContext, line: &str) -> Result<bool> {
    let loc = Location::new().name("the REPL".into());

    match ReplCommand::parse_str(&line, loc.clone())? {
        ReplCommand::Eval(expr) => {
            let (expr, ty) = mod_ctx.elab(&expr, &CstExpr::Hole(loc.clone()))?;
            println!("expr = {}", expr);
            println!("type = {}", ty);

            // let interp = Interpreter::new(&mut mod_ctx);
            // println!("{}", interp.is_normal(&expr));

            Ok(true)
        }
        ReplCommand::Quit => Ok(false),
        ReplCommand::TypeOf(expr) => {
            let (_, ty) = mod_ctx.elab(&expr, &CstExpr::Hole(loc.clone()))?;
            println!("{}", ty);
            Ok(true)
        }
        ReplCommand::TypeOfName(name) => {
            let (_, decl) = mod_ctx.resolve(name.clone())?;
            match decl {
                Decl::Def(_, _, ty, _) => println!("{} : {}", name, ty),
                _ => raise!(@decl.loc(), "{} is not a value", name),
            }
            Ok(true)
        }
    }
}

struct Helper<'m, 'l, 'c>(&'m RefCell<ModContext<'l, 'c>>);

impl rustyline::Helper for Helper<'_, '_, '_> {}
