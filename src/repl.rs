use rustyline::{
    config::{Config, EditMode},
    Editor,
};
use stahl_errors::Result;
use stahl_parser::parse_str;

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

    while let Ok(line) = rl.readline("\u{03bb}> ") {
        if let Err(e) = run_line(line) {
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

fn run_line(line: String) -> Result<()> {
    let exprs = parse_str(&line)?;
    for expr in exprs {
        println!("{}", expr);
        println!("{:?}", stahl_ast::Expr::from_value(&expr));
    }

    Ok(())
}
