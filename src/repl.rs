use rustyline::Editor;
use stahl_errors::Result;
use stahl_parser::parse_str;

/// Runs the REPL.
pub fn run() -> Result<()> {
    let mut rl = Editor::<()>::new();
    while let Ok(line) = rl.readline("\u{03bb}> ") {
        println!("line = {}", line);
        println!("     = {:?}", parse_str(&line));
    }
    Ok(())
}
