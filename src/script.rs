use stahl_ast::Decl;
use stahl_errors::Result;
use stahl_parser::parse_file;
use stahl_util::SharedPath;
use std::{path::PathBuf, sync::Arc};

/// Runs a file as a script.
pub fn run(main: PathBuf) -> Result<()> {
    let vals = parse_file(SharedPath::new(Arc::from(main)))?;
    for val in vals {
        let decl = Decl::from_value(val)?;
        println!("{}", decl);
    }
    Ok(())
}
