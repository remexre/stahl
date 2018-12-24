use stahl_errors::{Location, Result};
use stahl_modules::Module;
use stahl_parser::parse_file;
use stahl_util::SharedPath;
use std::{path::PathBuf, sync::Arc};

/// Runs a file as a script.
pub fn run(main: PathBuf) -> Result<()> {
    let main = SharedPath::new(Arc::from(main));
    let vals = parse_file(main.clone())?;
    let (_, module) = Module::from_values(vals, Location::new_file(Some(main)))?;
    println!("{:?}", module);

    for decl in module.decls {
        println!("{}", decl);
    }
    Ok(())
}
