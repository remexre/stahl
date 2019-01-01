use maplit::hashmap;
use stahl_ast::LibName;
use stahl_context::Context;
use stahl_errors::{Location, Result};
use stahl_modules::Module;
use stahl_parser::parse_file;
use stahl_util::SharedPath;
use std::{path::PathBuf, sync::Arc};

/// Runs a file as a script.
pub fn run(main: PathBuf) -> Result<()> {
    let main = SharedPath::new(Arc::from(main));
    let vals = parse_file(main.clone())?;
    let (mod_name, exports, imports, decls) =
        Module::from_values(vals, Location::new().path(main))?;

    let mut context = Context::new();
    let mut library = context.create_lib(LibName("#script#".into(), 0, 0, 0), hashmap! {});
    let mut module = library.create_mod(mod_name, exports, imports)?;

    for decl in decls {
        module.add_cst_decl(decl)?;
    }

    module.finish()?;
    library.finish()?;

    Ok(())
}
