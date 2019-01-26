use maplit::hashmap;
use stahl_ast::LibName;
use stahl_context::Context;
use stahl_errors::Location;
use stahl_modules::Module;
use stahl_parser::parse_str;
use std::path::PathBuf;

static MODULE_SRC: &str = r#"
module main

; Got error "The target of a call must be a function, not #VAR:83#" in 149cd3b
def id-test-1
  (id _ S) Z

; Got panic "x not found" in 149cd3b
def const-test-1
  (const _ _ 1) 2
"#;

#[test]
fn regression_tests() {
    let mut ctx = Context::new(true, Some(PathBuf::from(".").into())).unwrap();
    let loc = Location::new().name("regression tests".into());
    let vals = parse_str(MODULE_SRC, loc.clone()).unwrap();
    let (_, exports, imports, decls) = Module::from_values(vals, loc).unwrap();

    let std = ctx.std().unwrap();
    ctx.with_lib(
        LibName("main".into(), 0, 0, 0),
        hashmap! { std.0.clone() => std },
        None,
        |lib_ctx| {
            lib_ctx
                .with_mod("".into(), exports, imports, |mod_ctx| {
                    mod_ctx.add_prelude_import(true)?;

                    for decl in decls {
                        mod_ctx.add_cst_decl(decl).unwrap()
                    }

                    Ok(())
                })
                .map(|_| ())
        },
    )
    .unwrap();
}
