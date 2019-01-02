use maplit::{hashmap, hashset};
use stahl_ast::{Decl, Expr, Intrinsic, LibName};
use stahl_context::Context;
use stahl_errors::Location;
use stahl_modules::Module;
use stahl_util::SharedString;
use std::sync::Arc;

/// Creates the `#compiler-builtins#` library.
pub fn create_compiler_builtins_lib(ctx: &mut Context) {
    let lib_name = LibName("#compiler-builtins#".into(), 0, 0, 0);
    let mod_name = SharedString::from("");
    let loc = Location::new().name("compiler builtin".into());

    let fixnum_name = SharedString::from("fixnum");
    let fixnum_type = Arc::new(Expr::Type(loc.clone()));
    let fixnum_expr = Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Fixnum));

    let type_name = SharedString::from("type");
    let type_type = Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::TypeOfTypeOfTypes));
    let type_expr = Arc::new(Expr::Type(loc.clone()));

    let mut lib_ctx = ctx.create_lib(lib_name.clone(), hashmap! {});
    lib_ctx
        .insert_mod(Module {
            lib_name,
            mod_name,
            exports: hashset! {fixnum_name.clone(), type_name.clone()},
            imports: hashmap! {},
            decls: vec![
                Decl::Def(loc.clone(), fixnum_name, fixnum_type, fixnum_expr),
                Decl::Def(loc, type_name, type_type, type_expr),
            ],
        })
        .unwrap();
    lib_ctx.finish().unwrap();
}
