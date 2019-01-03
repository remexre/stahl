use maplit::{hashmap, hashset};
use stahl_ast::{Decl, Effects, Expr, Intrinsic, LibName};
use stahl_context::Context;
use stahl_errors::Location;
use stahl_modules::Module;
use stahl_util::SharedString;
use std::{collections::HashSet, sync::Arc};

/// Creates the `#compiler-builtins#` library.
pub fn create_compiler_builtins_lib(ctx: &mut Context) -> HashSet<SharedString> {
    let lib_name = LibName("#compiler-builtins#".into(), 0, 0, 0);
    let mod_name = SharedString::from("");
    let loc = Location::new().name("compiler builtin".into());

    let fixnum_name = SharedString::from("fixnum");
    let fixnum_type = Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type));
    let fixnum_expr = Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Fixnum));

    let the_name = SharedString::from("the");
    let the_type = Arc::new(Expr::Pi(
        loc.clone(),
        vec![
            (
                "T".into(),
                Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type)),
            ),
            (
                "x".into(),
                Arc::new(Expr::LocalVar(loc.clone(), "T".into())),
            ),
        ],
        Arc::new(Expr::LocalVar(loc.clone(), "T".into())),
        Effects::default(),
    ));
    let the_expr = Arc::new(Expr::Lam(
        loc.clone(),
        vec!["T".into(), "x".into()],
        vec![(
            None,
            Arc::new(Expr::LocalVar(loc.clone(), "T".into())),
            Arc::new(Expr::LocalVar(loc.clone(), "x".into())),
            Effects::default(),
        )],
    ));

    let the_type_name = SharedString::from("the-type");
    let the_type_type = Arc::new(Expr::Pi(
        loc.clone(),
        vec![(
            "T".into(),
            Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type)),
        )],
        Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type)),
        Effects::default(),
    ));
    let the_type_expr = Arc::new(Expr::Lam(
        loc.clone(),
        vec!["T".into()],
        vec![(
            None,
            Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type)),
            Arc::new(Expr::LocalVar(loc.clone(), "T".into())),
            Effects::default(),
        )],
    ));

    let type_name = SharedString::from("type");
    let type_type = Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::TypeOfTypeOfTypes));
    let type_expr = Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type));

    let exports = hashset! {
        fixnum_name.clone(), the_name.clone(), the_type_name.clone(), type_name.clone()
    };

    let mut lib_ctx = ctx.create_lib(lib_name.clone(), hashmap! {});
    lib_ctx
        .insert_mod(Module {
            lib_name,
            mod_name,
            exports: exports.clone(),
            imports: hashmap! {},
            decls: vec![
                Decl::Def(loc.clone(), fixnum_name, fixnum_type, fixnum_expr),
                Decl::Def(loc.clone(), the_name, the_type, the_expr),
                Decl::Def(loc.clone(), the_type_name, the_type_type, the_type_expr),
                Decl::Def(loc, type_name, type_type, type_expr),
            ],
        })
        .unwrap();
    lib_ctx.finish().unwrap();

    exports
}
