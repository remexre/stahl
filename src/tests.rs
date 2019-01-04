use maplit::{hashmap, hashset};
use stahl_ast::{Intrinsic, LibName, Literal};
use stahl_context::{Context, ModContext, UnifExpr};
use stahl_errors::Location;
use std::rc::Rc;

/// Runs the given closure with a context for tests.
fn with_context(f: impl FnOnce(&mut ModContext)) {
    let mut ctx = Context::new(Vec::new());

    let mut lib_ctx = ctx.create_lib(
        LibName("test".into(), 0, 0, 0),
        hashmap! {
            "compiler-builtins".into() => LibName("compiler-builtins".into(), 0, 0, 0)
        },
    );

    let mut mod_ctx = lib_ctx
        .create_mod(
            "".into(),
            hashset! {},
            hashmap! {
                "compiler-builtins".into() => hashmap! {
                    "".into() => hashset!{
                        "fixnum".into(),
                        "type".into(),
                    }
                }
            },
        )
        .unwrap();
    f(&mut mod_ctx);
    mod_ctx.finish().unwrap();

    lib_ctx.finish().unwrap();
}

#[test]
fn build_the() {
    with_context(|mod_ctx| {
        let loc = Location::new().name("the definition of `the'".into());
        let mut def_ctx = mod_ctx.create_def(loc.clone(), "the".into());

        let ty = def_ctx.type_zipper();
        ty.intros_pi(loc.clone(), vec!["T".into(), "x".into()]);
        ty.go_to_leftmost_hole();
        ty.fill(Rc::new(UnifExpr::Intrinsic(loc.clone(), Intrinsic::Type)));
        ty.go_to_leftmost_hole();
        ty.fill(Rc::new(UnifExpr::LocalVar(loc.clone(), "T".into())));
        ty.go_to_leftmost_hole();
        ty.fill(Rc::new(UnifExpr::LocalVar(loc.clone(), "T".into())));
        assert!(!ty.go_to_leftmost_hole());

        let expr = def_ctx.expr_zipper();
        expr.intros(loc.clone(), vec!["T".into(), "x".into()]);
        expr.fill(Rc::new(UnifExpr::LocalVar(loc, "x".into())));

        def_ctx.finish().unwrap();
    });
}

#[test]
fn the_fixnum_1() {
    with_context(|mod_ctx| {
        let loc = Location::new();

        let mut def_ctx = mod_ctx.create_def(loc.clone(), "one".into());
        def_ctx.type_zipper().fill(UnifExpr::hole(loc.clone()));
        def_ctx.expr_zipper().fill(Rc::new(UnifExpr::Call(
            loc.clone(),
            Rc::new(UnifExpr::GlobalVar(
                loc.clone(),
                "compiler-builtins-0-0-0:the".parse().unwrap(),
            )),
            vec![
                Rc::new(UnifExpr::Intrinsic(loc.clone(), Intrinsic::Fixnum)),
                Rc::new(UnifExpr::Const(loc.clone(), Literal::Int(loc.clone(), 1))),
            ],
        )));
        def_ctx.finish().unwrap();
    });
}
