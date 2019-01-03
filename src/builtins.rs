use maplit::{hashmap, hashset};
use stahl_ast::{Decl, Effects, Expr, Intrinsic, LibName};
use stahl_context::Context;
use stahl_errors::Location;
use stahl_util::SharedString;
use std::sync::Arc;

macro_rules! builtin {
    ($lib:expr, { $($k:tt $name:tt : $ty:tt = $val:expr;)* }) => {{
        let mut mod_ctx = $lib.create_mod("".into(), builtin!(@exports $($name),*), hashmap!{})
            .unwrap();
        let loc = Location::new().name("compiler builtin".into());
        macro_rules! loc { () => { loc.clone() } }
        $(mod_ctx.add(builtin!(@$k $name, $ty, $val)).unwrap();)*
        mod_ctx.finish().unwrap();
    }};

    (@exports $($name:expr),*) => { hashset! { $($name.into()),* } };
    (@def $name:expr, $ty:expr, $val:expr) => { Decl::Def(loc!(), $name.into(), $ty, $val) };
}

/// Creates an arrow type; i.e. a pi type whose argument names are all gensymmed, with no effects.
macro_rules! arrow {
    (($($arg:expr),* $(,)*) => $ret:expr) => {
        Arc::new(Expr::Pi(
            loc!(),
            vec![$((SharedString::gensym(), $arg)),*],
            $ret,
            Effects::default(),
        ))
    };
}

/// Returns an intrinsic.
macro_rules! intr {
    ($name:ident) => {
        Arc::new(Expr::Intrinsic(loc!(), Intrinsic::$name))
    };
}

/// Creates an lambda.
macro_rules! lam {
    (($($arg:ident),* $(,)*) : $ty:expr => $body:expr) => {
        Arc::new(Expr::Lam(
            loc!(),
            vec![$(stringify!($arg).into()),*],
            vec![(None, $ty, $body, Effects::default())],
        ))
    };
}

/// Creates an pi type with no effects.
macro_rules! pi {
    (($($name:ident : $arg:expr),* $(,)*) => $ret:expr) => {
        Arc::new(Expr::Pi(
            loc!(),
            vec![$((stringify!($name).into(), $arg)),*],
            $ret,
            Effects::default(),
        ))
    };
}

/// Returns an local variable.
macro_rules! var {
    ($name:ident) => {
        Arc::new(Expr::LocalVar(loc!(), stringify!($name).into()))
    };
}

/// Creates the `#compiler-builtins#` library.
pub fn create_compiler_builtins_lib(ctx: &mut Context) {
    let lib_name = LibName("#compiler-builtins#".into(), 0, 0, 0);
    let mut lib_ctx = ctx.create_lib(lib_name.clone(), hashmap! {});

    builtin!(lib_ctx, {
        def "+" : (arrow!((intr!(Fixnum), intr!(Fixnum)) => intr!(Fixnum))) = intr!(FixnumAdd);
        def "bogus" : (pi!((T: intr!(Type), U: intr!(Type), x: var!(T)) => var!(U))) =
            lam!((T, U, x) : var!(U) => var!(x));
        def "fixnum" : (intr!(Type)) = intr!(Fixnum);
        def "string" : (intr!(Type)) = intr!(String);
        def "symbol" : (intr!(Type)) = intr!(Symbol);
        def "type" : (intr!(TypeOfType)) = intr!(Type);
        def "the" : (pi!((T: intr!(Type), x: var!(T)) => var!(T))) = lam!((T, x) : var!(T) => var!(x));
        def "the-type" : (pi!((T: intr!(Type)) => var!(Type))) = lam!((T) : intr!(Type) => var!(T));
    });

    lib_ctx.finish().unwrap();
}
