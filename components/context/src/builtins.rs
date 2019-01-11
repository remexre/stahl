use crate::Context;
use maplit::{hashmap, hashset};
use stahl_ast::{Decl, Effects, Expr, Intrinsic, LibName};
use stahl_errors::{Location, Result};
use stahl_util::SharedString;
use std::sync::Arc;

macro_rules! builtin {
    ($lib:ident $(:$mod:ident)*, { $($k:tt $name:tt : $ty:tt = $val:expr;)* }) => {{
        let mut first = true;
        let mut mod_name = String::new();
        for s in &[$(stringify!($mod)),*] as &[&str] {
            if first {
                first = false;
            } else {
                mod_name += ":";
            }
            mod_name += s;
        }
        let mut mod_ctx = $lib.create_mod(mod_name.into(), builtin!(@exports $($name),*), hashmap!{})
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

/// Creates a function call.
macro_rules! call {
    ($func:expr $(,$arg:expr)* $(,)*) => { Arc::new(Expr::Call(loc!(), $func, vec![$($arg),*])) };
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

/// Creates the `compiler-builtins` library and adds it to the given context. If `load_std` is
/// `true`, will also load it.
pub fn add_to(ctx: &mut Context, load_std: bool) -> Result<()> {
    let lib_name = LibName("compiler-builtins".into(), 0, 0, 0);
    let mut lib_ctx = ctx.create_lib(lib_name.clone(), hashmap! {}, None);

    builtin!(lib_ctx, {
        def "Fixnum" : (intr!(Type)) = intr!(Fixnum);
        def "fixnum-add" : (arrow!((intr!(Fixnum), intr!(Fixnum)) => intr!(Fixnum))) = intr!(FixnumAdd);
        def "Eq" : (pi!((T: intr!(Type), x: var!(T), y: var!(T)) => intr!(Type))) = intr!(Eq);
        def "refl" : (pi!((T: intr!(Type), x: var!(T)) => call!(intr!(Eq), var!(T), var!(x), var!(x)))) = intr!(Refl);
        def "String" : (intr!(Type)) = intr!(String);
        def "Symbol" : (intr!(Type)) = intr!(Symbol);
        def "TYPE" : (intr!(TypeOfType)) = intr!(Type);
    });

    builtin!(lib_ctx:unsafe, {
        def "bogus" : (pi!((T: intr!(Type), U: intr!(Type), x: var!(T)) => var!(U))) =
            lam!((T, U, x) : var!(U) => var!(x));
    });

    lib_ctx.finish().unwrap();

    if load_std {
        let name = ctx.load_lib_highest_version("std".into())?;
        ctx.std = Some(name);
    }

    Ok(())
}
