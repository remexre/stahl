use stahl_ast::{Effects, Expr, FQName, Intrinsic, LibName};
use stahl_errors::Location;
use stahl_util::SharedString;
use std::sync::Arc;

type Ctor = (
    Location,
    SharedString,
    Vec<(SharedString, Arc<Expr>)>,
    Vec<Arc<Expr>>,
);

/// Creates the type of a constructor.
pub fn ctor_type(ty_name: FQName, tuple: &Ctor) -> Arc<Expr> {
    let (loc, _name, ctor_args, ret_args) = tuple;

    let base = if ret_args.is_empty() {
        Arc::new(Expr::GlobalVar(loc.clone(), ty_name))
    } else {
        Arc::new(Expr::Call(
            loc.clone(),
            Arc::new(Expr::GlobalVar(loc.clone(), ty_name)),
            ret_args.clone(),
        ))
    };
    if ctor_args.is_empty() {
        base
    } else {
        Arc::new(Expr::Pi(
            loc.clone(),
            ctor_args.clone(),
            base,
            Effects::default(),
        ))
    }
}

/// Creates the type of the eliminator for the algebraic data type.
pub fn elim_type(
    lib_name: LibName,
    mod_name: SharedString,
    loc: Location,
    name: SharedString,
    ty_args: Vec<(Option<SharedString>, Arc<Expr>)>,
    ctors: Vec<Ctor>,
) -> Arc<Expr> {
    let type_var = Arc::new(Expr::GlobalVar(
        loc.clone(),
        FQName(lib_name.clone(), mod_name.clone(), name.clone()),
    ));
    let ty_args = ty_args
        .into_iter()
        .map(|(name, ty)| {
            let (name, is_index) = if let Some(name) = name {
                (name, true)
            } else {
                (SharedString::gensym(), false)
            };
            (name, is_index, ty)
        })
        .collect::<Vec<_>>();

    let plain_args = ty_args
        .iter()
        .cloned()
        .filter(|&(_, is_index, _)| !is_index)
        .map(|(name, _, ty)| (name, ty));
    let index_args = ty_args
        .iter()
        .cloned()
        .filter(|&(_, is_index, _)| is_index)
        .map(|(name, _, ty)| (name, ty));

    let val_name = SharedString::gensym();
    let val_ty = if ty_args.is_empty() {
        type_var.clone()
    } else {
        Arc::new(Expr::Call(
            loc.clone(),
            type_var.clone(),
            ty_args
                .iter()
                .map(|(name, _, _)| Arc::new(Expr::LocalVar(loc.clone(), name.clone())))
                .collect(),
        ))
    };

    let motive_name = SharedString::gensym();
    let motive_ty = Arc::new(Expr::Pi(
        loc.clone(),
        vec![(val_name.clone(), val_ty.clone())],
        Arc::new(Expr::Intrinsic(loc.clone(), Intrinsic::Type)),
        Effects::default(),
    ));

    let ctor_cases = ctors.into_iter().map(|(_loc, name, ctor_args, ret_args)| {
        let ctor_case_name = SharedString::gensym();
        let ty = if ctor_args.is_empty() {
            Arc::new(Expr::Call(
                loc.clone(),
                Arc::new(Expr::LocalVar(loc.clone(), motive_name.clone())),
                vec![Arc::new(Expr::GlobalVar(
                    loc.clone(),
                    FQName(lib_name.clone(), mod_name.clone(), name.clone()),
                ))],
            ))
        } else {
            Arc::new(Expr::Atom(
                loc.clone(),
                FQName(LibName("TODO".into(), 0, 0, 0), "".into(), "TODO".into()),
                unimplemented!(),
            ))
        };
        (ctor_case_name, ty)
    });

    let mut pi_args = Vec::new();
    pi_args.extend(plain_args);
    pi_args.push((motive_name.clone(), motive_ty));
    pi_args.extend(ctor_cases);
    pi_args.push((val_name.clone(), val_ty));
    Arc::new(Expr::Pi(
        loc.clone(),
        pi_args,
        Arc::new(Expr::Call(
            loc.clone(),
            Arc::new(Expr::LocalVar(loc.clone(), motive_name)),
            vec![
                // TODO
                Arc::new(Expr::LocalVar(loc, val_name)),
            ],
        )),
        Effects::default(),
    ))
}

/// Creates the value of the eliminator for the algebraic data type.
pub fn elim_value(
    lib_name: LibName,
    mod_name: SharedString,
    loc: Location,
    name: SharedString,
    ty_args: Vec<(Option<SharedString>, Arc<Expr>)>,
    ctors: Vec<Ctor>,
) -> Arc<Expr> {
    unimplemented!()
}
