use stahl_ast::{Effects, Expr, FQName, Intrinsic, LibName, TagKind};
use stahl_errors::Location;
use stahl_util::SharedString;
use std::sync::Arc;

/// Creates the type of the eliminator for the algebraic data type.
pub fn elim_type(
    lib_name: LibName,
    mod_name: SharedString,
    loc: Location,
    name: SharedString,
    ty_args: Vec<(Option<SharedString>, Arc<Expr>)>,
    ctors: Vec<(SharedString, Vec<(SharedString, Arc<Expr>)>, Vec<Arc<Expr>>)>,
) -> Arc<Expr> {
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
        Arc::new(Expr::LocalVar(loc.clone(), name))
    } else {
        Arc::new(Expr::Call(
            loc.clone(),
            Arc::new(Expr::LocalVar(loc.clone(), name)),
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

    let ctor_cases = ctors.into_iter().map(|(name, ctor_args, ret_args)| {
        let ctor_case_name = SharedString::gensym();
        let ty = if ctor_args.is_empty() {
            Arc::new(Expr::Call(
                loc.clone(),
                Arc::new(Expr::LocalVar(loc.clone(), motive_name.clone())),
                vec![Arc::new(Expr::Intrinsic(
                    loc.clone(),
                    Intrinsic::Tag(
                        FQName(lib_name.clone(), mod_name.clone(), name.clone()),
                        TagKind::Ctor,
                    ),
                ))],
            ))
        } else {
            Arc::new(Expr::Intrinsic(
                loc.clone(),
                Intrinsic::Tag(
                    FQName(LibName("TODO".into(), 0, 0, 0), "".into(), "TODO".into()),
                    TagKind::Ctor,
                ),
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
