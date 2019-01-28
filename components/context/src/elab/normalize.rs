use crate::{types::UnifExpr, ModContext};
use log::{debug, error};
use stahl_ast::{Decl, Intrinsic, Literal};
use stahl_errors::Result;
use stahl_util::{unwrap_rc, SharedString};
use std::rc::Rc;

#[derive(Default)]
pub struct NormalizeEnv<'a> {
    pub base: &'a [(SharedString, Rc<UnifExpr>, Option<Rc<UnifExpr>>)],
    pub ext: Vec<(SharedString, Option<Rc<UnifExpr>>)>,
}

impl<'a> NormalizeEnv<'a> {
    pub fn new(base: &'a [(SharedString, Rc<UnifExpr>, Option<Rc<UnifExpr>>)]) -> NormalizeEnv<'a> {
        NormalizeEnv {
            base,
            ext: Vec::new(),
        }
    }

    fn get(&self, n: &str) -> Option<Rc<UnifExpr>> {
        let iter = self.base.iter().rev().map(|(n, _, v)| (n, v));
        let iter = self.ext.iter().rev().map(|(n, v)| (n, v)).chain(iter);
        iter.filter(|(name, _)| name == &n)
            .map(|(_, val)| val.clone())
            .next()
            .unwrap_or_else(|| {
                error!("{} not found", n);
                error!("self.base:");
                for (name, _, _) in self.base.iter() {
                    error!("  {}", name);
                }
                error!("self.ext:");
                for (name, _) in self.ext.iter() {
                    error!("  {}", name);
                }
                panic!("{} not found", n)
            })
    }
}

impl ModContext<'_, '_> {
    /// Normalizes an expression under an evaluation context. Stops when the expression is normal,
    /// or when an error is encountered.
    pub(crate) fn normalize(&self, expr: &mut Rc<UnifExpr>, env: Option<&mut NormalizeEnv>) {
        let mut default_env = NormalizeEnv {
            base: &[],
            ext: Vec::new(),
        };
        let env = env.unwrap_or(&mut default_env);
        loop {
            if expr.is_normal(env) {
                debug!("Fully normalized: {}", expr);
                break;
            }
            match self.normalize_step((**expr).clone(), env) {
                Ok(e) => *Rc::make_mut(expr) = e,
                Err(err) => {
                    debug!("Halting normalization: {}", err);
                    break;
                }
            }
        }
    }

    /// Normalizes the given expression by a single step.
    fn normalize_step(&self, expr: UnifExpr, env: &mut NormalizeEnv) -> Result<UnifExpr> {
        match expr {
            UnifExpr::Call(loc, mut func, mut call_args) => {
                self.normalize(&mut func, Some(env));
                for args in call_args.iter_mut() {
                    self.normalize(args, Some(env));
                }

                match *func {
                    UnifExpr::Atom(_, _, _) => Ok(UnifExpr::Call(loc, func, call_args)),
                    UnifExpr::Lam(_, ref args, ref body) => {
                        if args.len() != call_args.len() {
                            raise!(@loc.clone(), "{} is being passed {} args but expects {}",
                                func, call_args.len(), args.len());
                        }

                        let mut body = body.clone();
                        let mut last = body.pop().expect("An empty lambda slipped in").2;

                        assert_eq!(body.len(), 0); // TODO handle lambdas with bodies

                        for i in 0..args.len() {
                            replace(&args[i], &mut last, call_args[i].clone());
                        }

                        self.normalize(&mut last, Some(env));
                        Ok(unwrap_rc(last))
                    }
                    UnifExpr::Intrinsic(_, Intrinsic::FixnumAdd) => match &*call_args {
                        [l, r] => match (&**l, &**r) {
                            (
                                UnifExpr::Const(_, Literal::Int(_, l)),
                                UnifExpr::Const(_, Literal::Int(_, r)),
                            ) => Ok(UnifExpr::Const(loc.clone(), Literal::Int(loc, l + r))),
                            _ => raise!(@loc.clone(), "Type error in call to +"),
                        },
                        _ => raise!(@loc.clone(), "Invalid argn in call to +"),
                    },
                    _ => raise!(@loc.clone(), "{} is not callable", func),
                }
            }
            UnifExpr::GlobalVar(loc, name) => match self.get_decl(name.clone()) {
                Some((_, Decl::Def(_, _, _, expr))) => Ok((&**expr).into()),
                Some((_, Decl::DefEff(_, _, _, _))) => {
                    raise!(@loc.clone(), "{} is an effect, not a value", name)
                }
                Some((_, Decl::DefEffSet(_, _, _))) => {
                    raise!(@loc.clone(), "{} is an effect set, not a value", name)
                }
                None => raise!(@loc.clone(), "{} is not defined", name),
            },
            UnifExpr::LocalVar(loc, name) => env
                .get(&name)
                .map(unwrap_rc)
                .ok_or_else(|| err!(@loc.clone(), "{} has no value", name)),
            UnifExpr::Pi(loc, mut args, mut body, effs) => {
                let old_env_len = env.ext.len();
                for (ref name, ref mut arg) in &mut args {
                    self.normalize(arg, Some(env));
                    env.ext.push((name.clone(), None));
                }
                self.normalize(&mut body, Some(env));
                env.ext.truncate(old_env_len);
                Ok(UnifExpr::Pi(loc, args, body, effs))
            }
            expr => panic!("{} is already normal!", expr),
        }
    }
}

impl UnifExpr {
    /// Checks if an expression is normal under an evaluation context.
    pub fn is_normal(&self, env: &mut NormalizeEnv) -> bool {
        match self {
            UnifExpr::Atom(_, _, _) => true,
            UnifExpr::Call(_, func, args) => {
                if args.iter().all(|arg| arg.is_normal(env)) {
                    match **func {
                        UnifExpr::Atom(_, _, _) => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }
            UnifExpr::Const(_, _) => true,
            UnifExpr::GlobalVar(_, _) => false,
            UnifExpr::LocalVar(_, name) => env.get(name).is_none(),
            UnifExpr::Intrinsic(_, _) => true,
            UnifExpr::Lam(_, _, _) => true,
            UnifExpr::Pi(_, args, body, _) => {
                let mut normal = true;
                let old_env_len = env.ext.len();
                for (name, arg) in args {
                    normal &= arg.is_normal(env);
                    env.ext.push((name.clone(), None));
                }
                normal &= body.is_normal(env);
                env.ext.truncate(old_env_len);
                normal
            }
            UnifExpr::RecMatch(_, name, cases) => {
                unimplemented!();
            }
            UnifExpr::UnifVar(_, _) => true,
        }
    }
}

// TODO: Isn't this just UnifExpr::beta?
fn replace(name: &str, expr: &mut Rc<UnifExpr>, to: Rc<UnifExpr>) {
    let expr_ref = Rc::make_mut(expr);
    match expr_ref {
        UnifExpr::Call(_, func, args) => {
            for arg in args.iter_mut() {
                replace(name, arg, to.clone());
            }
            replace(name, func, to);
        }
        UnifExpr::LocalVar(_, ref n) if name == n.as_str() => *expr = to,
        UnifExpr::Lam(_, args, body) => {
            if !args.iter().any(|a| a == name) {
                for (def_name, def_ty, expr, _effs) in body.iter_mut() {
                    replace(name, def_ty, to.clone());
                    replace(name, expr, to.clone());
                    if let Some(def_name) = def_name {
                        if name == def_name {
                            return;
                        }
                    }
                }
            }
        }
        UnifExpr::Pi(_, args, body, _) => {
            for (n, arg) in args.iter_mut() {
                replace(name, arg, to.clone());
                if name == n {
                    return;
                }
            }
            replace(name, body, to)
        }
        UnifExpr::RecMatch(_, n, cases) => {
            if name == n {
                return;
            }
            for (ctor, expr) in cases.iter_mut() {
                replace(name, expr, to.clone());
            }
        }
        UnifExpr::Atom(_, _, _)
        | UnifExpr::Const(_, _)
        | UnifExpr::GlobalVar(_, _)
        | UnifExpr::Intrinsic(_, _)
        | UnifExpr::LocalVar(_, _)
        | UnifExpr::UnifVar(_, _) => {}
    }
}
