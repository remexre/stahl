use crate::{Decl, Effect, Effects, Expr};
use stahl_errors::{Location, Result, ResultExt};
use stahl_parser::Value;
use stahl_util::SharedString;
use std::sync::Arc;

impl Decl {
    /// Parses the declaration from a value.
    pub fn from_value(val: Value) -> Result<Decl> {
        let (loc, decl_type, t) = if let Value::Cons(loc, h, t) = val {
            let decl_type = if let Value::Symbol(_, s) = *h {
                s
            } else {
                raise!(@loc, "{} is not a valid declaration type", h)
            };
            (loc, decl_type, *t)
        } else {
            raise!(@val.loc(), "Non-cons value {} is not a declaration", val)
        };

        match decl_type.as_ref() {
            "def" => match t {
                Value::Cons(_, h, t) => match (*h, *t) {
                    (Value::Symbol(_, name), Value::Cons(_, e1, t)) => match *t {
                        Value::Cons(_, e2, t) => match *t {
                            Value::Nil(_) => Expr::from_value_unnamed(&e1, "a def's type")
                                .and_then(|ty| {
                                    Expr::from_value_unnamed(&e2, "a def's body").map(|expr| {
                                        Decl::Def(
                                            loc.clone(),
                                            name,
                                            Arc::new(Expr::Hole(loc)),
                                            expr,
                                        )
                                    })
                                }),
                            _ => raise!(@loc, "A def must have two or three arguments"),
                        },
                        Value::Nil(_) => {
                            Expr::from_value_unnamed(&e1, "a def's body").map(|expr| {
                                Decl::Def(loc.clone(), name, Arc::new(Expr::Hole(loc)), expr)
                            })
                        }
                        _ => raise!(@loc, "A def must have two or three arguments"),
                    },
                    (Value::Symbol(_, _), _) => {
                        raise!(@loc, "A def must have two or three arguments")
                    }
                    _ => raise!(@loc, "A def's name must be a symbol"),
                },
                _ => raise!(@loc, "A def must have two or three arguments"),
            },
            "defeff" => Effect::from_value(&t)
                .map(|eff| Decl::DefEff(loc, eff))
                .chain(|| err!(@t.loc(), "Invalid defeff")),
            decl_type => raise!(@loc, "{} is not a declaration type", decl_type),
        }
    }
}

impl Effect {
    /// Parses the effect specification from a value.
    pub fn from_value(val: &Value) -> Result<Effect> {
        match val {
            Value::Cons(_, h, t) => match (&**h, &**t) {
                (Value::Symbol(_, s), Value::Cons(_, e1, t)) => {
                    let s = s.clone();
                    let e1 = Expr::from_value_unnamed(e1, "an effect")?;
                    match &**t {
                        Value::Cons(_, e2, t) => {
                            let e2 = Expr::from_value_unnamed(e2, "an effect")?;
                            match &**t {
                                Value::Nil(_) => Ok(Effect(s, e1, Some(e2))),
                                _ => raise!(@val.loc(), "Invalid effect: {}", val),
                            }
                        }
                        Value::Nil(_) => Ok(Effect(s, e1, None)),
                        _ => raise!(@val.loc(), "Invalid effect: {}", val),
                    }
                }
                _ => raise!(@val.loc(), "Invalid effect: {}", val),
            },
            _ => raise!(@val.loc(), "Invalid effect: {}", val),
        }
    }
}

impl Effects {
    /// Parses the effect set specification from a value.
    pub fn from_value(val: &Value) -> Result<Effects> {
        match val {
            Value::Cons(_, h, t) => {
                let eff = Effect::from_value(h)?;
                let mut effs = Effects::from_value(t)
                    .chain(|| err!(@val.loc(), "Invalid effect set: {}", val))?;
                effs.0.push(eff);
                Ok(effs)
            }
            Value::Nil(_) => Ok(Effects(Vec::new())),
            _ => raise!(@val.loc(), "Invalid effect set: {}", val),
        }
    }
}

fn as_pi_arg(val: &Value) -> Result<(SharedString, Arc<Expr>)> {
    match val {
        Value::Cons(_, h, t) => match (&**h, &**t) {
            (Value::Symbol(_, s), Value::Cons(_, e, t)) => {
                let s = s.clone();
                let e = Expr::from_value_unnamed(e, "a pi type argument")?;
                match &**t {
                    Value::Nil(_) => Ok((s, e)),
                    _ => raise!(@val.loc(), "Invalid pi type argument: {}", val),
                }
            }
            _ => raise!(@val.loc(), "Invalid pi type argument: {}", val),
        },
        _ => raise!(@val.loc(), "Invalid pi type argument: {}", val),
    }
}

fn as_pi_arg_list(val: &Value) -> Result<Vec<(SharedString, Arc<Expr>)>> {
    match val {
        Value::Cons(_, h, t) => {
            let arg = as_pi_arg(h)?;
            let mut t = as_pi_arg_list(t)?;
            t.insert(0, arg);
            Ok(t)
        }
        _ => Ok(Vec::new()),
    }
}

impl Expr {
    /// Parses the expression from a value, allowing a `def`.
    pub fn from_value(
        val: &Value,
    ) -> Result<(Option<(SharedString, Option<Arc<Expr>>)>, Arc<Expr>)> {
        match val {
            Value::Cons(loc, _, _) => {
                let (l, t) = val.clone().as_list();
                if let Value::Nil(_) = t {
                    Expr::from_values(l, loc.clone())
                } else {
                    raise!(@val.loc(), "Improper list {} is not an expression", val)
                }
            }
            Value::Int(_, _) | Value::String(_, _) => {
                Ok((None, Arc::new(Expr::Const(val.loc(), val.clone()))))
            }
            Value::Symbol(loc, s) => {
                let name = s.as_ref();
                if name == "_" {
                    Ok((None, Arc::new(Expr::Hole(loc.clone()))))
                } else if name == "def" || name == "fn" || name == "pi" || name == "quote" {
                    raise!(@loc.clone(), "{} is not a legal variable name", name)
                } else {
                    Ok((None, Arc::new(Expr::Var(loc.clone(), s.clone()))))
                }
            }
            Value::Nil(loc) => raise!(@loc.clone(), "Nil is not an expression"),
        }
    }

    /// Parses the expression from a value, erroring on a `def`.
    pub fn from_value_unnamed(val: &Value, s: &str) -> Result<Arc<Expr>> {
        let (def_info, expr) = Expr::from_value(val)?;
        if def_info.is_some() {
            raise!(@val.loc(), "{} is not legal in {}", val, s)
        }
        Ok(expr)
    }

    /// Parses the expression from a list of values.
    pub fn from_values(
        mut vals: Vec<Value>,
        loc: Location,
    ) -> Result<(Option<(SharedString, Option<Arc<Expr>>)>, Arc<Expr>)> {
        // Theoretically, this is impossible anyway.
        ensure_ne!(vals.len(), 0, @loc);
        let head = vals.remove(0);
        if let Value::Symbol(_, ref s) = head {
            match &**s {
                "def" => {
                    if vals.len() == 2 || vals.len() == 3 {
                        let name = if let Value::Symbol(_, s) = vals.remove(0) {
                            let name = s.as_ref();
                            if name == "def" || name == "fn" || name == "pi" || name == "quote" {
                                raise!(@loc.clone(), "{} is not a legal variable name", name)
                            } else {
                                s
                            }
                        } else {
                            raise!(@loc, "A def's name must be a symbol")
                        };
                        if vals.len() == 1 {
                            let expr = Expr::from_value_unnamed(&vals[0], "a def's body")?;
                            return Ok((Some((name, None /* TODO */)), expr));
                        } else {
                            let ty = Expr::from_value_unnamed(&vals[0], "a def's type")?;
                            let expr = Expr::from_value_unnamed(&vals[1], "a def's body")?;
                            return Ok((Some((name, Some(ty))), expr));
                        }
                    } else {
                        raise!(@loc, "A def must have two arguments")
                    }
                }
                "fn" => {
                    if vals.len() > 1 {
                        let args = vals.remove(0);
                        let args = args.as_sym_list().ok_or_else(
                            || err!(@args.loc(), "An argument list must be composed of symbols"),
                        )?;
                        let body = vals
                            .iter()
                            .map(Expr::from_value)
                            .collect::<Result<Vec<_>>>()?;
                        return Ok((None, Arc::new(Expr::Lam(loc, args, body))));
                    } else {
                        raise!(@loc, "A fn must have arguments and a non-empty body")
                    }
                }
                "pi" => {
                    if vals.len() == 2 || vals.len() == 3 {
                        let args = as_pi_arg_list(&vals[0])?;
                        let ret = Expr::from_value_unnamed(&vals[1], "a pi return type")?;
                        let effs = vals
                            .get(2)
                            .map(Effects::from_value)
                            .unwrap_or_else(|| Ok(Effects::default()))?;
                        return Ok((None, Arc::new(Expr::Pi(loc, args, ret, effs))));
                    } else {
                        raise!(@loc, concat!("A pi expression must have arguments, a return, and ",
                                             "optionally an effect set"))
                    }
                }
                "quote" => {
                    if vals.len() == 1 {
                        let val = vals.remove(0);
                        return Ok((None, Arc::new(Expr::Const(loc, val))));
                    } else {
                        raise!(@loc, "A quote must have a value")
                    }
                }
                _ => {}
            }
        }

        Ok((
            None,
            Arc::new(Expr::Call(
                loc,
                Expr::from_value_unnamed(&head, "a call")?,
                vals.iter()
                    .map(|expr| Expr::from_value_unnamed(expr, "a call"))
                    .collect::<Result<_>>()?,
            )),
        ))
    }
}
