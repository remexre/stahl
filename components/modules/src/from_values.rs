use crate::Module;
use stahl_cst::Decl;
use stahl_errors::{Location, Result};
use stahl_parser::Value;
use stahl_util::SharedString;
use std::collections::HashMap;

impl Module {
    /// Parses a module from values.
    pub fn from_values(
        mut vals: Vec<Value>,
        loc: Location,
    ) -> Result<(
        SharedString,
        Vec<SharedString>,
        HashMap<SharedString, HashMap<SharedString, Vec<SharedString>>>,
        Vec<Decl>,
    )> {
        let mut vals = vals.drain(..);

        let (mod_name, exports) = match vals.next() {
            Some(module_form) => parse_module_form(&module_form).ok_or_else(
                || err!(@module_form.loc(), "{} is not a valid module form", module_form),
            )?,
            None => raise!(@loc, "Can't have an empty module"),
        };

        let mut imports = HashMap::new();
        let mut decls = Vec::new();
        let mut processing_imports = true;
        for val in vals {
            if processing_imports {
                if let Some((implib, imps)) = parse_import_form(&val)? {
                    if imports.contains_key(&implib) {
                        raise!(@val.loc(), "Duplicate import form for {}", implib)
                    }
                    imports.insert(implib, imps);
                } else {
                    processing_imports = false;
                    decls.push(Decl::from_value(&val)?);
                }
            } else {
                decls.push(Decl::from_value(&val)?);
            }
        }

        Ok((mod_name, exports, imports, decls))
    }
}

fn parse_module_form(val: &Value) -> Option<(SharedString, Vec<SharedString>)> {
    if let Value::Cons(_, h, t) = val {
        if let (&Value::Symbol(_, ref module), &Value::Cons(_, ref h, ref t)) = (&**h, &**t) {
            if module != "module" {
                None
            } else if let Value::Symbol(_, ref name) = **h {
                let exps = t.as_sym_list()?;
                Some((name.clone(), exps))
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_import_form(
    val: &Value,
) -> Result<Option<(SharedString, HashMap<SharedString, Vec<SharedString>>)>> {
    if let Value::Cons(_, h, t) = val {
        if let (&Value::Symbol(_, ref import), &Value::Cons(_, ref h, ref t)) = (&**h, &**t) {
            if import != "import" {
                Ok(None)
            } else if let Value::Symbol(_, ref name) = **h {
                let mut imps = HashMap::new();
                let (imp_vals, t) = t.clone().as_list();
                if let Value::Nil(_) = t {
                    for imp_val in imp_vals {
                        let (impmod, impvals) = parse_imp_clause(&imp_val)?;
                        if imps.contains_key(&impmod) {
                            raise!(@val.loc(), "Duplicate module import in import form {}", val)
                        }
                        imps.insert(impmod, impvals);
                    }
                    Ok(Some((name.clone(), imps)))
                } else {
                    raise!(@val.loc(), "Import form {} must not be an improper list", val)
                }
            } else {
                raise!(@val.loc(), "Invalid library name {} in import form {}", h, val)
            }
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

fn parse_imp_clause(val: &Value) -> Result<(SharedString, Vec<SharedString>)> {
    let mut syms = val.as_sym_list().ok_or_else(
        || err!(@val.loc(), "Module clause {} of import form must be a list of symbols", val),
    )?;

    if syms.is_empty() {
        raise!(@val.loc(), "Nil is not a legal module clause")
    }

    let impmod = syms.remove(0);
    Ok((impmod, syms))
}
