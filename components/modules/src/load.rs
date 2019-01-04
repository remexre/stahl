use stahl_ast::LibName;
use stahl_cst::Value;
use stahl_errors::{Location, Result};
use stahl_parser::parse_file;
use stahl_util::{SharedPath, SharedString};
use std::{collections::HashMap, u16, u32};

/// Finds the `lib.stahld` for the given library name in the search path.
pub fn load_lib_stahld(
    name: LibName,
    search_paths: &[SharedPath],
) -> Result<(SharedPath, HashMap<SharedString, LibName>)> {
    search_paths
        .iter()
        .map(|search_path| search_path.join(&*name.0).join("lib.stahld"))
        .map(SharedPath::from)
        .filter_map(|path| {
            let r = parse_file(path.clone())
                .and_then(|vals| lib_stahld_from_values(vals, Location::new().path(path.clone())));
            match r {
                Ok((ref n, _)) if n != &name => None,
                Ok((_, deps)) => Some(Ok((path, deps))),
                Err(err) => Some(Err(err)),
            }
        })
        .next()
        .unwrap_or_else(|| raise!("Couldn't find library {}", name))
}

/// Parses a `lib.stahld` file from values.
fn lib_stahld_from_values(
    vals: Vec<Value>,
    loc: Location,
) -> Result<(LibName, HashMap<SharedString, LibName>)> {
    let mut name = None;
    let mut version = None;
    let mut deps = None;

    for val in vals {
        let loc = val.loc();
        if let Some((form_name, mut vals)) = sym_headed_list(val.clone()) {
            match &*form_name {
                "name" if vals.len() == 1 => match vals.pop().unwrap() {
                    Value::Symbol(_, s) => {
                        if name.is_some() {
                            raise!(@loc, "Duplicate name form");
                        }
                        name = Some(s);
                    }
                    val => raise!(@val.loc(), "Invalid library name: {}", val),
                },
                "version" if vals.len() == 1 => {
                    let val = vals.pop().unwrap();
                    match version_from_value(val.clone()) {
                        Some(v) => {
                            if version.is_some() {
                                raise!(@loc, "Duplicate version form");
                            }
                            version = Some(v);
                        }
                        None => raise!(@val.loc(), "Invalid version: {}", val),
                    }
                }
                "deps" => unimplemented!(),
                _ => raise!(@loc, "Invalid form: {}", val),
            }
        } else {
            raise!(@loc, "Invalid form: {}", val);
        }
    }

    let name = name.ok_or_else(|| err!(@loc.clone(), "No name was specified"))?;
    let version = version.ok_or_else(|| err!(@loc, "No name was specified"))?;
    let deps = deps.unwrap_or_else(HashMap::new);
    Ok((LibName(name, version.0, version.1, version.2), deps))
}

fn sym_headed_list(val: Value) -> Option<(SharedString, Vec<Value>)> {
    match val {
        Value::Cons(_, h, t) => match *h {
            Value::Symbol(_, s) => {
                let (l, t) = t.as_list();
                if let Value::Nil(_) = t {
                    Some((s, l))
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn version_from_value(val: Value) -> Option<(u16, u16, u32)> {
    let (mut l, t) = val.as_list();
    if let Value::Nil(_) = t {
        if l.len() == 3 {
            let patch = l.pop().unwrap();
            let minor = l.pop().unwrap();
            let major = l.pop().unwrap();
            match (major, minor, patch) {
                (Value::Int(_, major), Value::Int(_, minor), Value::Int(_, patch)) => {
                    if 0 <= major
                        && major <= u16::MAX as isize
                        && 0 <= minor
                        && minor <= u16::MAX as isize
                        && 0 <= patch
                        && patch <= u32::MAX as isize
                    {
                        Some((major as u16, minor as u16, patch as u32))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } else {
            None
        }
    } else {
        None
    }
}
