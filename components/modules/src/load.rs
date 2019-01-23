use stahl_ast::LibName;
use stahl_errors::{Location, Result};
use stahl_iexpr_parser::parse_file;
use stahl_util::{SharedPath, SharedString};
use stahl_value::Value;
use std::{
    collections::HashMap,
    io::{Error as IoError, ErrorKind as IoErrorKind},
    u16, u32,
};

/// Returns an iterator over `lib.stahld` contents for the given library name.
pub fn lib_stald_iter<'a>(
    name: &'a str,
    search_paths: &'a [SharedPath],
) -> impl 'a + Iterator<Item = (SharedPath, LibName, HashMap<SharedString, LibName>)> {
    search_paths
        .iter()
        .map(move |search_path| search_path.join(name).join("lib.stahld"))
        .map(SharedPath::from)
        .map(|path| {
            parse_file(path.clone())
                .and_then(|vals| lib_stahld_from_values(vals, Location::new().path(path.clone())))
                .map(|(lib, deps)| {
                    let path = path.parent().unwrap().to_owned();
                    (path.into(), lib, deps)
                })
                .map_err(|mut err| {
                    err.loc.path = Some(path.clone());
                    err
                })
        })
        .filter_map(move |r| match r {
            Ok(x) => Some(x),
            Err(e) => {
                if e.err
                    .downcast_ref()
                    .map(|e: &IoError| e.kind() != IoErrorKind::NotFound)
                    .unwrap_or(true)
                {
                    error!("When loading {}, found an invalid lib.stahld: {}", name, e);
                }
                None
            }
        })
}

/// Parses a `lib.stahld` file from values.
fn lib_stahld_from_values(
    vals: Vec<Value>,
    loc: Location,
) -> Result<(LibName, HashMap<SharedString, LibName>)> {
    let mut name = None;
    let mut description = None;
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
                "description" if vals.len() == 1 => match vals.pop().unwrap() {
                    Value::String(_, s) => {
                        if description.is_some() {
                            raise!(@loc, "Duplicate description form");
                        }
                        description = Some(s);
                    }
                    val => raise!(@val.loc(), "Invalid library description: {}", val),
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
                "deps" => {
                    if deps.is_some() {
                        raise!(@loc, "Duplicate deps form");
                    }
                    let mut dep_map = HashMap::new();
                    for dep_form in vals {
                        let name = sym_headed_list(dep_form)
                            .and_then(|(name, mut vals)| {
                                if vals.len() == 1 {
                                    version_from_value(vals.pop().unwrap()).map(
                                        |(major, minor, patch)| LibName(name, major, minor, patch),
                                    )
                                } else {
                                    None
                                }
                            })
                            .ok_or_else(|| err!(@loc.clone(), "Invalid deps form"))?;
                        if dep_map.contains_key(&name.0) {
                            raise!(@loc.clone(), "Duplicate dependency on {}", name.0);
                        }
                        dep_map.insert(name.0.clone(), name);
                    }
                    deps = Some(dep_map);
                }
                _ => raise!(@loc, "Invalid form: {}", val),
            }
        } else {
            raise!(@loc, "Invalid form: {}", val);
        }
    }

    let name = name.ok_or_else(|| err!(@loc.clone(), "No name was specified"))?;
    let version = version.ok_or_else(|| err!(@loc, "No version was specified"))?;
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
