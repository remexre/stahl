use stahl_iexpr_parser::parse_file;
use std::{
    fs::read_dir,
    io::Result,
    path::{Path, PathBuf},
};

#[test]
fn parse_all_in_repo() {
    let paths = list_all("../..".as_ref()).unwrap();
    let mut had_err = false;
    for path in paths {
        if let Err(err) = parse_file(path.into()) {
            eprintln!("{}", err);
            had_err = true;
        }
    }
    assert!(!had_err);
}

fn list_all(base_path: &Path) -> Result<Vec<PathBuf>> {
    fn helper(dir: &Path, paths: &mut Vec<PathBuf>) -> Result<()> {
        for entry in read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if entry.file_type()?.is_dir() {
                helper(&path, paths)?;
            } else if path.extension() == Some("stahl".as_ref()) {
                paths.push(path);
            }
        }
        Ok(())
    }

    let mut paths = Vec::new();
    helper(base_path, &mut paths)?;
    Ok(paths)
}
