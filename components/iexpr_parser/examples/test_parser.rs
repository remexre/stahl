use stahl_iexpr_parser::parse_file;
use std::{env::args, path::PathBuf};

fn main() {
    let args = args().collect::<Vec<String>>();
    if args.len() != 2 {
        panic!("Usage: test_parser filename");
    }

    let path: PathBuf = (&args[1]).into();
    let vals = parse_file(path.into()).unwrap();
    for val in vals {
        println!("{}", val);
    }
}
