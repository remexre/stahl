use stahl_parser::lexer::Lexer;
use std::{env::args, fs::File, io::Read};

fn main() {
    let args = args().collect::<Vec<String>>();
    if args.len() != 2 {
        panic!("Usage: test_lexer filename");
    }

    let mut src = String::new();
    File::open(&args[1])
        .unwrap()
        .read_to_string(&mut src)
        .unwrap();
    let toks = Lexer::new(&src).collect::<Result<Vec<_>, _>>().unwrap();
    for (_, tok, _) in toks {
        println!("{:?}", tok);
    }
}
