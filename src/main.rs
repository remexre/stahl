#[macro_use]
extern crate log;
extern crate rustyline;
#[macro_use]
extern crate stahl_errors;
extern crate stahl_modules;
extern crate stahl_parser;
extern crate stderrlog;
#[macro_use]
extern crate structopt;

mod options;
mod repl;
mod script;

use crate::options::{Command, Options};
use stahl_errors::Result;
use std::process::exit;
use structopt::StructOpt;

fn main() {
    let options = Options::from_args();
    options.start_logger();
    if let Err(err) = run(options.command()) {
        error!("{}", err);
        exit(1);
    }
}

fn run(command: Command) -> Result<()> {
    match command {
        Command::Repl => repl::run(),
        Command::Script { main } => script::run(main),
        _ => raise!("TODO {:?}", command),
    }
}
