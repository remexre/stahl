#[macro_use]
extern crate log;
#[macro_use]
extern crate stahl_errors;
#[macro_use]
extern crate structopt;

mod options;
mod repl;
mod script;
#[cfg(test)]
mod tests;

use crate::options::{Command, Options};
use stahl_context::Context;
use stahl_errors::Result;
use std::{env, process::exit};
use structopt::StructOpt;

fn main() {
    let options = Options::from_args();
    options.start_logger();

    let mut search_paths = Vec::new();
    if !options.disable_built_in_search_paths {
        if let Ok(mut path) = env::current_exe() {
            // Yeah, gross side effects... pop returns false on failure.
            let not_too_close_to_root = path.pop() && path.pop();
            if not_too_close_to_root {
                path.push("lib");
                search_paths.push(path.into());
            }
        }

        if let Some(path) = env::var_os("STAHL_PATH") {
            for path in env::split_paths(&path) {
                search_paths.push(path.into());
            }
        }
    }
    search_paths.extend(options.search_paths);

    let ctx = Context::new(search_paths);
    if let Err(err) = run(options.command.unwrap_or(Command::Repl), ctx) {
        error!("{}", err);
        exit(1);
    }
}

fn run(command: Command, ctx: Context) -> Result<()> {
    match command {
        Command::Repl => repl::run(ctx),
        Command::Script { main, args } => script::run(ctx, main, args),
    }
}
