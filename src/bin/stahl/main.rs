#[macro_use]
extern crate log;
#[macro_use]
extern crate structopt;

mod options;
mod repl;
#[cfg(test)]
mod tests;

use crate::options::{Command, Options};
use stahl_context::Context;
use stahl_errors::Result;
use stahl_util::SharedPath;
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
                path.push("libs");
                search_paths.push(path.into());
            }
        }

        if let Some(path) = env::var_os("STAHL_PATH") {
            for path in env::split_paths(&path) {
                search_paths.push(path.into());
            }
        }
    }
    search_paths.extend(options.search_paths.iter().cloned().map(SharedPath::from));

    let r = Context::new(true, search_paths)
        .and_then(|mut ctx| run(options.command.unwrap_or(Command::Repl), &mut ctx));
    if let Err(err) = r {
        error!("{}", err);
        exit(1);
    }
}

fn run(command: Command, ctx: &mut Context) -> Result<()> {
    match command {
        Command::Interpret { main, args } => stahl::run_package(ctx, main, args),
        Command::Repl => repl::run(ctx),
        Command::Script { main, args } => stahl::run_script(ctx, main, args),
    }
}
