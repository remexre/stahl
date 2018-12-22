use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(raw(setting = "::structopt::clap::AppSettings::ColoredHelp"))]
pub struct Options {
    /// Turns off message output.
    #[structopt(short = "q", long = "quiet")]
    pub quiet: bool,

    /// Increases the verbosity.
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    pub verbose: usize,

    /// The command.
    #[structopt(subcommand)]
    pub command: Option<Command>,
}

impl Options {
    /// Sets up logging as specified by the `-q` and `-v` flags.
    pub fn start_logger(&self) {
        stderrlog::new()
            .verbosity(self.verbose + 1)
            .quiet(self.quiet)
            .init()
            .unwrap();
    }

    /// Returns the command, filling in the default if none was present.
    pub fn command(self) -> Command {
        self.command.unwrap_or(Command::Repl)
    }
}

#[derive(Debug, StructOpt)]
pub enum Command {
    /// Compiles a file to a binary.
    #[structopt(name = "build")]
    Build {
        #[structopt(name = "main_file", parse(from_os_str))]
        main: PathBuf,
    },

    /// Starts a REPL.
    #[structopt(name = "repl")]
    Repl,
}
