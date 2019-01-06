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

    /// Additional search paths for packages.
    #[structopt(short = "I", long = "include", parse(from_os_str))]
    pub search_paths: Option<PathBuf>,

    /// Disables the built-in search paths.
    #[structopt(long = "disable-built-in-includes")]
    pub disable_built_in_search_paths: bool,

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
            .modules(vec![
                "stahl",
                "stahl_ast",
                "stahl_context",
                "stahl_cst",
                "stahl_errors",
                "stahl_interpreter",
                "stahl_modules",
                "stahl_parser",
                "stahl_util",
            ])
            .init()
            .unwrap();
    }
}

#[derive(Debug, StructOpt)]
pub enum Command {
    /// Runs a package's main function.
    #[structopt(name = "interpret")]
    Interpret {
        /// The file being interpreted.
        #[structopt(name = "main_package")]
        main: String,

        /// The arguments to the script.
        #[structopt(name = "args")]
        args: Vec<String>,
    },

    /// Starts a REPL.
    #[structopt(name = "repl")]
    Repl,

    /// Interprets a single file.
    #[structopt(name = "script")]
    Script {
        /// The file being interpreted.
        #[structopt(name = "main_file", parse(from_os_str))]
        main: PathBuf,

        /// The arguments to the script.
        #[structopt(name = "args")]
        args: Vec<String>,
    },
}
