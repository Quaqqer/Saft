use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
pub struct Args {
    #[command(subcommand)]
    pub(crate) command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    /// Run a saft script
    Run {
        file: PathBuf,

        #[arg(short, long, action)]
        /// Open a REPL after evaluating module
        interactive: bool,
    },

    /// Start a REPL session
    Repl {},
}
