//! Limn - A constraint-based programming language
//!
//! Limn is an order-independent programming language where programs are
//! constraint systems rather than sequential instructions. Execution is
//! bidirectional constraint satisfaction.

mod lexer;
mod parser;
mod ast;
mod solver;
mod value;
mod error;
mod ipfs;
mod registry;
mod package;
mod cli;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use colored::Colorize;

#[derive(Parser)]
#[command(name = "limn")]
#[command(author = "Limn Contributors")]
#[command(version = "0.2.0")]
#[command(about = "Limn - Constraint-based programming language with IPFS packages")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Run a .limn file directly
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    /// Key bindings (key=value)
    #[arg(short, long, value_name = "KEY=VALUE")]
    bind: Vec<String>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Limn program
    Run {
        /// Path to .limn file
        file: PathBuf,

        /// Key bindings (key=value)
        #[arg(short, long, value_name = "KEY=VALUE")]
        bind: Vec<String>,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Package management
    #[command(subcommand)]
    Pak(PakCommands),

    /// Start interactive REPL
    Repl,

    /// Show version information
    Version,
}

#[derive(Subcommand)]
enum PakCommands {
    /// Initialize a new package
    Init {
        /// Package name
        name: String,

        /// Directory (defaults to name)
        #[arg(short, long)]
        dir: Option<PathBuf>,
    },

    /// Validate package structure
    Check {
        /// Package directory
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Build package for distribution
    Build {
        /// Package directory
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Publish package to IPFS
    Pub {
        /// Package directory
        #[arg(default_value = ".")]
        dir: PathBuf,

        /// Sign with key
        #[arg(long)]
        sign: Option<String>,

        /// Pin to service
        #[arg(long)]
        pin: Option<String>,
    },

    /// Install dependencies
    Install {
        /// Package directory
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Add a dependency
    Add {
        /// Package name or CID
        package: String,

        /// Specific version
        #[arg(long)]
        version: Option<String>,

        /// Direct CID
        #[arg(long)]
        cid: Option<String>,
    },

    /// Remove a dependency
    Remove {
        /// Package name
        package: String,
    },

    /// Show dependency tree
    Tree {
        /// Package directory
        #[arg(default_value = ".")]
        dir: PathBuf,
    },

    /// Search registry
    Search {
        /// Search query
        query: String,
    },

    /// Show package info
    Info {
        /// Package name or CID
        package: Option<String>,
    },

    /// Cache management
    Cache {
        #[command(subcommand)]
        cmd: CacheCommands,
    },

    /// Key management
    Key {
        #[command(subcommand)]
        cmd: KeyCommands,
    },
}

#[derive(Subcommand)]
enum CacheCommands {
    /// Show cache info
    Info,
    /// Clear cache
    Clear,
}

#[derive(Subcommand)]
enum KeyCommands {
    /// Generate new signing key
    Gen {
        /// Key name
        name: String,
    },
    /// List keys
    List,
    /// Export public key
    Export {
        /// Key name
        name: String,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Some(Commands::Run { file, bind, verbose }) => {
            cli::run_file(&file, &bind, verbose)
        }
        Some(Commands::Pak(pak_cmd)) => {
            cli::handle_pak_command(pak_cmd)
        }
        Some(Commands::Repl) => {
            cli::run_repl()
        }
        Some(Commands::Version) => {
            cli::show_version();
            Ok(())
        }
        None => {
            // Direct file execution or help
            if let Some(file) = cli.file {
                cli::run_file(&file, &cli.bind, cli.verbose)
            } else {
                println!("{}", "Limn v0.2.0".green().bold());
                println!("Constraint-based programming language with IPFS packages\n");
                println!("Usage:");
                println!("  limn <file.limn>           Run a Limn program");
                println!("  limn run <file.limn>       Run a Limn program");
                println!("  limn pak <command>         Package management");
                println!("  limn repl                  Interactive REPL");
                println!("\nRun 'limn --help' for more information.");
                Ok(())
            }
        }
    };

    if let Err(e) = result {
        eprintln!("{}: {}", "Error".red().bold(), e);
        std::process::exit(1);
    }
}
