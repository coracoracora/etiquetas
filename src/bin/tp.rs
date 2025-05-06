use std::{
    fs::read_to_string,
    ops::Not,
    path::{Path, PathBuf},
};

use anyhow::Result;
use clap::{Parser, Subcommand};
use derive_more::{Display, From};
use regex::Regex;

/// A CLI tool for performing various operations.
#[derive(Parser)]
#[command(name = "clap-derive-cli")]
#[command(about = "A CLI tool with a 'scan' subcommand", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Clone, Copy, Default, Display, PartialEq, Eq, Hash, From)]
struct FollowSymlinksFlag(bool);

impl From<&bool> for FollowSymlinksFlag {
    fn from(value: &bool) -> Self {
        Self(*value)
    }
}

impl Not for FollowSymlinksFlag {
    type Output = bool;

    fn not(self) -> Self::Output {
        self.0.not()
    }
}

#[derive(Debug, Clone, Copy, Default, Display, PartialEq, Eq, Hash, From)]
struct IncludeDotfilesFlag(bool);

impl From<&bool> for IncludeDotfilesFlag {
    fn from(value: &bool) -> Self {
        Self(*value)
    }
}
impl Not for IncludeDotfilesFlag {
    type Output = bool;

    fn not(self) -> Self::Output {
        self.0.not()
    }
}

#[derive(Subcommand)]
enum Commands {
    /// Scan a file or directory path.
    Scan {
        /// The file or directory path to scan.
        #[arg(short, long)]
        path: String,

        #[arg(short = 'd', long = "include-dotfiles")]
        include_dotfiles: bool,

        #[arg(short = 'f', long = "follow-symlinks")]
        follow_symlinks: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Scan {
            path,
            include_dotfiles,
            follow_symlinks,
        } => {
            println!("Scanning path: {} ", path);
            handle_scan(path, follow_symlinks.into(), include_dotfiles.into())
        }
    }
}

/// Handle a scan command.
/// - `path_str`: The path to scan
///
fn handle_scan(
    path_str: &str,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
) -> Result<()> {
    let path: PathBuf = path_str.into();
    let parent_prefix_len = path.display().to_string().len();
    scan_path(
        &path,
        follow_symlinks,
        include_dotfiles,
        0,
        parent_prefix_len,
    )
}

/// Scan a path. If it's a directory, descend. If it's a file,
/// scan the file.
/// - `path`: The path to scan
/// - `follow_symlinks`: If true, follow symlinks. If false, ignore them.
/// - `parent_scan_depth`: The depth of the scan at the call site.
fn scan_path(
    path: &Path,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
    parent_scan_depth: usize,
    parent_prefix_len: usize,
) -> Result<()> {
    if !follow_symlinks {
        if path.is_symlink() {
            println!("Ignoring symlink: {}.", path.display());
            return Ok(());
        }
    }

    if !include_dotfiles {
        if path
            .file_name()
            .is_some_and(|n| n.len() > 1 && n.to_string_lossy().starts_with("."))
        {
            println!("Ignoring dotfile: {}.", path.display());
            return Ok(());
        }
    }

    if path.is_file() {
        return scan_file(path, parent_prefix_len);
    }

    if path.is_dir() {
        for entry_result in path.read_dir()? {
            match entry_result {
                Err(e) => {
                    println!("Error reading {:#?}", e);
                }
                Ok(d) => {
                    let _ = scan_path(
                        &d.path(),
                        follow_symlinks,
                        include_dotfiles,
                        parent_scan_depth + 1,
                        parent_prefix_len,
                    );
                }
            }
        }
    }

    Ok(())
}

fn scan_file(path: &Path, prefix_len: usize) -> Result<()> {
    let body = read_to_string(path)?;
    let mut distinct_path_part = path.display().to_string();
    distinct_path_part.replace_range(..prefix_len + 1, "");
    for m in scan_text(&body)?.iter() {
        println!("{}: {}", distinct_path_part, m);
    }
    Ok(())
}

fn scan_text(text: &str) -> Result<Vec<String>> {
    let re = Regex::new(r"\#fg::kw::[a-zA-Z0-9_\-]+").expect("Failed to compile regex!");
    let matches: Vec<_> = re.find_iter(text).map(|m| m.as_str().to_owned()).collect();
    Ok(matches)
}
