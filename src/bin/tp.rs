use std::{
    fs::read_to_string,
    ops::Not,
    path::{Path, PathBuf},
};

use anyhow::Result;
use clap::{Parser, Subcommand};
use derive_more::{Display, From};
use etiquetas::etq::{
    mdscanner::{self, ScanEvent, TagsFound},
    model::TagIndex,
};
use pulldown_cmark::{Event, Parser as MDParser, TextMergeWithOffset};
use regex::Regex;

/// tp: A Tag Processor
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

    /// Scan and dump a file using the bespoke Markdown acanner.
    MDScan {
        #[arg(short, long)]
        file: String,
    },

    /// Scan using the default pull parser.
    Pull {
        #[arg(short, long)]
        file: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let re = Regex::new(r"\#fg::\w+::[a-zA-Z0-9_\-]+").expect("Failed to compile regex!");

    match &cli.command {
        Commands::Scan {
            path,
            include_dotfiles,
            follow_symlinks,
        } => handle_scan(path, &re, follow_symlinks.into(), include_dotfiles.into()),
        Commands::MDScan { file } => {
            let _ = scan_markdown_file(PathBuf::from(file).as_ref(), &re)?;
            Ok(())
        }
        Commands::Pull { file } => handle_pull(file),
    }
}

fn handle_pull(path_str: &str) -> Result<()> {
    let path: PathBuf = path_str.into();

    let content = read_to_string(path)?;
    let mut event_stack: Vec<String> = vec![];

    let parser_iter = TextMergeWithOffset::new(MDParser::new(&content).into_offset_iter());
    for (event, range) in parser_iter {
        println!(
            "EVENT: {3:?} :: {0}@{1}..{2}",
            event_stack
                .clone()
                .into_iter()
                .fold("*".to_string(), |acc, e| format!("{} → {}", acc, e)),
            range.start,
            range.end,
            event
        );
        match event {
            Event::Start(t) => {
                event_stack.push(format!("{:?}", t));
            }
            Event::End(_) => {
                let _ = event_stack.pop();
            }
            _ => {}
        }
    }

    Ok(())
}

/// Handle a scan command.
/// - `path_str`: The path to scan
///
fn handle_scan(
    path_str: &str,
    re: &Regex,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
) -> Result<()> {
    let path: PathBuf = path_str.into();
    let parent_prefix_len = path.display().to_string().len();
    let index = scan_path(
        &path,
        re,
        follow_symlinks,
        include_dotfiles,
        parent_prefix_len,
        0,
    )?;
    let _grouping = index.group_keys(
        r"(\#fg\:\:[^:]+)(::.*\b)",
        |c| (c[1]).to_string(),
        "A default grouping based upon project::type stemming.",
    );
    print!("{}", _grouping);
    Ok(())
}

/// Scan a path. If it's a directory, descend. If it's a file,
/// scan the file.
/// - `path`: The path to scan
/// - `follow_symlinks`: If true, follow symlinks. If false, ignore them.
/// - `parent_scan_depth`: The depth of the scan at the call site.
fn scan_path(
    path: &Path,
    re: &Regex,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
    _parent_prefix_len: usize,
    _parent_scan_depth: usize,
) -> Result<TagIndex> {
    // println!("Scanning path: {} ", path.display());
    let mut my_tag_index = TagIndex::new(path);

    if !follow_symlinks && path.is_symlink() {
        return Ok(my_tag_index);
    }

    if !include_dotfiles
        && path
            .file_name()
            .is_some_and(|n| n.len() > 1 && n.to_string_lossy().starts_with("."))
    {
        return Ok(my_tag_index);
    }

    if path.is_file() && path.extension().is_some_and(|e| e == "md") {
        let hits = scan_markdown_file(path, re)?;
        for tag in hits.1 {
            my_tag_index.add_tag_location(&tag.tag, hits.0.clone(), tag.range_in_body);
        }
        return Ok(my_tag_index);
    }

    if path.is_dir() {
        for entry_result in path.read_dir()? {
            match entry_result {
                Err(e) => {
                    println!("Error reading {:#?}", e);
                }
                Ok(d) => {
                    my_tag_index = my_tag_index
                        + scan_path(
                            &d.path(),
                            re,
                            follow_symlinks,
                            include_dotfiles,
                            _parent_prefix_len,
                            _parent_scan_depth + 1,
                        )?;
                }
            }
        }
    }

    Ok(my_tag_index)
}

/// Scan a markdown file using pulldown_cmark.
fn scan_markdown_file(path: &Path, re: &Regex) -> Result<(PathBuf, TagsFound)> {
    let body = read_to_string(path)?;
    let scanner = mdscanner::MarkdownScanner::new(&body, |t| {
        // Regex::new(r"\#fg::\w+::[a-zA-Z0-9_\-]+")
        //     .expect("Failed to compile regex!")
        re.find_iter(t)
            .map(|m| (m.as_str().to_owned().into(), m.range().into()))
            .collect::<Vec<_>>()
    });
    let found: TagsFound = scanner
        .into_iter()
        .flat_map(|e| match e {
            ScanEvent::TagsFound(t) => {
                println!(
                    "Tags Found in {}: \n{}\n",
                    path.file_name()
                        .map(|f| f.to_string_lossy().to_string())
                        .unwrap_or("-".to_string()),
                    t
                );
                t
            }
        })
        .collect();

    Ok((path.into(), found))
}
