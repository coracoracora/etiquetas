use std::{
    fs::read_to_string,
    ops::Not,
    path::{Path, PathBuf},
};

use anyhow::Result;
use clap::{Parser, Subcommand};
use derive_more::{Display, From};
use etiquetas::etq::{
    mdscanner::{self, ScanEvent},
    model::{FoundTags, FoundTagsAtPath, TagIndex},
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

    /// Dump markdown events from a parse.
    Dump {
        #[arg(short, long)]
        file: String,
    },

    /// Scan using the pull parser.
    Pull {
        #[arg(short, long)]
        file: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Scan {
            path,
            include_dotfiles,
            follow_symlinks,
        } => handle_scan(path, follow_symlinks.into(), include_dotfiles.into()),
        // Commands::Dump { file } => handle_dump(file),
        Commands::Dump { file } => scan_markdown_file(file),
        Commands::Pull { file } => handle_dump(file),
    }
}

fn handle_dump(path_str: &str) -> Result<()> {
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
                let _ = event_stack.push(format!("{:?}", t));
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
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
) -> Result<()> {
    let path: PathBuf = path_str.into();
    let parent_prefix_len = path.display().to_string().len();
    let index = scan_path(
        &path,
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
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
    parent_prefix_len: usize,
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
        my_tag_index.add_path_hits(scan_file(path, parent_prefix_len)?);
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
                            follow_symlinks,
                            include_dotfiles,
                            parent_prefix_len,
                            _parent_scan_depth + 1,
                        )?;
                }
            }
        }
    }
    // println!("Done with path: {}", path.display());

    Ok(my_tag_index)
}

fn scan_markdown_file(path: &str) -> Result<()> {
    let body = read_to_string(path)?;
    let scanner = mdscanner::MarkdownScanner::new(&body, |t| {
        let re = Regex::new(r"\#fg::\w+::[a-zA-Z0-9_\-]+").expect("Failed to compile regex!");
        let ret: Vec<_> = re
            .find_iter(t)
            .map(|m| (m.as_str().to_owned().into(), m.range().into()))
            .collect();
        ret
    });
    for evt in scanner.into_iter().map(|e| match e { ScanEvent::TagsFound(t) => t}).flatten() {
        println!("{:?}", evt)
    }
    Ok(())
}

fn scan_file(path: &Path, prefix_len: usize) -> Result<Option<FoundTagsAtPath>> {
    let body = read_to_string(path)?;
    let mut distinct_path_part = path.display().to_string();
    distinct_path_part.replace_range(..prefix_len + 1, "");
    Ok(FoundTagsAtPath::try_new(
        path,
        &distinct_path_part,
        scan_text(&body),
    ))
}

fn scan_text(text: &str) -> Option<FoundTags> {
    // println!("Scanning text: {}...", text.get(0..50).expect("Unable to get text from str!"));

    let re = Regex::new(r"\#fg::\w+::[a-zA-Z0-9_\-]+").expect("Failed to compile regex!");
    let mut hitmap = FoundTags::new(re.to_string()); // this feels hacky
    for hit in re
        .find_iter(text)
        .map(|m| (m.as_str().to_owned(), m.range()))
    {
        hitmap.add_hit(hit.0, &hit.1.into())
    }
    hitmap.if_nonempty()
}
