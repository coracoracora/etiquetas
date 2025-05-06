use std::{
    collections::{HashMap, hash_map::Keys},
    fs::read_to_string,
    ops::{Not, Range},
    path::{Path, PathBuf},
};

use anyhow::Result;
use clap::{Parser, Subcommand};
use derive_more::{Display, From};
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
        parent_prefix_len,
        0,
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
    parent_prefix_len: usize,
    _parent_scan_depth: usize,
) -> Result<()> {
    if !follow_symlinks && path.is_symlink() {
        // println!("Ignoring symlink: {}.", path.display());
        return Ok(());
    }

    if !include_dotfiles
        && path
            .file_name()
            .is_some_and(|n| n.len() > 1 && n.to_string_lossy().starts_with("."))
    {
        // println!("Ignoring dotfile: {}.", path.display());
        return Ok(());
    }

    // Check that we're markdown files.
    if path.is_file() && path.extension().is_some_and(|e| e == "md") {
        let _ = scan_file(path, parent_prefix_len);
        return Ok(());
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
                        parent_prefix_len,
                        _parent_scan_depth + 1,
                    );
                }
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
struct PathHits {
    #[allow(dead_code)]
    full_path: PathBuf,
    distinct_path_part: String,
    hits: Hits,
}

impl PathHits {
    fn try_new(full_path: &Path, distinct_path_part: &str, hits: Option<Hits>) -> Option<Self> {
        hits.map(|h| Self {
            full_path: full_path.to_path_buf(),
            distinct_path_part: distinct_path_part.to_string(),
            hits: h,
        })
    }
}

impl std::fmt::Display for PathHits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.distinct_path_part)?;
        for tag in self.hits.hit_keys_iter() {
            writeln!(f, "    tag: {}", tag)?;
            for range in self
                .hits
                .iter_key(tag)
                .expect("We just pulled the tag, but it's not there now!")
            {
                writeln!(f, "    |___ {:#?}", range)?;
            }
        }
        writeln!(f)
    }
}

fn scan_file(path: &Path, prefix_len: usize) -> Result<Option<PathHits>> {
    let body = read_to_string(path)?;
    let mut distinct_path_part = path.display().to_string();
    distinct_path_part.replace_range(..prefix_len + 1, "");

    Ok(
        if let Some(hits) = PathHits::try_new(path, &distinct_path_part, scan_text(&body)) {
            print!("{}", hits);
            Some(hits)
        } else {
            None
        },
    )
}

#[derive(Debug, Clone)]
struct Hits {
    #[allow(dead_code)]
    re: Regex,
    hits: HashMap<String, Vec<Range<usize>>>,
}

impl Hits {
    fn new(re: Regex) -> Self {
        Self {
            re,
            hits: HashMap::new(),
        }
    }
}

impl Hits {
    fn if_nonempty(self) -> Option<Self> {
        match self.empty() {
            true => None,
            _ => Some(self),
        }
    }

    fn empty(&self) -> bool {
        self.hit_count() == 0
    }

    fn hit_count(&self) -> usize {
        self.hits.len()
    }
    fn add_hit(&mut self, m: &str, range: Range<usize>) {
        let hitvec = self.hits.entry(m.to_string()).or_default();
        hitvec.push(range);
    }

    fn hit_keys_iter(&self) -> Keys<String, Vec<Range<usize>>> {
        self.hits.keys()
    }

    fn iter_key(&self, key: &str) -> Option<&Vec<Range<usize>>> {
        self.hits.get(key)
    }
}

fn scan_text(text: &str) -> Option<Hits> {
    let re = Regex::new(r"\#fg::\w+::[a-zA-Z0-9_\-]+").expect("Failed to compile regex!");
    let mut hitmap = Hits::new(re.clone()); // this feels hacky
    for hit in re
        .find_iter(text)
        .map(|m| (m.as_str().to_owned(), m.range()))
    {
        hitmap.add_hit(&hit.0, hit.1)
        // let hitvec = hitmap.entry(hit.0).or_insert(vec![]);
        // hitvec.push(hit.1);
    }
    hitmap.if_nonempty()
}
