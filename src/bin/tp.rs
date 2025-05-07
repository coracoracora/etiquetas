use std::{
    collections::{HashMap, hash_map::Keys},
    fs::read_to_string,
    ops::{Add, Not, Range},
    path::{Path, PathBuf},
};

use anyhow::Result;
use clap::{Parser, Subcommand};
use derive_more::{Display, From};
use regex::{Captures, Regex};

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
    let index = scan_path(
        &path,
        follow_symlinks,
        include_dotfiles,
        parent_prefix_len,
        0,
    )?;
    let grouping = index.group_keys(r"(\#fg\:\:[^:]+)(::.*\b)", |c| (c[1]).to_string());
    print!("{}", grouping);
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
    let mut my_tag_index = TagIndex::new(path);

    if !follow_symlinks && path.is_symlink() {
        // println!("Ignoring symlink: {}.", path.display());
        return Ok(my_tag_index);
    }

    if !include_dotfiles
        && path
            .file_name()
            .is_some_and(|n| n.len() > 1 && n.to_string_lossy().starts_with("."))
    {
        // println!("Ignoring dotfile: {}.", path.display());
        return Ok(my_tag_index);
    }

    // Check that we're markdown files.
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

    Ok(my_tag_index)
}

type TagsToPaths = HashMap<String, Vec<(String, Range<usize>)>>;

#[derive(Debug, Clone)]
struct TagGroup {
    capture_pattern: String,
    groups: HashMap<String, TagsToPaths>,
}

impl std::fmt::Display for TagGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "capture_pattern: {}", self.capture_pattern)?;
        for (group_key, group) in self.groups.clone() {
            writeln!(f, "📦  {}:", group_key)?;
            for (tag, locs) in group {
                writeln!(f, "      |__ 🏷️  {}", tag)?;
                for (loc, range) in locs {
                    writeln!(
                        f,
                        "      |    |__ 📄 {}:{}..{}",
                        loc, range.start, range.end
                    )?;
                }
            }
        }
        write!(f, "")
    }
}

#[derive(Debug, Clone, Default)]
struct TagIndex {
    // Toplevel path
    full_path: PathBuf,

    // Map of tags to vecs of subpaths to files.
    tags_to_paths: TagsToPaths,
}

impl std::fmt::Display for TagIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.full_path.display())?;
        for tag in self.tags_to_paths.keys() {
            writeln!(f, "  🏷️  {}", tag)?;
            for loc in self
                .tags_to_paths
                .get(tag)
                .unwrap_or_else(|| panic!("Unable to look up tag {}, which we just pulled.", &tag))
                .iter()
            {
                writeln!(f, "     |__ 📄 {}:{}..{}", loc.0, loc.1.start, loc.1.end)?;
            }
        }
        write!(f, "")
    }
}

/// This inverts a collection of PathHits, turning it into a map
/// of Tag -> Vec<Filename, Range>, providing a file and location in
/// the file for each instance of the tag found.
///
impl TagIndex {
    fn new(full_path: &Path) -> Self {
        Self {
            full_path: full_path.to_path_buf(),
            tags_to_paths: HashMap::default(),
        }
    }

    fn group_keys(
        &self,
        capture_pattern: &str,
        group_key_constructor: impl Fn(&Captures) -> String,
    ) -> TagGroup {
        let re = Regex::new(capture_pattern)
            .unwrap_or_else(|e| panic!("RE compile failed for '{}': {:?}", capture_pattern, e));

        let mut ret = HashMap::default();

        // uglee
        for (key, value) in self.tags_to_paths.clone() {
            let group_key = re.replace(&key, &group_key_constructor);
            let entry: &mut HashMap<_, _> = ret.entry(group_key.into()).or_default();
            let _ = entry.insert(key, value);
        }

        TagGroup {
            capture_pattern: capture_pattern.to_owned(),
            groups: ret,
        }
    }

    /// Merge the given ``PathHits`` instance intos the index.
    fn add_path_hits(&mut self, path_hits: Option<PathHits>) {
        // gnarly
        if let Some(path_hits) = path_hits {
            let subpath = &path_hits.distinct_path_part;
            for tag in path_hits.hits.tag_iter() {
                let tag_vec = self.tags_to_paths.entry(tag.clone()).or_default();
                if let Some(range_vec) = path_hits.hits.hits_iter_for_tag(tag) {
                    let mut append_vec: Vec<_> = range_vec
                        .iter()
                        .map(|r| (subpath.clone(), r.clone()))
                        .collect();
                    tag_vec.append(&mut append_vec);
                }
            }
        }
    }
}

impl Add for TagIndex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        TagIndex {
            full_path: self.full_path,
            tags_to_paths: HashMap::from_iter(
                self.tags_to_paths.into_iter().chain(rhs.tags_to_paths),
            ),
        }
    }
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
        for tag in self.hits.tag_iter() {
            writeln!(f, "    tag: {}", tag)?;
            for range in self
                .hits
                .hits_iter_for_tag(tag)
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
    Ok(PathHits::try_new(
        path,
        &distinct_path_part,
        scan_text(&body),
    ))
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

    fn tag_iter(&self) -> Keys<String, Vec<Range<usize>>> {
        self.hits.keys()
    }

    fn hits_iter_for_tag(&self, key: &str) -> Option<&Vec<Range<usize>>> {
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
