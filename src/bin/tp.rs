use std::{
    fs::read_to_string,
    ops::Not,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use derive_more::{Display, From};
use etiquetas::etq::{
    embeddings::{embed_and_cluster_tags, ClusterSize, DbscanEpsilon, SentenceEmbeddingsModelType},
    mdscanner::{self, ScanEvent, TagsFound},
    model::TagIndex,
};
use pulldown_cmark::{Event, Parser as MDParser, TextMergeWithOffset};
use regex::Regex;

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

/// tp: A Tag Processor
#[derive(Parser)]
#[command(name = "clap-derive-cli")]
#[command(about = "A CLI tool with a 'scan' subcommand", long_about = None)]
struct Cli {
    #[cfg(samply_kludges)]
    /// Only used when we're debugging or profiling;
    /// see https://github.com/PhilippPolterauer/cargo-samply/pull/7
    samply_dummy_ignored: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Copy, Clone, Display)]
#[display(
    "{{ min_cluster_size: {}, tolerance: {} }}",
    min_cluster_size,
    tolerance
)]
struct ClusterReportConfig {
    /// The minimum number of members allowed in a cluster.
    min_cluster_size: usize,
    /// The tolerance ("epsilon") value passed to the underlying DBSCAN clustering algorithm.
    tolerance: f32,
}

impl Default for ClusterReportConfig {
    fn default() -> Self {
        Self {
            min_cluster_size: 2,
            tolerance: 0.975,
        }
    }
}

#[derive(Subcommand)]
enum Commands {
    /// Scan a file or directory path.
    Scan {
        /// The file or directory path to scan.
        #[arg(short, long)]
        path: String,

        /// If enabled, dotfiles/dotdirs will be scanned.
        #[arg(short = 'd', long = "include-dotfiles")]
        include_dotfiles: bool,

        /// If enable, symlinks will be followed.
        #[arg(short = 'f', long = "follow-symlinks")]
        follow_symlinks: bool,

        /// The pattern to use for recognizing tags
        #[arg(short = 'r', long, default_value = r"\#fg::\w+::[a-zA-Z0-9_\-]+")]
        tag_pattern: String,

        /// The pattern to use for grouping tags. This must contain at least
        /// one capture group, which will be used for grouping. If more than
        /// one group is included, the first will be used.
        #[arg(short = 'g', long, default_value = r"(\#fg\:\:[^:]+)(::.*\b)")]
        group_pattern: String,

        /// If present, the maximum recursion depth for the path scan. Unspecified = unlimited.
        #[arg(short = 'x', long)]
        max_depth: Option<usize>,
    },

    /// Dump a tag cluster report. This scans tags and performs a cluster analysis
    /// usng DBSCAN (see https://rust-ml.github.io/book/4_dbscan.html) and the given parameters.
    Cluster {
        /// The file or directory path to scan.
        #[arg(short, long)]
        path: String,

        /// If enabled, dotfiles/dotdirs will be scanned.
        #[arg(short = 'd', long = "include-dotfiles")]
        include_dotfiles: bool,

        /// If enable, symlinks will be followed.
        #[arg(short = 'f', long = "follow-symlinks")]
        follow_symlinks: bool,

        /// The pattern to use for recognizing tags
        #[arg(short = 'r', long, default_value = r"\#fg::\w+::[a-zA-Z0-9_\-]+")]
        tag_pattern: String,

        /// If present, the maximum recursion depth for the path scan. Unspecified = unlimited.
        #[arg(short = 'x', long)]
        max_depth: Option<usize>,

        /// The minimum number of members for a cluster.
        #[arg(short, long, default_value_t = ClusterSize::MIN.into())]
        min_cluster_size: usize,

        /// The tolerance (or epsilon) hyperparameter for
        /// the DBSCAN algorithm.
        #[arg(short, long, default_value_t = 1.05)]
        tolerance: f32,

        /// The model to use.
        #[arg(short = 'o', long, default_value = "all-mini-lm-l6v2")]
        model: SentenceEmbeddingsModelType,
    },

    /// Scan and dump a file using the bespoke Markdown acanner.
    ScanFile {
        #[arg(short, long)]
        file: String,

        /// The pattern to use for recognizing tags
        #[arg(short = 'p', long, default_value = r"\#fg::\w+::[a-zA-Z0-9_\-]+")]
        tag_pattern: String,
    },

    /// Scan using the default pull parser.
    DumpEvents {
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
            tag_pattern,
            group_pattern,
            max_depth,
        } => {
            let tag_re = Regex::new(tag_pattern)?;
            let group_re = Regex::new(group_pattern)?;
            handle_scan(
                path,
                &tag_re,
                &group_re,
                follow_symlinks.into(),
                include_dotfiles.into(),
                *max_depth,
            )
        }
        Commands::Cluster {
            path,
            include_dotfiles,
            follow_symlinks,
            tag_pattern,
            max_depth,
            min_cluster_size,
            tolerance,
            model,
        } => {
            let tag_re = Regex::new(tag_pattern)?;
            let follow_symlinks: FollowSymlinksFlag = follow_symlinks.into();
            let include_dotfiles: IncludeDotfilesFlag = include_dotfiles.into();
            handle_cluster_cmd(
                path,
                &tag_re,
                follow_symlinks,
                include_dotfiles,
                *max_depth,
                0,
                *min_cluster_size,
                *tolerance,
                *model,
            )
        }

        Commands::ScanFile { file, tag_pattern } => {
            let tag_re = Regex::new(tag_pattern)?;
            let _ = scan_markdown_file(PathBuf::from(file).as_ref(), &tag_re)?;
            Ok(())
        }
        Commands::DumpEvents { file } => handle_dump_markdown_parse_events(file),
    }
}

#[allow(clippy::too_many_arguments)]
fn handle_cluster_cmd(
    path_str: &str,
    re: &Regex,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
    max_depth: Option<usize>,
    parent_scan_depth: usize,
    min_cluster_size: usize,
    tolerance: f32,
    model_type: SentenceEmbeddingsModelType,
) -> Result<()> {
    let min_cluster_size = ClusterSize::try_new(min_cluster_size).with_context(|| "min-cluster-size")?;
    let tolerance = DbscanEpsilon::try_new(tolerance).with_context(|| "tolerance")?;
    let tags = scan_path(
        Path::new(path_str),
        re,
        follow_symlinks,
        include_dotfiles,
        max_depth,
        parent_scan_depth,
    )?
    .tags();
    let clusters = embed_and_cluster_tags(
        tags,
        tch::Device::Cpu,
        model_type,
        min_cluster_size,
        tolerance,
    )?;
    println!("{}", clusters);
    Ok(())
}

/// Bare parse of the given markdown file,
/// dumping events as they're received.
fn handle_dump_markdown_parse_events(path_str: &str) -> Result<()> {
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

/// Handle a scan of markdown files in a directory tree.
/// - `path_str`: The path to scan
/// - `tag_re`: The regex to use for tag recognition.
/// - `group_re`: The regex that will be applied to each recognized tag
///   when grouping. At least one capture must be specified;
///   if more than one is specified, then the first is used.
/// - `follow_symlinks`: If true, symlinks will be followed when scanning
///   the tree. If false, they won't.
/// - `include_dotfiles`: If true, dotfiles / dotdirs will be included
///   in the scan. If false, they won't.
fn handle_scan(
    path_str: &str,
    tag_re: &Regex,
    group_re: &Regex,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
    max_depth: Option<usize>,
) -> Result<()> {
    let path: PathBuf = path_str.into();
    let index = scan_path(
        &path,
        tag_re,
        follow_symlinks,
        include_dotfiles,
        max_depth,
        0,
    )?;
    let _grouping = index.group_keys(
        group_re,
        |c| (c[1]).to_string(),
        "A default grouping based upon project::type stemming.",
    );
    print!("{}", _grouping);
    Ok(())
}

/// Scan a path. If it's a directory, descend. If it's a file,
/// scan the file.
/// - `path`: The path to scan
/// - `re`: The regex to be used for finding tags.
/// - `follow_symlinks`: If true, follow symlinks. If false, ignore them.
/// - `include_dotfiles`: If true, dotfiles and dotdirs will be included
///   in scanning and traversal, respectively.
/// - `_parent_prefix_len`: The length of the
/// - `_parent_scan_depth`: The depth of the scan at the call site.
fn scan_path(
    path: &Path,
    re: &Regex,
    follow_symlinks: FollowSymlinksFlag,
    include_dotfiles: IncludeDotfilesFlag,
    max_depth: Option<usize>,
    parent_scan_depth: usize,
) -> Result<TagIndex> {
    // println!("Scanning path: {} ", path.display());
    let mut my_tag_index = TagIndex::new(path);

    if let Some(max_depth) = max_depth {
        if parent_scan_depth >= max_depth {
            return Ok(my_tag_index);
        }
    }

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
            my_tag_index.add_tag_location(hits.0.clone(), tag);
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
                            max_depth,
                            parent_scan_depth + 1,
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
        re.find_iter(t)
            .map(|m| (m.as_str().to_owned().into(), m.range().into()))
            .collect::<Vec<_>>()
    });
    let found: TagsFound = scanner
        .into_iter()
        .flat_map(|e| match e {
            ScanEvent::TagsFound(t) => {
                // println!(
                //     "Tags Found in {}: \n{}\n",
                //     path.file_name()
                //         .map(|f| f.to_string_lossy().to_string())
                //         .unwrap_or("-".to_string()),
                //     t
                // );
                t
            }
        })
        .collect();

    Ok((path.into(), found))
}
