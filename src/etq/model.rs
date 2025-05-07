use std::{
    collections::{HashMap, hash_map::Keys},
    ops::{Add, Range},
    path::{Path, PathBuf},
};

use regex::{Captures, Regex};
use serde::{Deserialize, Serialize};

type TagsToPaths = HashMap<String, Vec<(String, Range<usize>)>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TagGroup {
    pub capture_pattern: String,
    pub groups: HashMap<String, TagsToPaths>,
}

impl std::fmt::Display for TagGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "capture_pattern: {}", self.capture_pattern)?;
        for (group_key, group) in self.groups.clone() {
            writeln!(f, "📦  {} ({}):", group_key, group.len())?;
            for (tag, locs) in group {
                writeln!(f, "      |__ 🏷️  {} ({})", tag, locs.len())?;
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TagIndex {
    // Toplevel path
    pub full_path: PathBuf,

    // Map of tags to vecs of subpaths to files.
    pub tags_to_paths: TagsToPaths,
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
    pub fn new(full_path: &Path) -> Self {
        Self {
            full_path: full_path.to_path_buf(),
            tags_to_paths: HashMap::default(),
        }
    }

    pub fn group_keys(
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
    pub fn add_path_hits(&mut self, path_hits: Option<PathHits>) {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathHits {
    #[allow(dead_code)]
    pub full_path: PathBuf,
    pub distinct_path_part: String,
    pub hits: Hits,
}

impl PathHits {
    pub fn try_new(full_path: &Path, distinct_path_part: &str, hits: Option<Hits>) -> Option<Self> {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Hits {
    #[allow(dead_code)]
    pub pat: String,
    pub hits: HashMap<String, Vec<Range<usize>>>,
}

impl Hits {
    pub fn new(re: String) -> Self {
        Self {
            pat: re,
            hits: HashMap::new(),
        }
    }
}

impl Hits {
    pub fn if_nonempty(self) -> Option<Self> {
        match self.empty() {
            true => None,
            _ => Some(self),
        }
    }

    pub fn empty(&self) -> bool {
        self.hit_count() == 0
    }

    pub fn hit_count(&self) -> usize {
        self.hits.len()
    }
    pub fn add_hit(&mut self, m: &str, range: Range<usize>) {
        let hitvec = self.hits.entry(m.to_string()).or_default();
        hitvec.push(range);
    }

    pub fn tag_iter(&self) -> Keys<String, Vec<Range<usize>>> {
        self.hits.keys()
    }

    pub fn hits_iter_for_tag(&self, key: &str) -> Option<&Vec<Range<usize>>> {
        self.hits.get(key)
    }
}
