use std::{
    collections::{hash_map::Keys, HashMap}, fmt::Display, ops::{Add, Range}, path::{Path, PathBuf}
};

use derive_more::Display;
use regex::{Captures, Regex};
use serde::{Deserialize, Serialize};

/// Represents a location of a tag in a file, at a byte range (verify... maybe codepoint?)
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TextLocation {
    path: PathBuf,
    range: TextRange,
}
impl Display for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}@{}..{}",
            self.path.display(),
            self.range.start(),
            self.range.end(),
        )
    }
}

impl TextLocation {
    pub fn new<P, L>(into_pathbuf: P, into_range: L) -> Self
    where
        P: Into<PathBuf>,
        L: Into<TextRange>,
    {
        Self {
            path: into_pathbuf.into(),
            range: into_range.into(),
        }
    }
}

impl<P, L> From<(P, L)> for TextLocation
where
    P: Into<PathBuf>,
    L: Into<TextRange>,
{
    fn from(value: (P, L)) -> Self {
        Self::new(value.0, value.1)
    }
}

/// Represents a collection of tag file locations.
#[derive(Default, Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct TextLocations {
    inner: Vec<TextLocation>,
}

impl From<Vec<TextLocation>> for TextLocations {
    fn from(value: Vec<TextLocation>) -> Self {
        Self { inner: value }
    }
}

impl IntoIterator for TextLocations {
    type Item = TextLocation;
    type IntoIter = std::vec::IntoIter<TextLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a TextLocations {
    type Item = &'a TextLocation;
    type IntoIter = std::slice::Iter<'a, TextLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut TextLocations {
    type Item = &'a mut TextLocation;
    type IntoIter = std::slice::IterMut<'a, TextLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl TextLocations {
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn push(&mut self, value: &TextLocation) {
        self.inner.push(value.clone())
    }

    pub fn append(&mut self, value: &mut TextLocations) {
        self.inner.append(&mut value.inner)
    }
}

/// Represents a mapping of `Tag` instances to `TagLocations` instances.
#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct TagsToLocations {
    inner: HashMap<Tag, TextLocations>,
}

impl From<HashMap<Tag, TextLocations>> for TagsToLocations {
    fn from(value: HashMap<Tag, TextLocations>) -> Self {
        Self { inner: value }
    }
}

impl IntoIterator for TagsToLocations {
    type Item = (Tag, TextLocations);
    type IntoIter = std::collections::hash_map::IntoIter<Tag, TextLocations>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a TagsToLocations {
    type Item = (&'a Tag, &'a TextLocations);
    type IntoIter = std::collections::hash_map::Iter<'a, Tag, TextLocations>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut TagsToLocations {
    type Item = (&'a Tag, &'a mut TextLocations);
    type IntoIter = std::collections::hash_map::IterMut<'a, Tag, TextLocations>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl TagsToLocations {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: Tag, value: TextLocations) -> Option<TextLocations> {
        self.inner.insert(key, value)
    }

    pub fn entry(&mut self, key: Tag) -> std::collections::hash_map::Entry<Tag, TextLocations> {
        self.inner.entry(key)
    }

    pub fn get(&self, key: &Tag) -> Option<&TextLocations> {
        self.inner.get(key)
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<Tag, TextLocations> {
        self.inner.keys()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

}

/// Encapsulates tag patterns and functions used for re.replace(), for construction
/// of groups.
pub struct TagGrouper {
    pub _capture_pattern: String,
    pub _group_key_constructor: Box<dyn Fn(&Captures) -> String>,
    pub _description: String,
}

impl<'a> TagGrouper {
    fn capture_pattern(&'a self) -> &'a str {
        &self._capture_pattern
    }

    fn group_key_constructor(&'a self) -> &'a impl Fn(&Captures) -> String {
        &self._group_key_constructor
    }

    fn description(&'a self) -> &'a str {
        &self._description
    }
}

impl std::fmt::Debug for TagGrouper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TagGrouper")
            .field("capture_pattern", &self.capture_pattern())
            .finish()
    }
}

#[derive(Debug, Clone, Display, Serialize, Deserialize, Hash, Eq, PartialEq, PartialOrd)]
#[serde(transparent)]
pub struct TagGroup(Tag);
impl AsRef<str> for TagGroup {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
impl<T> From<T> for TagGroup
where
    T: Into<Tag>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TagGroups {
    pub capture_pattern: String,
    pub groups: HashMap<TagGroup, TagsToLocations>,
    pub description: String,
}

impl Display for TagGroups {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "capture_pattern: {}", self.capture_pattern)?;
        for (group_key, group) in self.groups.clone() {
            writeln!(f, "📦  {} ({}):", group_key, group.len())?;
            for (tag, locs) in group {
                writeln!(f, "      |__ 🏷️  {} ({})", tag, locs.len())?;
                for TextLocation { path, range } in locs {
                    writeln!(
                        f,
                        "      |    |__ 📄 {}:{}..{}",
                        path.display(),
                        range.start(),
                        range.end(),
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
    pub root_path: PathBuf,

    // Map of tags to vecs of subpaths to files.
    pub tags_to_locations: TagsToLocations,
}

impl Display for TagIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.root_path.display())?;
        for (tag, locs) in &self.tags_to_locations {
            writeln!(f, "  🏷️  {}", tag)?;
            for TextLocation { path, range } in locs.clone() {
                // hacky
                writeln!(
                    f,
                    "     |__ 📄 {}:{}..{}",
                    path.display(),
                    range.start(),
                    range.end(),
                )?;
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
            root_path: full_path.to_path_buf(),
            tags_to_locations: TagsToLocations::default(),
        }
    }

    pub fn group_with_grouper<'a>(&'a self, grouper: &'a TagGrouper) -> TagGroups {
        self.group_keys(
            grouper.capture_pattern(),
            grouper.group_key_constructor(),
            grouper.description(),
        )
    }

    pub fn group_keys(
        &self,
        capture_pattern: &str,
        group_key_constructor: impl Fn(&Captures) -> String,
        description: &str,
    ) -> TagGroups {
        let re = Regex::new(capture_pattern)
            .unwrap_or_else(|e| panic!("RE compile failed for '{}': {:?}", capture_pattern, e));

        let mut groups = HashMap::default();

        // uglee
        for (tag, tag_locations) in self.tags_to_locations.clone() {
            let group_key: TagGroup = re.replace(tag.as_ref(), &group_key_constructor).into();
            let entry: &mut TagsToLocations = groups.entry(group_key).or_default();
            let _ = entry.insert(tag, tag_locations);
        }

        TagGroups {
            capture_pattern: capture_pattern.to_owned(),
            groups,
            description: description.to_owned(),
        }
    }

    /// Merge the given ``PathHits`` instance intos the index.
    pub fn add_path_hits(&mut self, path_hits: Option<FoundTagsAtPath>) {
        // gnarly
        if let Some(path_hits) = path_hits {
            // println!("addin path hits: {}", path_hits);

            for (tag, locations) in &path_hits.hits.tag_locations {
                // println!("adding tag/locs: {}, {:?}", tag, locations);
                let tag_entry = self.tags_to_locations.entry(tag.clone()).or_default();
                let locations: TextLocations = locations
                    .clone()
                    .into_iter()
                    .map(|l| {
                        // println!("in map; l: {:?}", l);
                        TextLocation::new(path_hits.distinct_path_part.clone(), l.inner)
                    })
                    .collect::<Vec<TextLocation>>()
                    .into();
                // println!("appending locs: {:?}", locations);
                tag_entry.append(&mut locations.clone())
            }
            // println!("finished adding path hits: {}", path_hits);
        }
    }
}

impl Add for TagIndex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        TagIndex {
            root_path: self.root_path,
            tags_to_locations: HashMap::from_iter(
                self.tags_to_locations.into_iter().chain(rhs.tags_to_locations),
            )
            .into(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FoundTagsAtPath {
    #[allow(dead_code)]
    pub full_path: PathBuf,
    pub distinct_path_part: String,
    pub hits: FoundTags,
}

impl FoundTagsAtPath {
    pub fn try_new(full_path: &Path, distinct_path_part: &str, hits: Option<FoundTags>) -> Option<Self> {
        hits.map(|h| Self {
            full_path: full_path.to_path_buf(),
            distinct_path_part: distinct_path_part.to_string(),
            hits: h,
        })
    }
}

impl Display for FoundTagsAtPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.distinct_path_part)?;
        for (tag, locations) in &self.hits.tag_locations {
            let locations = locations.clone(); // debug
            writeln!(f, "    tag: {}", tag)?;
            // println!("pathhits::display::locations: {:#?}", locations);
            for location in locations {
                writeln!(f, "    |___ @{}..{}", location.start(), location.end())?;
            }
        }
        writeln!(f)
    }
}

/// Tags, their locations, and the pattern that found them.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FoundTags {
    pub pattern: String,
    pub tag_locations: TagTextRanges,
}

impl FoundTags {
    pub fn new(pattern: String) -> Self {
        Self {
            pattern,
            tag_locations: TagTextRanges::default(),
        }
    }
}

impl FoundTags {
    pub fn insert(&mut self, key: Tag, value: TextRanges) -> Option<TextRanges> {
        self.tag_locations.insert(key, value)
    }

    pub fn entry(&mut self, key: Tag) -> std::collections::hash_map::Entry<Tag, TextRanges> {
        self.tag_locations.entry(key)
    }

    pub fn get(&self, key: &Tag) -> Option<&TextRanges> {
        self.tag_locations.get(key)
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<Tag, TextRanges> {
        self.tag_locations.keys()
    }

    pub fn len(&self) -> usize {
        self.tag_locations.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.tag_locations.is_empty()
    }

}

impl FoundTags {
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
        self.tag_locations.len()
    }
    pub fn add_hit<T>(&mut self, tag: T, location: &TextRange)
    where
        T: Into<Tag>,
    {
        let locations = self.tag_locations.entry(tag.into()).or_default();
        locations.push(location.clone())
    }

    pub fn tag_iter(&self) -> Keys<Tag, TextRanges> {
        self.tag_locations.keys()
    }

    pub fn hits_iter_for_tag<T>(&self, key: T) -> Option<&TextRanges>
    where
        T: AsRef<Tag>,
    {
        self.tag_locations.get(key.as_ref())
    }
}

/// NewType wrapper around a [`HashMap<Tag, TextRanges>`]. The keys
/// are [`Tag`] instances, the values collections of text  ranges.

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TagTextRanges {
    inner: HashMap<Tag, TextRanges>,
}

impl TagTextRanges {
    pub fn insert(&mut self, key: Tag, value: TextRanges) -> Option<TextRanges> {
        self.inner.insert(key, value)
    }

    pub fn entry(&mut self, key: Tag) -> std::collections::hash_map::Entry<Tag, TextRanges> {
        self.inner.entry(key)
    }

    pub fn get(&self, key: &Tag) -> Option<&TextRanges> {
        self.inner.get(key)
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<Tag, TextRanges> {
        self.inner.keys()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

}

impl IntoIterator for TagTextRanges {
    type Item = (Tag, TextRanges);
    type IntoIter = std::collections::hash_map::IntoIter<Tag, TextRanges>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a TagTextRanges {
    type Item = (&'a Tag, &'a TextRanges);
    type IntoIter = std::collections::hash_map::Iter<'a, Tag, TextRanges>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut TagTextRanges {
    type Item = (&'a Tag, &'a mut TextRanges);
    type IntoIter = std::collections::hash_map::IterMut<'a, Tag, TextRanges>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

/// Represents a fully-formed tag
#[derive(
    Default, Debug, Clone, PartialEq, Eq, PartialOrd, Hash, Display, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct Tag {
    inner: String,
}
impl<T> From<T> for Tag
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self {
            inner: value.into(),
        }
    }
}

impl AsRef<str> for Tag {
    fn as_ref(&self) -> &str {
        self.inner.as_str()
    }
}

/// Newtype wrapper around a vec of TextRanges.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TextRanges {
    inner: Vec<TextRange>,
}

impl<T> From<T> for TextRanges where T: Into<Vec<TextRange>> {
    fn from(value: T) -> Self {
        Self { inner: value.into() }
    }
}

impl IntoIterator for TextRanges {
    type Item = TextRange;
    type IntoIter = std::vec::IntoIter<TextRange>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a TextRanges {
    type Item = &'a TextRange;
    type IntoIter = std::slice::Iter<'a, TextRange>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut TextRanges {
    type Item = &'a mut TextRange;
    type IntoIter = std::slice::IterMut<'a, TextRange>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl TextRanges {
    fn push(&mut self, value: TextRange) {
        self.inner.push(value)
    }
}

/// Newtype wrapper around a [`Range<usize>'].
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq, Hash)]
#[serde(transparent)]
pub struct TextRange {
    inner: Range<usize>,
}
impl TextRange {
    fn start(&self) -> usize {
        self.inner.start
    }

    fn end(&self) -> usize {
        self.inner.end
    }
}

impl From<Range<usize>> for TextRange {
    fn from(value: Range<usize>) -> Self {
        Self { inner: value }
    }
}

impl Display for TextRange {
    
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.inner.start, self.inner.end)
    }
}
