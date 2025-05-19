use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::Display,
    ops::{Add, Range},
    path::{Path, PathBuf},
};

use derive_more::{AsRef, Deref, DerefMut, Display, From, FromStr, Index, IndexMut, IntoIterator};
use regex::{Captures, Regex};
use serde::{Deserialize, Serialize};

use super::{embeddings::ClusteringProduct, mdscanner::TagFound};

/// Represents a location of something in a file, at a byte range (verify... maybe codepoint?)
#[derive(Debug, Display, Clone, PartialEq, Hash, Serialize, Deserialize)]
#[display("{}@{}", self.path.display(), self.text_range_in_file())]
pub struct TagLocation {
    /// The path to the file containing whatever is being located.
    path: PathBuf,

    tag_found: TagFound,
}

impl TagLocation {
    pub fn new<P>(into_pathbuf: P, tag_found: TagFound) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            path: into_pathbuf.into(),
            tag_found,
        }
    }
}

impl TagLocation {
    /// The range of the tag in the file.
    pub fn text_range_in_file(&self) -> StrRange {
        self.tag_found.range_in_body.clone()
    }

    /// The range of the tag in the context.
    pub fn text_range_in_context(&self) -> StrRange {
        self.tag_found.range_in_text.clone()
    }
}

/// Represents a collection of tag file locations.
#[derive(Default, Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct TagLocations {
    inner: Vec<TagLocation>,
}

impl From<Vec<TagLocation>> for TagLocations {
    fn from(value: Vec<TagLocation>) -> Self {
        Self { inner: value }
    }
}

impl IntoIterator for TagLocations {
    type Item = TagLocation;
    type IntoIter = std::vec::IntoIter<TagLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a TagLocations {
    type Item = &'a TagLocation;
    type IntoIter = std::slice::Iter<'a, TagLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut TagLocations {
    type Item = &'a mut TagLocation;
    type IntoIter = std::slice::IterMut<'a, TagLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl TagLocations {
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn push(&mut self, value: &TagLocation) {
        self.inner.push(value.clone())
    }

    pub fn append(&mut self, value: &mut TagLocations) {
        self.inner.append(&mut value.inner)
    }
}

/// Represents a mapping of `Tag` instances to `TagLocations` instances.
#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct TagsToLocations {
    inner: HashMap<Tag, TagLocations>,
}

impl From<HashMap<Tag, TagLocations>> for TagsToLocations {
    fn from(value: HashMap<Tag, TagLocations>) -> Self {
        Self { inner: value }
    }
}

impl IntoIterator for TagsToLocations {
    type Item = (Tag, TagLocations);
    type IntoIter = std::collections::hash_map::IntoIter<Tag, TagLocations>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a> IntoIterator for &'a TagsToLocations {
    type Item = (&'a Tag, &'a TagLocations);
    type IntoIter = std::collections::hash_map::Iter<'a, Tag, TagLocations>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for &'a mut TagsToLocations {
    type Item = (&'a Tag, &'a mut TagLocations);
    type IntoIter = std::collections::hash_map::IterMut<'a, Tag, TagLocations>;

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

    pub fn insert(&mut self, key: Tag, value: TagLocations) -> Option<TagLocations> {
        self.inner.insert(key, value)
    }

    pub fn entry(&mut self, key: Tag) -> std::collections::hash_map::Entry<Tag, TagLocations> {
        self.inner.entry(key)
    }

    pub fn get(&self, key: &Tag) -> Option<&TagLocations> {
        self.inner.get(key)
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<Tag, TagLocations> {
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
    pub _capture_expression: Regex,
    pub _group_key_constructor: Box<dyn Fn(&Captures) -> String>,
    pub _description: String,
}

impl<'a> TagGrouper {
    fn capture_pattern(&'a self) -> &'a Regex {
        &self._capture_expression
    }
}

impl std::fmt::Debug for TagGrouper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TagGrouper")
            .field("capture_pattern", &self.capture_pattern())
            .finish()
    }
}

#[derive(Debug, Clone, Display, Serialize, Deserialize, Hash, Eq, PartialEq, PartialOrd, From)]
#[serde(transparent)]
pub struct TagGroup(Tag);
impl AsRef<str> for TagGroup {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<std::borrow::Cow<'_, str>> for TagGroup {
    fn from(value: std::borrow::Cow<'_, str>) -> Self {
        let tag_string: String = value.into();
        Self(tag_string.into())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, IntoIterator)]
pub struct TagGroups {
    pub capture_pattern: String,
    #[into_iterator(owned, ref, ref_mut)]
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
                for TagLocation { path, tag_found } in locs {
                    writeln!(
                        f,
                        "      |    |__ 📄 {}:{}..{}",
                        path.display(),
                        tag_found.range_in_body.start(),
                        tag_found.range_in_body.end(),
                    )?;
                }
            }
        }
        write!(f, "")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Display)]
#[display("{}\n\n{}", tag_index, clusters)]
pub struct ClusteredTagIndex {
    pub tag_index: TagIndex,
    pub clusters: ClusteringProduct,
}

impl ClusteredTagIndex {
    pub fn new(tag_index: TagIndex, clusters: ClusteringProduct) -> Self {
        Self {
            tag_index,
            clusters,
        }
    }

    pub fn tags(&self) -> Tags {
        self.tag_index.tags()
    }

}


#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
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
            for TagLocation { path, tag_found } in locs.clone() {
                // hacky
                writeln!(
                    f,
                    "     |__ 📄 {}:{}..{}",
                    path.display(),
                    tag_found.range_in_body.start(),
                    tag_found.range_in_body.end(),
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

    /// The tags that are present in the index.
    pub fn tags(&self) -> Tags {
        self.tags_to_locations
            .inner
            .keys()
            .cloned()
            .collect::<Vec<Tag>>()
            .into()
    }
    pub fn add_tag_location(&mut self, path: PathBuf, tag_found: TagFound) {
        let entry = self
            .tags_to_locations
            .entry(tag_found.tag.to_owned())
            .or_default();
        let location = TagLocation {
            path,
            tag_found,
        };
        entry.push(&location)
    }

    pub fn group_keys(
        &self,
        group_capture_expression: &Regex,
        group_key_constructor: impl Fn(&Captures) -> String,
        description: &str,
    ) -> TagGroups {
        let mut groups = HashMap::default();

        // uglee
        for (tag, tag_locations) in self.tags_to_locations.clone() {
            let group_key: TagGroup = group_capture_expression
                .replace(tag.as_ref(), &group_key_constructor)
                .into();
            let entry: &mut TagsToLocations = groups.entry(group_key).or_default();
            let _ = entry.insert(tag, tag_locations);
        }

        TagGroups {
            capture_pattern: group_capture_expression.as_str().to_string(),
            groups,
            description: description.to_owned(),
        }
    }
}

impl Add for TagIndex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        TagIndex {
            root_path: self.root_path,
            tags_to_locations: HashMap::from_iter(
                self.tags_to_locations
                    .into_iter()
                    .chain(rhs.tags_to_locations),
            )
            .into(),
        }
    }
}

/// Represents a fully-formed tag
#[derive(
    Default,
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Hash,
    Display,
    Serialize,
    Deserialize,
    AsRef,
    From,
    FromStr,
)]
#[as_ref(str, [u8], String)]
#[serde(transparent)]
pub struct Tag {
    inner: String,
}

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Default,
    IntoIterator,
    From,
    AsRef,
    Deref,
    DerefMut,
    PartialEq,
    Eq,
)]
#[deref(forward)]
#[deref_mut(forward)]
#[as_ref(forward)]
pub struct Tags(Vec<Tag>);
impl Tags {
    pub fn push(&mut self, value: Tag) {
        self.0.push(value)
    }
    pub fn append(&mut self, mut value: Tags) {
        self.0.append(&mut value.0)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Newtype wrapper around a vec of TextRanges.
#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Default,
    IntoIterator,
    From,
    Deref,
    DerefMut,
    PartialEq,
    Eq,
)]
#[deref(forward)]
#[deref_mut(forward)]
pub struct TextRanges {
    inner: Vec<StrRange>,
}

impl TextRanges {
    pub fn push(&mut self, value: StrRange) {
        self.inner.push(value)
    }

    pub fn append(&mut self, other: &mut Self) {
        self.inner.append(&mut other.inner)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

/// Newtype wrapper around a `Range` that refers to a range of _bytes_,
/// as opposed to characters. Note that there is no enherent constraint
/// in the type system that enforces this, however this model introduces
/// the constraints for clarity.
///
/// Under the hood, this is used in two cases:
/// 1. To represent spans returned by the markdown parser, and
/// 2. To represent the results of regex matches.
///
/// In both cases, the range unit is a `usize` which represents a byte index.
/// From [`regex::Match`]:
/// > Since this Match can only be produced by the top-level Regex APIs that only
/// > support searching UTF-8 encoded strings, the byte offsets for a Match are
/// > guaranteed to fall on valid UTF-8 codepoint boundaries. That is, slicing a
/// > &str with Match::range is guaranteed to never panic.
///
/// `pullldown_cmark` doesn't document this directly, but as it's just indexing
/// `str` under the hood, we should be good.
///
#[derive(
    Debug,
    Display,
    Clone,
    Serialize,
    Deserialize,
    Default,
    PartialEq,
    Eq,
    Hash,
    From,
    IntoIterator,
    Index,
    IndexMut,
)]
#[display("{}..{}", self.start(), self.end())]
#[serde(transparent)]
pub struct StrRange {
    inner: Range<usize>,
}
impl StrRange {
    /// The start byte of the tag text within its source.
    /// According to the `regex` crate docs:
    ///
    /// > Returns the byte offset of the start of the match in the haystack. The
    /// > start of the match corresponds to the position where the match begins
    /// > and includes the first byte in the match. > > It is guaranteed that
    /// > Match::start() <= Match::end(). > > This is guaranteed to fall on a
    /// > valid UTF-8 codepoint boundary. That is, it will never be an offset that
    /// > appears between the UTF-8 code units of a UTF-8 encoded Unicode scalar
    /// > value. Consequently, it is always safe to slice the corresponding
    /// > haystack using this offset.
    ///
    /// — https://docs.rs/regex/latest/regex/struct.Match.html#method.start
    fn start(&self) -> usize {
        self.inner.start
    }

    /// > Returns the byte offset of the end of the match in the haystack. The
    /// > end of the match corresponds to the byte immediately following the last
    /// > byte in the match. This means that &slice[start..end] works as one would
    /// > expect. > > It is guaranteed that Match::start() <= Match::end(). > >
    /// > This is guaranteed to fall on a valid UTF-8 codepoint boundary. That is,
    /// > it will never be an offset that appears between the UTF-8 code units of
    /// > a UTF-8 encoded Unicode scalar value. Consequently, it is always safe to
    /// > slice the corresponding haystack using this offset.
    ///
    /// — https://docs.rs/regex/latest/regex/struct.Match.html#method.end
    fn end(&self) -> usize {
        self.inner.end
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Hash)]
pub struct TextRangeInContext<'a> {
    range_bytes: Range<usize>,
    ctx: Cow<'a, str>,
}
