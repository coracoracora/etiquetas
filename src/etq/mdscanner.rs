use std::borrow::Cow;

use derive_more::IntoIterator;
use pulldown_cmark::{OffsetIter, Parser, TextMergeWithOffset};

use super::model::{Tag, TextRange};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TagFound<'a> {
    /// The tag found
    pub tag: Tag,

    /// The range of the tag, in bytes, with respect to the body of the content being parsed.
    range_in_body: TextRange,

    /// Text
    found_in_text: Cow<'a, str>,

    /// The range of the tag, in bytes, in the element.
    range_in_text: TextRange,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, IntoIterator)]
pub struct TagsFound<'a>(pub Vec<TagFound<'a>>);

impl<'a> FromIterator<TagFound<'a>> for TagsFound<'a> {
    fn from_iter<T: IntoIterator<Item = TagFound<'a>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a> TagsFound<'a> {
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn push(&mut self, other: TagFound<'a>) {
        self.0.push(other)
    }
    pub fn append(&mut self, other: &mut TagsFound<'a>) {
        self.0.append(&mut other.0)
    }
}

#[derive(Debug, Clone)]
pub enum ScanEvent<'a> {
    /// We've found a tag at the given path in the current file.
    TagsFound(TagsFound<'a>),
}

// #[derive(Debug, Clone, Default)]
// pub struct MarkdownScannerState {}

#[derive(Debug)]
pub struct MarkdownScanner<'a, M, I>
where
    I: IntoIterator<Item = (Tag, TextRange)>,
    M: FnMut(&str) -> I,
{
    parser: TextMergeWithOffset<'a, OffsetIter<'a>>,
    matcher: M,
    // state: MarkdownScannerState,
}

impl<'a, M, I> MarkdownScanner<'a, M, I>
where
    M: FnMut(&str) -> I,
    I: IntoIterator<Item = (Tag, TextRange)>,
{
    pub fn new(input: &'a str, matcher: M) -> Self {
        Self {
            parser: TextMergeWithOffset::new(Parser::new(input).into_offset_iter()),
            matcher,
            // state: MarkdownScannerState::default(),
        }
    }
}

impl<'a, M, I> Iterator for MarkdownScanner<'a, M, I>
where
    M: FnMut(&str) -> I,
    I: IntoIterator<Item = (Tag, TextRange)>,
{
    type Item = ScanEvent<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut acc = TagsFound::default();

        loop {
            let (event, range) = self.parser.next()?;
            // println!("MDEVENT: {:?}", event);
            match event {
                pulldown_cmark::Event::Text(cow_str) | pulldown_cmark::Event::Code(cow_str) => {
                    let context_range_in_body: TextRange = range.into();
                    let mut matches: TagsFound = (self.matcher)(&cow_str)
                        .into_iter()
                        .map(|(t, r)| TagFound {
                            tag: t,
                            range_in_body: context_range_in_body.clone(),
                            found_in_text: cow_str.clone().into(),
                            range_in_text: r,
                        })
                        .collect();

                    acc.append(&mut matches);
                }
                pulldown_cmark::Event::End(_) => {
                    if !acc.is_empty() {
                        return Some(ScanEvent::TagsFound(acc));
                    }
                }

                _ => {
                    // println!("Ignoring event: {:?}", other)
                }
            }
        }
    }
}
