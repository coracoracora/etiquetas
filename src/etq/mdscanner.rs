use std::{fmt::Display, marker::PhantomData};

use derive_more::{Display, IntoIterator};
use pulldown_cmark::{Event, HeadingLevel, OffsetIter, Parser, Tag as CMTag, TextMergeWithOffset};
use serde::{Deserialize, Serialize};

use super::model::{Tag, StrRange};

// ==================================================================
// Event Bookender Typestatey Builder
//
// This is a completely overengineered builder to capture a multi-event element
// in whose consistency is guaranteed by the type system.
//
//

/// Trate to which all supported events are constrained.
trait EventGroupMember {}

trait EventHelpers {
    fn is_heading_start(&self) -> bool;
    fn is_heading_end(&self) -> bool;

    fn is_heading_inter(&self) -> bool {
        !(self.is_heading_start() || self.is_heading_end())
    }
}

impl EventHelpers for Event<'_> {
    fn is_heading_start(&self) -> bool {
        matches!(
            self,
            Event::Start(CMTag::Heading {
                level: _,
                id: _,
                classes: _,
                attrs: _
            })
        )
    }

    fn is_heading_end(&self) -> bool {
        matches!(self, Event::End(pulldown_cmark::TagEnd::Heading(_)))
    }
}

/// Wraps a pulldown_cmark Start event.
#[derive(Debug, Clone, PartialEq)]
pub struct StartEvent(Event<'static>);
impl EventGroupMember for StartEvent {}
impl<'a> From<Event<'a>> for StartEvent {
    fn from(value: Event<'a>) -> StartEvent {
        if !value.is_heading_start() {
            panic!("Not a heading start: {:?}", value)
        }
        StartEvent(value.into_static())
    }
}

/// Wraps a pulldown_cmark event that comes between a start and an end.
#[derive(Debug, Clone, PartialEq)]
pub struct InterEvent(Event<'static>);
impl EventGroupMember for InterEvent {}
impl<'a> From<Event<'a>> for InterEvent {
    fn from(value: Event<'a>) -> Self {
        if !value.is_heading_inter() {
            panic!("Not a heading inter: {:?}", value)
        }
        InterEvent(value.into_static())
    }
}

/// Wraps a pulldown_cmark end event.
#[derive(Debug, Clone, PartialEq)]
pub struct EndEvent(Event<'static>);
impl EventGroupMember for EndEvent {}
impl<'a> From<Event<'a>> for EndEvent {
    fn from(value: Event<'a>) -> EndEvent {
        if !value.is_heading_end() {
            panic!("Not a heading end: {:?}", value)
        }
        EndEvent(value.into_static())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Hash, Display)]
#[display("FrozenHeading {{ level: {}, id: {:?}, classes: {:?}, attrs: {:?}, text: {} }}", level, id, classes, attrs, text)]
pub struct FrozenHeading {
    level: HeadingLevel,
    id: Option<String>,
    classes: Vec<String>,
    attrs: Vec<(String, Option<String>)>,
    text: String,
}

impl From<Heading> for FrozenHeading {
    fn from(value: Heading) -> Self {
        value.frozen()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Heading {
    pub start_event: StartEvent,
    pub end_event: EndEvent,
    pub inter_events: Vec<InterEvent>,
}

impl Display for Heading {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}{}",
            self.level(),
            self.text().unwrap_or("<empty>".to_string()),
            self.id().map(|i| format!(" (id: {})", i)).unwrap_or("".to_string())
        )
    }
}

impl std::hash::Hash for Heading {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.level().hash(state);
        self.id().hash(state);
        self.classes().hash(state);
        self.attrs().hash(state);
    }
}

impl Heading {

    /// Freeze this heading into something that is owned and can be serialized.
    pub fn frozen(&self) -> FrozenHeading {
        FrozenHeading {
            level: self.level(),
            id: self.id().map(|s| s.to_string()),
            classes: self.classes().iter().map(|s| s.to_string()).collect(),
            attrs: self
                .attrs()
                .iter()
                .map(|s| (s.0.to_string(), s.1.clone().map(|e| e.to_string())))
                .collect(),
            text: self.text().unwrap_or("<empty>".to_string()),
        }
    }

    /// Returns the level of thi sheading.
    pub fn level(&self) -> HeadingLevel {
        if let Event::Start(CMTag::Heading {
            level,
            id: _,
            classes: _,
            attrs: _,
        }) = &self.start_event.0
        {
            *level
        } else {
            panic!("Failed invarient: self.start_event.0 had no valid level.")
        }
    }

    /// Returns the id of this heading, converted to owned types.
    pub fn id(&self) -> Option<String> {
        if let Event::Start(CMTag::Heading {
            level: _,
            id,
            classes: _,
            attrs: _,
        }) = &self.start_event.0
        {
            id.clone().map(|s| s.to_string())
        } else {
            panic!("Failed invarient: self.start_event.0 had no valid level.")
        }
    }

    /// Returns the classes of this heading, converted to owned types.
    pub fn classes(&self) -> Vec<String> {
        if let Event::Start(CMTag::Heading {
            level: _,
            id: _,
            classes,
            attrs: _,
        }) = &self.start_event.0
        {
            classes.iter().map(|s| s.to_string()).collect()
        } else {
            panic!("Failed invarient: self.start_event.0 had no valid level.")
        }
    }

    /// Returns the attrs of this heading, converted to
    /// owned types.
    pub fn attrs(&self) -> Vec<(String, Option<String>)> {
        if let Event::Start(CMTag::Heading {
            level: _,
            id: _,
            classes: _,
            attrs,
        }) = &self.start_event.0
        {
            // lol this is hideous
            attrs
                .iter()
                .map(|s| (s.0.to_string(), s.1.clone().map(|e| e.to_string())))
                .collect()
        } else {
            panic!("Failed invarient: self.start_event.0 had no valid level.")
        }
    }

    /// Finds the first [`Event::Text`] in inter_events, if any, converts it,
    /// and returns it.
    pub fn text(&self) -> Option<String> {
        self.inter_events.iter().find_map(|e| match e.0 {
            Event::Text(ref t) => Some(t.to_string()),
            _ => None,
        })
    }
}

struct HeadingBuilder<T: EventGroupMember> {
    start_event: StartEvent,
    inter_events: Vec<InterEvent>,
    phantom: PhantomData<T>,
}

impl HeadingBuilder<InterEvent> {
    /// Restart the build process.
    fn start(start_event: StartEvent) -> HeadingBuilder<InterEvent> {
        HeadingBuilder {
            start_event,
            inter_events: Vec::<InterEvent>::default(),
            phantom: PhantomData,
        }
    }

    /// Add an [`InterEvent`] to the prototypical heading.
    /// Modifes `self`; more than one can be added.
    fn add_inter(&mut self, event: InterEvent) -> &mut Self {
        self.inter_events.push(event);
        self
    }

    /// Build the [`Heading`] instance.
    fn end(&mut self, event: EndEvent) -> Heading {
        Heading {
            start_event: self.start_event.clone(),
            end_event: event,
            inter_events: self.inter_events.clone(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Serialize, Deserialize)]
pub struct TagFound {
    /// The tag found
    pub tag: Tag,

    // pub last_heading_seen: String,
    /// The range of the tag, in bytes, with respect to the body of the content being parsed.
    pub range_in_body: StrRange,

    /// Text
    pub found_in_text: String,

    /// The range of the tag, in bytes, in the element.
    pub range_in_text: StrRange,

    /// The heading in which the tag was found
    pub in_heading: Option<FrozenHeading>,
}

impl Display for TagFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at {}{}",
            self.tag,
            self.range_in_body,
            self.in_heading
                .as_ref()
                .map(|h| format!(", under {}", h).to_string())
                .unwrap_or("".to_string())
        )
    }
}

#[derive(Debug, Default, Clone, Hash, PartialEq, IntoIterator)]
pub struct TagsFound(pub Vec<TagFound>);

impl Display for TagsFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl FromIterator<TagFound> for TagsFound {
    fn from_iter<T: IntoIterator<Item = TagFound>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl TagsFound {
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn push(&mut self, other: TagFound) {
        self.0.push(other)
    }
    pub fn append(&mut self, other: &mut TagsFound) {
        self.0.append(&mut other.0)
    }
}

#[derive(Debug, Clone)]
pub enum ScanEvent {
    /// We've found a tag at the given path in the current file.
    TagsFound(TagsFound),
}

// #[derive(Debug, Clone, Default)]
// pub struct MarkdownScannerState {}

#[derive(Debug)]
pub struct MarkdownScanner<'a, M, I>
where
    I: IntoIterator<Item = (Tag, StrRange)>,
    M: FnMut(&str) -> I,
{
    parser: TextMergeWithOffset<'a, OffsetIter<'a>>,
    matcher: M,
    last_heading: Option<Heading>,
    // state: MarkdownScannerState,
}

impl<'a, M, I> MarkdownScanner<'a, M, I>
where
    M: FnMut(&str) -> I,
    I: IntoIterator<Item = (Tag, StrRange)>,
{
    pub fn new(input: &'a str, matcher: M) -> Self {
        Self {
            parser: TextMergeWithOffset::new(Parser::new(input).into_offset_iter()),
            matcher,
            last_heading: None,
            // state: MarkdownScannerState::default(),
        }
    }
}

impl<M, I> Iterator for MarkdownScanner<'_, M, I>
where
    M: FnMut(&str) -> I,
    I: IntoIterator<Item = (Tag, StrRange)>,
{
    type Item = ScanEvent;

    fn next(&mut self) -> Option<Self::Item> {
        let mut acc = TagsFound::default();
        let mut heading_builder = Option::<HeadingBuilder<InterEvent>>::default();

        loop {
            let (event, range) = self.parser.next()?;
            // println!("MDEVENT: {:?}", event);
            match event {
                e @ pulldown_cmark::Event::Start(_) => {
                    if e.is_heading_start() {
                        heading_builder = Some(HeadingBuilder::start(e.into()));
                    }
                }

                pulldown_cmark::Event::Text(ref cow_str)
                | pulldown_cmark::Event::Code(ref cow_str) => {
                    if let Some(ref mut hb) = heading_builder {
                        let _ = hb.add_inter(event.into());
                    } else {
                        let context_range_in_body: StrRange = range.into();
                        let mut matches: TagsFound = (self.matcher)(cow_str)
                            .into_iter()
                            .map(|(t, r)| TagFound {
                                tag: t,
                                range_in_body: context_range_in_body.clone(),
                                found_in_text: cow_str.clone().into(),
                                range_in_text: r,
                                in_heading: self.last_heading.as_ref().map(|m| m.frozen()),
                            })
                            .collect();

                        acc.append(&mut matches);
                    }
                }

                event @ pulldown_cmark::Event::End(_) => {
                    if event.is_heading_end() {
                        if let Some(mut hb) = heading_builder {
                            self.last_heading = Some(hb.end(event.into()));
                            heading_builder = None
                        }
                    } else if !acc.is_empty() {
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
