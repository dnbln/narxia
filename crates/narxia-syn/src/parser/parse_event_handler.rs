use std::fmt;

use rowan::Language;

use crate::language::NarxiaLanguage;
use crate::parse_error::ParseError;
use crate::parser::{ColorizeProcedure, ParserDbgStyling};
use crate::syntax_kind::SyntaxKind;
use crate::syntree::GreenTree;
use crate::text_span::TextSpan;
use crate::token_source::TokenRepr;

pub struct ParseEventHandlerPos(usize);

#[derive(Debug)]
pub struct ParseEventHandler {
    events: Vec<ParseEvent>,
}

impl ParseEventHandler {
    pub(crate) fn new() -> Self {
        Self { events: Vec::new() }
    }
}

#[must_use]
pub struct Marker(usize, #[cfg(debug_assertions)] drop_bomb::DropBomb);

impl Marker {
    fn new(idx: usize) -> Self {
        Self(
            idx,
            #[cfg(debug_assertions)]
            drop_bomb::DropBomb::new("Marker must be either completed or abandoned"),
        )
    }
}

pub struct CompletedMarker {
    idx: usize,
    idx_end: usize,
}

impl ParseEventHandler {
    #[inline(always)]
    pub fn begin(&mut self) -> Marker {
        let idx = self.events.len();
        self.events.push(ParseEvent::Begin);
        Marker::new(idx)
    }

    #[inline(always)]
    pub fn token(&mut self, kind: SyntaxKind, span: TextSpan) {
        self.events.push(ParseEvent::Token { trepr: TokenRepr::new(kind, span) });
    }

    pub fn present(
        &self,
        offset: usize,
        count: usize,
        add_absolute_positions: bool,
        styling: ParserDbgStyling,
    ) -> RecentEventPresenter {
        RecentEventPresenter {
            internal: self,
            offset,
            count,
            add_absolute_positions,
            styling,
        }
    }

    #[inline(always)]
    pub fn end(&mut self, mut marker: Marker, kind: SyntaxKind) -> CompletedMarker {
        let idx_end = self.events.len();
        self.events.push(ParseEvent::End { kind });
        #[cfg(debug_assertions)]
        marker.1.defuse();
        CompletedMarker {
            idx: marker.0,
            idx_end,
        }
    }

    pub fn abandon(&mut self, mut marker: Marker) {
        self.events[marker.0] = ParseEvent::Tombstone;
        #[cfg(debug_assertions)]
        marker.1.defuse();
    }

    pub fn undo_complete(&mut self, marker: CompletedMarker) -> (Marker, SyntaxKind) {
        let old_kind = match self.events[marker.idx_end] {
            ParseEvent::End { kind } => kind,
            _ => unreachable!(),
        };
        self.events[marker.idx_end] = ParseEvent::Tombstone;
        (Marker::new(marker.idx), old_kind)
    }

    pub fn abandon_complete(&mut self, marker: CompletedMarker) {
        self.events[marker.idx] = ParseEvent::Tombstone;
        self.events[marker.idx_end] = ParseEvent::Tombstone;
    }

    pub fn precede(&mut self, marker: &Marker) -> Marker {
        let idx = self.events.len();
        self.events.push(ParseEvent::Precede { idx: marker.0 });
        Marker::new(idx)
    }

    pub fn precede_completed(&mut self, marker: &CompletedMarker) -> Marker {
        let idx = self.events.len();
        self.events.push(ParseEvent::Precede { idx: marker.idx });
        Marker::new(idx)
    }

    pub fn error(&mut self, error: ParseError) {
        self.events.push(ParseEvent::Error(Box::new(error)));
    }

    pub fn finish<'a>(self, tb: &mut dyn TreeBuilder<'a>) {
        let mut stack = Vec::new();

        enum CompiledParseEvent {
            Begin {
                kind: SyntaxKind,
                precede: Vec<usize>,
                preceding: bool,
            },
            Token(TokenRepr),
            Error(ParseError),
            End,
        }

        let mut compiled_events = Vec::new();

        for event in self.events {
            match event {
                ParseEvent::Begin => {
                    stack.push(compiled_events.len());
                    compiled_events.push(CompiledParseEvent::Begin {
                        kind: SyntaxKind::__TOMBSTONE,
                        precede: Vec::new(),
                        preceding: false,
                    });
                }
                ParseEvent::Token { trepr } => {
                    compiled_events.push(CompiledParseEvent::Token(trepr));
                }
                ParseEvent::End { kind } => {
                    let start = stack.pop().unwrap();
                    let CompiledParseEvent::Begin { kind: k, .. } = &mut compiled_events[start]
                    else {
                        unreachable!();
                    };
                    *k = kind;
                    compiled_events.push(CompiledParseEvent::End);
                }
                ParseEvent::Precede { idx } => {
                    let ptr = compiled_events.len();
                    let CompiledParseEvent::Begin { precede, .. } = &mut compiled_events[idx]
                    else {
                        unreachable!();
                    };
                    precede.push(ptr);
                    compiled_events.push(CompiledParseEvent::Begin {
                        kind: SyntaxKind::__TOMBSTONE,
                        precede: Vec::new(),
                        preceding: true,
                    });
                    if let Some(p) = stack.iter().position(|p| *p == idx) {
                        // we are preceding a node that is already on the stack (normal precede).
                        stack.insert(p, ptr);
                    } else {
                        // we are preceding a node that is not already on the stack
                        // (it was completed, see precede_completed).
                        stack.push(ptr);
                    }
                }
                ParseEvent::Error(error) => {
                    compiled_events.push(CompiledParseEvent::Error(*error));
                }
                ParseEvent::Tombstone => {}
            }
        }

        assert!(stack.is_empty());

        fn handle_precede<'a>(
            tb: &mut dyn TreeBuilder<'a>,
            kind: SyntaxKind,
            precede: &[usize],
            compiled_events: &[CompiledParseEvent],
        ) {
            for &idx in precede {
                match &compiled_events[idx] {
                    CompiledParseEvent::Begin { kind, precede, .. } => {
                        handle_precede(tb, *kind, precede, compiled_events);
                    }
                    CompiledParseEvent::Token(..) => {}
                    CompiledParseEvent::Error(_error) => {}
                    CompiledParseEvent::End => {}
                }
            }
            tb.start_node(kind);
        }

        for e in compiled_events.iter() {
            match e {
                CompiledParseEvent::Begin {
                    kind,
                    precede,
                    preceding,
                } => {
                    if *preceding {
                        continue;
                    }

                    handle_precede(tb, *kind, precede, &compiled_events);
                }
                CompiledParseEvent::Token(trepr) => {
                    tb.token(trepr.kind(), trepr.span());
                }
                CompiledParseEvent::Error(error) => {
                    tb.error(error.clone());
                }
                CompiledParseEvent::End => {
                    tb.finish_node();
                }
            }
        }
    }

    pub fn state(&self) -> ParseEventHandlerPos {
        ParseEventHandlerPos(self.events.len())
    }

    #[cfg(debug_assertions)]
    #[track_caller]
    fn assert_safe_rollback(&self, pos: &ParseEventHandlerPos) {
        let mut stk = Vec::new();
        for ev in &self.events[pos.0..] {
            match ev {
                ParseEvent::Begin => {
                    stk.push(());
                }
                ParseEvent::End { .. } => {
                    assert!(!stk.is_empty(), "End after pos to Begin event before pos.");
                    stk.pop().unwrap();
                }
                ParseEvent::Precede { .. } => {
                    stk.push(());
                }
                ParseEvent::Token { .. } => {}
                ParseEvent::Error(..) => {}
                ParseEvent::Tombstone => {}
            }
        }
        assert!(stk.is_empty(), "Begin / Precede after pos not finished.");
    }

    #[track_caller]
    pub fn rollback(&mut self, pos: ParseEventHandlerPos) {
        #[cfg(debug_assertions)]
        self.assert_safe_rollback(&pos);

        self.events.truncate(pos.0);
    }
}

pub trait TreeBuilder<'a> {
    fn start_node(&mut self, kind: SyntaxKind);
    fn finish_node(&mut self);
    fn token(&mut self, kind: SyntaxKind, span: TextSpan);
    fn error(&mut self, error: ParseError);
}

pub struct GreenTreeBuilder<'a> {
    tb: rowan::GreenNodeBuilder<'a>,
    src: Box<dyn Fn(TextSpan) -> &'a str + 'a>,
    parse_errors: Vec<ParseError>,
}

impl<'a> GreenTreeBuilder<'a> {
    pub(crate) fn new(src: Box<dyn Fn(TextSpan) -> &'a str + 'a>) -> Self {
        Self {
            tb: rowan::GreenNodeBuilder::new(),
            src,
            parse_errors: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> (GreenTree, Vec<ParseError>) {
        (GreenTree::new(self.tb.finish()), self.parse_errors)
    }
}

impl<'a> TreeBuilder<'a> for GreenTreeBuilder<'a> {
    fn start_node(&mut self, kind: SyntaxKind) {
        self.tb.start_node(NarxiaLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.tb.finish_node();
    }

    fn token(&mut self, kind: SyntaxKind, span: TextSpan) {
        self.tb
            .token(NarxiaLanguage::kind_to_raw(kind), (self.src)(span));
    }

    fn error(&mut self, error: ParseError) {
        self.parse_errors.push(error);
    }
}

pub struct GreenTreeBuilderSD<'a, F: Fn(TextSpan) -> &'a str + 'a> {
    tb: rowan::GreenNodeBuilder<'a>,
    src: F,
    parse_errors: Vec<ParseError>,
}

impl<'a, F> GreenTreeBuilderSD<'a, F>
where
    F: Fn(TextSpan) -> &'a str + 'a,
{
    pub(crate) fn new(src: F) -> Self {
        Self {
            tb: rowan::GreenNodeBuilder::new(),
            src,
            parse_errors: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> (GreenTree, Vec<ParseError>) {
        (GreenTree::new(self.tb.finish()), self.parse_errors)
    }
}

impl<'a, F> TreeBuilder<'a> for GreenTreeBuilderSD<'a, F>
where
    F: Fn(TextSpan) -> &'a str + 'a,
{
    fn start_node(&mut self, kind: SyntaxKind) {
        self.tb.start_node(NarxiaLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.tb.finish_node();
    }

    fn token(&mut self, kind: SyntaxKind, span: TextSpan) {
        self.tb
            .token(NarxiaLanguage::kind_to_raw(kind), (self.src)(span));
    }

    fn error(&mut self, error: ParseError) {
        self.parse_errors.push(error);
    }
}

#[derive(Debug)]
enum ParseEvent {
    Begin,
    Token { trepr: TokenRepr },
    Error(Box<ParseError>),
    End { kind: SyntaxKind },
    Precede { idx: usize },
    Tombstone,
}

impl fmt::Display for ParseEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseEvent::Begin => write!(f, "Begin"),
            ParseEvent::Token { trepr } => {
                write!(f, "Token({:?}, @{})", trepr.kind(), trepr.span())
            }
            ParseEvent::Error(error) => write!(f, "Error({:?})", error),
            ParseEvent::End { kind } => write!(f, "End({:?})", kind),
            ParseEvent::Precede { idx } => write!(f, "Precede({})", idx),
            ParseEvent::Tombstone => write!(f, "Tombstone"),
        }
    }
}

pub struct RecentEventPresenter<'a> {
    internal: &'a ParseEventHandler,
    offset: usize,
    count: usize,
    add_absolute_positions: bool,
    styling: ParserDbgStyling,
}

impl<'a> fmt::Display for RecentEventPresenter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut start = 0;
        if self.count <= self.internal.events.len() {
            start = self.internal.events.len() - self.count;
        }
        let end = self.internal.events.len();
        let actual_count = end - start;

        for (index, event) in self.internal.events[start..end].iter().enumerate() {
            writeln!(
                f,
                "{:offset$}{}[-{:3}] {}",
                "",
                if self.add_absolute_positions {
                    format!(
                        "@{} ",
                        self.styling
                            .recent_event_absolute_position
                            .colorize(format!("{:3}", start + index))
                    )
                } else {
                    "".to_owned()
                },
                self.styling
                    .recent_event_relative_position
                    .colorize(format!("{}", actual_count - index - 1)),
                self.styling.recent_event_kind.colorize(format!("{event}")),
                offset = self.offset
            )?;
        }

        Ok(())
    }
}
