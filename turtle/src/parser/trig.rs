//! [TriG] parser
//!
//! [TriG]: https://www.w3.org/TR/rdf12-trig

use std::io::BufRead;

use sophia_api::{
    parser::QuadParser,
    quad::Spog,
    source::{Source, StreamError::SinkError, StreamResult},
    version::Version,
};
use sophia_iri::resolve::BaseIriRef;

use crate::{
    _term::StashedTerm,
    lazy_regex,
    parser::{
        _common::{Extra, GenericSource, Inner, State, TxSource},
        _error::{Error, ErrorKind, ResultExt},
    },
};

mod _state;
use _state::*;

sophia_api::def_mod_functions_for_bufread_parser!(TriGParser, QuadParser);

/// [TriG] parser.
///
/// [TriG]: https://www.w3.org/TR/rdf12-trig/
#[derive(Clone, Debug, Default)]
pub struct TriGParser {
    /// The default base IRI, if any.
    ///
    /// Will be applied until overridden with a `@base`/`BASE` directive.
    pub base: Option<BaseIriRef<Box<str>>>,
    /// The default version specifier.
    ///
    /// Will be applied until overridden with a `@version`/`VERSION` directive.
    pub version: Version,
}

impl TriGParser {
    /// Construct a [`TriGParser`] with default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Change the [`base`](TriGParser::base) option of this parser.
    #[must_use]
    pub fn with_base(self, base: Option<BaseIriRef<Box<str>>>) -> Self {
        Self { base, ..self }
    }

    /// Change the [`version`](TriGParser::version) option of this parser.
    #[must_use]
    pub fn with_version(self, version: Version) -> Self {
        Self { version, ..self }
    }
}

impl<I: BufRead> QuadParser<I> for TriGParser {
    type Source = TriGSource<I>;

    fn parse(&self, data: I) -> Self::Source {
        TriGSource {
            input: data,
            inner: Inner {
                version: self.version,
                ..Default::default()
            },
            extra: Extra {
                base: self.base.clone(),
                ..Default::default()
            },
        }
    }
}

/// [`QuadSource`](sophia_api::prelude::QuadSource) returned by a [`TriGParser`]
#[derive(Clone, Debug)]
pub struct TriGSource<I> {
    input: I,
    inner: Inner,
    extra: Extra<TriGState>,
}

impl<I> GenericSource for TriGSource<I> {
    type Output<'x> = Spog<StashedTerm<'x>>;

    fn inner(&self) -> &Inner {
        &self.inner
    }

    fn inner_mut(&mut self) -> &mut Inner {
        &mut self.inner
    }

    fn emit_tuple<C, E, E2>(&self, callback: &mut C, offset: usize) -> StreamResult<usize, E, E2>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E2>,
        E: std::error::Error,
        E2: std::error::Error,
    {
        let g = match self.extra.state[0] {
            TriGState::NamedGraph(has_name) => {
                debug_assert!(has_name);
                debug_assert!(!self.inner.terms.is_empty());
                Some(StashedTerm {
                    terms: &self.inner.terms,
                    buffers: &self.inner.buffers,
                    idx: 0,
                })
            }
            _ => None,
        };
        let spo = StashedTerm::new_triple(&self.inner.terms, &self.inner.buffers);
        callback((spo, g)).map_err(SinkError).map(|_| offset)
    }
}

impl<I> TxSource<TriGState> for TriGSource<I> {
    fn extra(&self) -> &Extra<TriGState> {
        &self.extra
    }

    fn extra_mut(&mut self) -> &mut Extra<TriGState> {
        &mut self.extra
    }

    fn inner_and_extra_mut(&mut self) -> (&mut Inner, &mut Extra<TriGState>) {
        (&mut self.inner, &mut self.extra)
    }

    fn emit_reification_tuple<C, E, E2>(
        &self,
        callback: &mut C,
        offset: usize,
    ) -> StreamResult<usize, E, E2>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E2>,
        E: std::error::Error,
        E2: std::error::Error,
    {
        debug_assert!(!self.extra.state.is_empty());
        let g = match self.extra.state[0] {
            TriGState::NamedGraph(has_name) => {
                debug_assert!(has_name);
                debug_assert!(!self.inner.terms.is_empty());
                Some(StashedTerm {
                    terms: &self.inner.terms,
                    buffers: &self.inner.buffers,
                    idx: 0,
                })
            }
            _ => None,
        };
        let [o, p, s] = StashedTerm::new_triple(&self.inner.terms, &self.inner.buffers);
        callback(([s, p, o], g)).map_err(SinkError).map(|_| offset)
    }

    fn parse_token<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        use TriGState::*;

        debug_assert!(
            !txt.is_empty()
                && (self.ws(txt) == 0
                    || matches!(self.extra.state.last(), Some(StringLiteralLong(_))))
        );

        match self.extra.state.last() {
            None => self.enter_trig_doc(txt),
            Some(DefaultGraph) => self.in_default_graph(txt),
            Some(NamedGraph(has_name)) => self.in_named_graph(txt, *has_name),
            Some(Block(square_bracket)) => self.in_block(txt, *square_bracket),
            Some(LabelNotSubject) => self.after_label_not_subject(txt),
            Some(AtDirective) => self.end_at_directive(txt),
            Some(BaseIri) => self.in_base_iri(txt),
            Some(PrefixDeclaration) => self.in_prefix_declaration(txt),
            Some(PrefixIri) => self.in_prefix_iri(txt),
            Some(VersionSpecifier) => self.in_version_specifier(txt),
            Some(Triples(require_pol)) => self.in_triples(txt, *require_pol),
            Some(PredicateObjectList) => self.in_predicate_object_list(txt),
            Some(ObjectList) => self.in_object_list(txt, callback),
            Some(Verb) => self.exit_verb(txt, callback),
            Some(Object) => self.exit_object(txt, callback),
            Some(BlankNodePropertyListOrAnon) => self.in_blank_node_property_list_or_anon(txt),
            Some(Collection) => self.in_collection(txt, callback),
            Some(RdfLiteral(dt)) => self.in_rdf_literal(txt, *dt),
            Some(Reifier) => self.in_reifier(txt, callback),
            Some(ReifiedTriple(true)) => self.enter_rt_subject(txt),
            Some(ReifiedTriple(false)) => self.exit_reified_triple(txt),
            Some(RtSubject) => self.exit_rt_subject(txt),
            Some(RtObject) => self.exit_rt_object(txt, callback),
            Some(TripleTerm) => self.enter_tt_subject(txt),
            Some(TtSubject) => self.exit_tt_subject(txt),
            Some(TtObject) => self.exit_tt_object(txt),
            Some(AnnotationBlock) => self.enter_predicate_object_list(txt),
            Some(StringLiteralLong(quote)) => self.in_string_literal_long(txt, *quote),
            Some(Anon(false)) => self.in_anon(txt),
            Some(Anon(true)) => self.in_anon_reifier(txt, callback),
        }
    }

    fn in_predicate_object_list<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if txt.starts_with('}') {
            if !(3..=4).contains(&self.extra.state.len()) {
                return Err(ErrorKind::Expected("verb, ';' or closing bracket".into()))
                    .wrap_in(self);
            }
            debug_assert!(
                self.extra.state[self.extra.state.len() - 2].in_triples(),
                "{:?}",
                self.extra.state
            );
            debug_assert!(
                self.extra.state.len() == 3
                    || matches!(
                        self.extra.state[self.extra.state.len() - 3],
                        TriGState::DefaultGraph | TriGState::NamedGraph(true)
                    ),
                "{:?}",
                self.extra.state
            );
            self.pop_triple_components(1);
            if self.extra.state[0] == TriGState::NamedGraph(true) {
                self.pop_triple_components(1);
            }
            debug_assert!(self.inner.terms.is_empty());
            for _ in 0..3 {
                self.extra.state.pop();
            }
            Ok(1)
        } else {
            self.in_predicate_object_list_common(txt)
        }
    }

    fn in_object_list<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        if txt.starts_with('}') {
            if self.extra.state.len() != 4 {
                return Err(ErrorKind::Expected(
                    "reifier, annotation, punctuation".into(),
                ))
                .wrap_in(self);
            }
            debug_assert!(self.extra.state[self.extra.state.len() - 2].in_predicate_object_list());
            debug_assert!(self.extra.state[self.extra.state.len() - 3].in_triples());
            debug_assert!(
                self.extra.state.len() == 3
                    || matches!(
                        self.extra.state[self.extra.state.len() - 4],
                        TriGState::DefaultGraph | TriGState::NamedGraph(true)
                    ),
                "{:?}",
                self.extra.state
            );
            self.pop_triple_components(3);
            if self.extra.state[0] == TriGState::NamedGraph(true) {
                self.pop_triple_components(1);
            }
            debug_assert!(self.inner.terms.is_empty());
            for _ in 0..self.extra.state.len() {
                self.extra.state.pop();
            }
            Ok(1)
        } else {
            self.in_object_list_common::<E, C>(txt, callback)
        }
    }

    /// Expect the final '.' at the end of a `triples` production.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with '.'.
    ///
    /// In TriG, in a graph context, the final period before the closing bracket '}' can be omitted.
    fn end_triples<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty());
        debug_assert!((1..=2).contains(&self.extra.state.len()));
        debug_assert!(matches!(
            self.extra.state.last(),
            Some(&TriGState::Triples(false))
        ));

        if self.extra.state.len() == 1 {
            if txt.starts_with('.') {
                self.extra.state.pop();
                self.pop_term();
                Ok(1)
            } else {
                Err(ErrorKind::Expected("'.'".into())).wrap_in(self)
            }
        } else {
            debug_assert!(matches!(
                self.extra.state[0],
                TriGState::DefaultGraph | TriGState::NamedGraph(true)
            ));
            let close_graph = match txt.as_bytes()[0] {
                b'.' => false,
                b'}' => true,
                _ => return Err(ErrorKind::Expected("'.' or '}'".into())).wrap_in(self),
            };
            self.extra.state.pop();
            self.pop_term();
            if close_graph {
                self.extra.state.pop();
            }
            Ok(1)
        }
    }
}

impl<I: BufRead> Source for TriGSource<I> {
    type Item<'x> = Spog<StashedTerm<'x>>;

    type Error = Error;

    fn try_for_some_item<E, F>(&mut self, f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        let mut line = String::with_capacity(1024);
        self.inner.col = 0;
        let read = self.input.read_line(&mut line).wrap_in(self)?;
        if read == 0 {
            if let Some(state) = self.extra.state.pop() {
                Err(ErrorKind::EOF(format!("{state:?}"))).wrap_in(self)
            } else {
                Ok(false)
            }
        } else {
            self.parse_line(&line, f)?;
            self.inner.line += 1;
            Ok(true)
        }
    }
}

impl<I> TriGSource<I> {
    #[cfg(test)]
    fn new(input: I) -> Self {
        TriGSource {
            input,
            inner: Default::default(),
            extra: Default::default(),
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-trig/#grammar-production-trigDoc
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_trig_doc<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(self.extra.state.last().is_none(), "{:?}", self.extra.state);

        match txt.as_bytes()[0] {
            b'@' => self.enter_at_keyword(txt),
            b'<' => {
                if txt[1..].starts_with('<') {
                    self.extra.state.push(TriGState::Triples(false));
                    self.extra.state.push(TriGState::ReifiedTriple(true));
                    Ok(2)
                } else {
                    self.extra.state.push(TriGState::Block(false));
                    self.iriref(txt)
                }
            }
            b'[' => {
                self.extra.state.push(TriGState::Block(true));
                self.mint_bnode_label();
                Ok(1)
            }
            b'_' => {
                self.extra.state.push(TriGState::Block(false));
                self.blank_node_label(txt)
            }
            b'(' => {
                self.extra.state.push(TriGState::Triples(true));
                self.extra.state.push(TriGState::Collection);
                Ok(1)
            }
            b'{' => {
                self.extra.state.push(TriGState::DefaultGraph);
                Ok(1)
            }
            _ => {
                lazy_regex!(
                    SPARQL_KW = [
                        r"^(?i)BASE[ \n\r\t#<]",
                        r"^(?i)PREFIX[ \n\r\t#]",
                        r#"^(?i)VERSION[ \n\r\t#"']"#,
                        r"^(?i)GRAPH[ \n\r\t#<]"
                    ]
                );
                match SPARQL_KW.matches(txt).iter().next() {
                    Some(0) => {
                        self.extra.state.push(TriGState::BaseIri);
                        Ok("BASE".len())
                    }
                    Some(1) => {
                        self.extra.state.push(TriGState::PrefixDeclaration);
                        Ok("PREFIX".len())
                    }
                    Some(2) => {
                        self.extra.state.push(TriGState::VersionSpecifier);
                        Ok("VERSION".len())
                    }
                    Some(3) => {
                        self.extra.state.push(TriGState::NamedGraph(false));
                        Ok("GRAPH".len())
                    }
                    None => {
                        self.extra.state.push(TriGState::Block(false));
                        self.prefixed_name(txt)
                    }
                    Some(_) => unreachable!(),
                }
            }
        }
    }

    /// Expect any production corresponding to a subject in the default graph.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_default_graph<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(matches!(
            self.extra.state.last(),
            Some(TriGState::DefaultGraph)
        ));
        debug_assert!(self.extra.state.len() == 1);
        if txt.starts_with("}") {
            self.extra.state.pop();
            Ok(1)
        } else {
            self.subject(txt)
        }
    }

    /// Expect any production corresponding to a subject.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    self.extra.state.push(TriGState::Triples(false));
                    self.extra.state.push(TriGState::ReifiedTriple(true));
                    Ok(2)
                } else {
                    self.extra.state.push(TriGState::Triples(true));
                    self.iriref(txt)
                }
            }
            b'[' => {
                self.extra.state.push(TriGState::Triples(false));
                self.extra
                    .state
                    .push(TriGState::BlankNodePropertyListOrAnon);
                self.mint_bnode_label();
                Ok(1)
            }
            b'_' => {
                self.extra.state.push(TriGState::Triples(true));
                self.blank_node_label(txt)
            }
            b'(' => {
                self.extra.state.push(TriGState::Triples(true));
                self.extra.state.push(TriGState::Collection);
                Ok(1)
            }
            _ => {
                self.extra.state.push(TriGState::Triples(true));
                self.prefixed_name(txt)
            }
        }
    }

    /// Expect any production corresponding to a named graph.
    ///
    /// If `has_name` is false, the graph name is expected,
    /// otherwise, a subject is expected.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_named_graph<E>(&mut self, txt: &str, has_name: bool) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert_eq!(
            self.extra.state.last(),
            Some(&TriGState::NamedGraph(has_name))
        );
        if !has_name {
            debug_assert!(self.extra.state.len() == 1);
            self.extra.state[0] = TriGState::NamedGraph(true);
            self.extra.state.push(TriGState::LabelNotSubject);
            match txt.as_bytes()[0] {
                b'<' => self.iriref(txt),
                b'[' => {
                    self.extra.state.push(TriGState::Anon(false));
                    Ok(1)
                }
                b'_' => self.blank_node_label(txt),
                _ => self.prefixed_name(txt),
            }
        } else {
            debug_assert!(has_name);
            if txt.starts_with("}") {
                self.extra.state.pop();
                self.pop_triple_components(1);
                Ok(1)
            } else {
                self.subject(txt)
            }
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-trig/#grammar-production-block
    /// possibly after reading a leading '[' (if square_bracket is true).
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_block<E>(&mut self, txt: &str, square_bracket: bool) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        let state = &mut self.extra.state;
        debug_assert_eq!(state.last(), Some(&TriGState::Block(square_bracket)));
        debug_assert_eq!(state.len(), 1);
        #[allow(clippy::collapsible_else_if)]
        if square_bracket {
            if txt.starts_with(']') {
                state[0] = TriGState::Block(false);
                Ok(1)
            } else {
                state[0] = TriGState::Triples(false);
                state.push(TriGState::BlankNodePropertyListOrAnon);
                self.enter_predicate_object_list(txt)
            }
        } else {
            debug_assert!(!square_bracket);
            if txt.starts_with('{') {
                state[0] = TriGState::NamedGraph(true);
                Ok(1)
            } else {
                state[0] = TriGState::Triples(true);
                self.enter_predicate_object_list(txt)
            }
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-trig/#grammar-production-labelOrSubject
    /// just after the keyword "GRAPH".
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn after_label_not_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert_eq!(self.extra.state.last(), Some(&TriGState::LabelNotSubject));
        if txt.starts_with('{') {
            self.extra.state.pop();
            Ok(1)
        } else {
            Err(ErrorKind::Expected("'{'".into())).wrap_in(self)
        }
    }

    /// Compute the next context after production https://www.w3.org/TR/rdf12-trig/#grammar-production-verb
    /// based on context.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn exit_verb<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            matches!(self.extra.state.last(), Some(TriGState::Verb)),
            "{:?}",
            self.extra.state
        );

        self.extra.state.pop();
        match self.extra.state.last() {
            Some(TriGState::PredicateObjectList) => self.enter_object_list(txt, callback),
            Some(TriGState::ReifiedTriple(true)) => self.enter_rt_object(txt),
            Some(TriGState::TripleTerm) => self.enter_tt_object(txt),
            _ => unreachable!(),
        }
    }

    /// Compute the next context after production https://www.w3.org/TR/rdf12-trig/#grammar-production-object,
    /// after emitting the last triple in stack.
    ///
    /// See also [`TriGSource::enter_object`]
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn exit_object<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            matches!(self.extra.state.last(), Some(TriGState::Object)),
            "{:?}",
            self.extra.state
        );

        self.extra.state.pop();
        match self.extra.state.last() {
            Some(TriGState::ObjectList) => {
                self.emit_tuple(callback, 0)?;
                self.in_object_list(txt, callback)
            }
            Some(TriGState::Collection) => {
                self.emit_tuple(callback, 0)?;
                self.grow_collection()?;
                self.in_collection(txt, callback)
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test;
