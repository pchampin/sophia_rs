//! [Turtle] parser
//!
//! [Turtle]: https://www.w3.org/TR/rdf12-turtle

use std::io::BufRead;

use sophia_api::{
    parser::TripleParser,
    source::{Source, StreamError::SinkError, StreamResult},
    version::Version,
};
use sophia_iri::resolve::BaseIriRef;

use crate::{
    _term::StashedTerm,
    lazy_regex,
    parser::{
        _common::{Extra, GenericSource, Inner, TxSource},
        _error::{Error, ErrorKind, ResultExt},
    },
};

mod _state;
use _state::*;

sophia_api::def_mod_functions_for_bufread_parser!(TurtleParser, TripleParser);

/// [Turtle] parser.
///
/// [Turtle]: https://www.w3.org/TR/rdf12-turtle/
#[derive(Clone, Debug, Default)]
pub struct TurtleParser {
    /// The default base IRI, if any.
    ///
    /// Will be applied until overridden with a `@base`/`BASE` directive.
    pub base: Option<BaseIriRef<Box<str>>>,
    /// The default version specifier.
    ///
    /// Will be applied until overridden with a `@version`/`VERSION` directive.
    pub version: Version,
}

impl TurtleParser {
    /// Construct a [`TurtleParser`] with default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Change the [`base`](TurtleParser::base) option of this parser.
    #[must_use]
    pub fn with_base(self, base: Option<BaseIriRef<Box<str>>>) -> Self {
        Self { base, ..self }
    }

    /// Change the [`version`](TurtleParser::version) option of this parser.
    #[must_use]
    pub fn with_version(self, version: Version) -> Self {
        Self { version, ..self }
    }
}

impl<I: BufRead> TripleParser<I> for TurtleParser {
    type Source = TurtleSource<I>;

    fn parse(&self, data: I) -> Self::Source {
        TurtleSource {
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

/// [`TripleSource`](sophia_api::prelude::TripleSource) returned by a [`TurtleParser`]
#[derive(Clone, Debug)]
pub struct TurtleSource<I> {
    input: I,
    inner: Inner,
    extra: Extra<TurtleState>,
}

impl<I> GenericSource for TurtleSource<I> {
    type Output<'x> = [StashedTerm<'x>; 3];

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
        callback(StashedTerm::new_triple(
            &self.inner.terms,
            &self.inner.buffers,
        ))
        .map_err(SinkError)
        .map(|_| offset)
    }
}

impl<I> TxSource<TurtleState> for TurtleSource<I> {
    fn extra(&self) -> &Extra<TurtleState> {
        &self.extra
    }

    fn extra_mut(&mut self) -> &mut Extra<TurtleState> {
        &mut self.extra
    }

    fn inner_and_extra_mut(&mut self) -> (&mut Inner, &mut Extra<TurtleState>) {
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
        let [o, p, s] = StashedTerm::new_triple(&self.inner.terms, &self.inner.buffers);
        callback([s, p, o]).map_err(SinkError).map(|_| offset)
    }

    fn parse_token<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        use TurtleState::*;

        debug_assert!(
            !txt.is_empty()
                && (self.ws(txt) == 0
                    || matches!(self.extra.state.last(), Some(StringLiteralLong(_))))
        );

        match self.extra.state.last() {
            None => self.enter_statement(txt),
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

    /// Expect the final '.' at the end of a `triples` production.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with '.'.
    fn end_triples<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty());
        debug_assert_eq!(self.extra.state.len(), 1);
        debug_assert!(matches!(self.extra.state[0], TurtleState::Triples(false)));

        if txt.starts_with('.') {
            self.extra.state.pop();
            self.pop_term();
            Ok(1)
        } else {
            Err(ErrorKind::Expected("'.'".into())).wrap_in(self)
        }
    }
}

impl<I: BufRead> Source for TurtleSource<I> {
    type Item<'x> = [StashedTerm<'x>; 3];

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

impl<I> TurtleSource<I> {
    #[cfg(test)]
    fn new(input: I) -> Self {
        TurtleSource {
            input,
            inner: Default::default(),
            extra: Default::default(),
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-statement
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_statement<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(self.extra.state.last().is_none(), "{:?}", self.extra.state);

        match txt.as_bytes()[0] {
            b'@' => self.enter_at_keyword(txt),
            b'<' => {
                if txt[1..].starts_with('<') {
                    self.extra.state.push(TurtleState::Triples(false));
                    self.extra.state.push(TurtleState::ReifiedTriple(true));
                    Ok(2)
                } else {
                    self.extra.state.push(TurtleState::Triples(true));
                    self.iriref(txt)
                }
            }
            b'[' => {
                self.extra.state.push(TurtleState::Triples(false));
                self.extra
                    .state
                    .push(TurtleState::BlankNodePropertyListOrAnon);
                self.mint_bnode_label();
                Ok(1)
            }
            b'_' => {
                self.extra.state.push(TurtleState::Triples(true));
                self.blank_node_label(txt)
            }
            b'(' => {
                self.extra.state.push(TurtleState::Triples(true));
                self.extra.state.push(TurtleState::Collection);
                Ok(1)
            }
            _ => {
                lazy_regex!(
                    SPARQL_KW = [
                        r"^(?i)BASE[ \n\r\t#<]",
                        r"^(?i)PREFIX[ \n\r\t#]",
                        r#"^(?i)VERSION[ \n\r\t#"']"#
                    ]
                );
                match SPARQL_KW.matches(txt).iter().next() {
                    Some(0) => {
                        self.extra.state.push(TurtleState::BaseIri);
                        Ok("BASE".len())
                    }
                    Some(1) => {
                        self.extra.state.push(TurtleState::PrefixDeclaration);
                        Ok("PREFIX".len())
                    }
                    Some(2) => {
                        self.extra.state.push(TurtleState::VersionSpecifier);
                        Ok("VERSION".len())
                    }
                    None => {
                        self.extra.state.push(TurtleState::Triples(true));
                        self.prefixed_name(txt)
                    }
                    Some(_) => unreachable!(),
                }
            }
        }
    }

    /// Compute the next context after production https://www.w3.org/TR/rdf12-turtle/#grammar-production-verb
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
            matches!(self.extra.state.last(), Some(TurtleState::Verb)),
            "{:?}",
            self.extra.state
        );

        self.extra.state.pop();
        match self.extra.state.last() {
            Some(TurtleState::PredicateObjectList) => self.enter_object_list(txt, callback),
            Some(TurtleState::ReifiedTriple(true)) => self.enter_rt_object(txt),
            Some(TurtleState::TripleTerm) => self.enter_tt_object(txt),
            _ => unreachable!(),
        }
    }

    /// Compute the next context after production https://www.w3.org/TR/rdf12-turtle/#grammar-production-object,
    /// after emitting the last triple in stack.
    ///
    /// See also [`TurtleSource::enter_object`]
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
            matches!(self.extra.state.last(), Some(TurtleState::Object)),
            "{:?}",
            self.extra.state
        );

        self.extra.state.pop();
        match self.extra.state.last() {
            Some(TurtleState::ObjectList) => {
                self.emit_tuple(callback, 0)?;
                self.in_object_list(txt, callback)
            }
            Some(TurtleState::Collection) => {
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
