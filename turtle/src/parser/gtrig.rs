//! Generalized [TriG] parser
//!
//! [TriG]: https://www.w3.org/TR/rdf12-trig

use std::io::BufRead;

use sophia_api::{
    parser::QuadParser,
    quad::Spog,
    source::{Source, StreamError::SinkError, StreamResult},
    term::IriRef,
    version::Version,
};
use sophia_iri::resolve::BaseIriRef;

use crate::{
    _term::{IndexedTerm, IndexedTermSliceExt, StashedTerm},
    lazy_regex,
    parser::{
        _common::{Extra, GenericSource, Inner, State, TxSource},
        _error::{Error, ErrorKind, ResultExt},
    },
};

mod _state;
use _state::*;

sophia_api::def_mod_functions_for_bufread_parser!(GTriGParser, QuadParser);

/// [TriG] parser.
///
/// [TriG]: https://www.w3.org/TR/rdf12-trig/
#[derive(Clone, Debug, Default)]
pub struct GTriGParser {
    /// The default base IRI, if any.
    ///
    /// Will be applied until overridden with a `@base`/`BASE` directive.
    pub base: Option<BaseIriRef<Box<str>>>,
    /// The default version specifier.
    ///
    /// Will be applied until overridden with a `@version`/`VERSION` directive.
    pub version: Version,
}

impl GTriGParser {
    /// Construct a [`GTriGParser`] with default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Change the [`base`](GTriGParser::base) option of this parser.
    #[must_use]
    pub fn with_base(self, base: Option<BaseIriRef<Box<str>>>) -> Self {
        Self { base, ..self }
    }

    /// Change the [`version`](GTriGParser::version) option of this parser.
    #[must_use]
    pub fn with_version(self, version: Version) -> Self {
        Self { version, ..self }
    }
}

impl<I: BufRead> QuadParser<I> for GTriGParser {
    type Source = GTriGSource<I>;

    fn parse(&self, data: I) -> Self::Source {
        GTriGSource {
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

/// [`QuadSource`](sophia_api::prelude::QuadSource) returned by a [`GTriGParser`]
#[derive(Clone, Debug)]
pub struct GTriGSource<I> {
    input: I,
    inner: Inner,
    extra: Extra<GTriGState>,
}

impl<I> GenericSource for GTriGSource<I> {
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
            GTriGState::NamedGraph(has_name) => {
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
        let spo = StashedTerm::new_generalized_triple(&self.inner.terms, &self.inner.buffers);
        callback((spo, g)).map_err(SinkError).map(|_| offset)
    }
}

impl<I> TxSource<GTriGState> for GTriGSource<I> {
    fn extra(&self) -> &Extra<GTriGState> {
        &self.extra
    }

    fn extra_mut(&mut self) -> &mut Extra<GTriGState> {
        &mut self.extra
    }

    fn inner_and_extra_mut(&mut self) -> (&mut Inner, &mut Extra<GTriGState>) {
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
            GTriGState::NamedGraph(has_name) => {
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
        let [o, p, s] = StashedTerm::new_generalized_triple(&self.inner.terms, &self.inner.buffers);
        callback(([s, p, o], g)).map_err(SinkError).map(|_| offset)
    }

    fn parse_token<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        use GTriGState::*;

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
            Some(Reifier(_)) => self.in_reifier(txt, callback),
            Some(ReifiedTriple(true)) => self.enter_rt_subject(txt),
            Some(ReifiedTriple(false)) => self.exit_reified_triple(txt),
            Some(RtSubject) => self.exit_rt_subject(txt),
            Some(RtObject) => self.exit_rt_object(txt, callback),
            Some(TripleTerm) => self.enter_tt_subject(txt),
            Some(TtSubject) => self.exit_tt_subject(txt),
            Some(TtObject) => self.exit_tt_object(txt),
            Some(AnnotationBlock) => self.enter_predicate_object_list(txt),
            Some(StringLiteralLong(quote)) => self.in_string_literal_long(txt, *quote),
            Some(Anon) => self.in_anon(txt),
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
                        GTriGState::DefaultGraph | GTriGState::NamedGraph(true)
                    ),
                "{:?}",
                self.extra.state
            );
            self.pop_triple_components(1);
            if self.extra.state[0] == GTriGState::NamedGraph(true) {
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
                        GTriGState::DefaultGraph | GTriGState::NamedGraph(true)
                    ),
                "{:?}",
                self.extra.state
            );
            self.pop_triple_components(3);
            if self.extra.state[0] == GTriGState::NamedGraph(true) {
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
            Some(&GTriGState::Triples(false))
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
                GTriGState::DefaultGraph | GTriGState::NamedGraph(true)
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

    // Override the default implementations of some methods of TxSource
    // (beyond the methods overridden by the TriG parser)

    fn enter_verb<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra.state.last().unwrap().in_verb_context(),
            "{:?}",
            self.extra.state
        );
        debug_assert!(self.extra.state.len() > 1);

        lazy_regex!(DELIMITER = r##"^([ \n\r\t\[\(<'"#]|$)"##);

        self.extra.state.push(GTriGState::Verb);
        if let Some(tail) = txt.strip_prefix('a')
            && DELIMITER.is_match(tail)
        {
            self.inner
                .terms
                .push(IndexedTerm::RdfBuiltin(IriRef::new_unchecked("#type")));
            Ok(1)
        } else {
            match self.extra.state[self.extra.state.len() - 2] {
                GTriGState::PredicateObjectList => match self.enter_term(txt)? {
                    (offset, None) => Ok(offset),
                    (offset, Some(GTriGState::RdfLiteral(after_dt))) => {
                        debug_assert_eq!(offset, 0);
                        debug_assert!(!after_dt);
                        self.enter_rdf_literal(txt)
                    }
                    (offset, Some(state)) => {
                        self.extra.state.push(state);
                        Ok(offset)
                    }
                },
                GTriGState::ReifiedTriple(true) => self.enter_simple_term(txt, true),
                GTriGState::TripleTerm => self.enter_simple_term(txt, false),
                _ => unreachable!(),
            }
        }
    }

    fn enter_object<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra.state.last().unwrap().in_object_context(),
            "{:?}",
            self.extra.state
        );

        match self.enter_term(txt)? {
            (offset, None) => self.emit_tuple(callback, offset),
            (offset, Some(GTriGState::RdfLiteral(after_dt))) => {
                debug_assert_eq!(offset, 0);
                debug_assert!(!after_dt);
                self.extra.state.push(GTriGState::Object);
                self.enter_rdf_literal(txt)
            }
            (offset, Some(state)) => {
                self.extra.state.push(GTriGState::Object);
                self.extra.state.push(state);
                Ok(offset)
            }
        }
    }

    fn in_reifier<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);

        // detect any character that can not start a reifier
        // either a bracket or punctuation, or a period *not followed by a digit*
        // (in GTrig numbers such as '.1' can be reifiers)
        lazy_regex!(NO_REIFIER = r"^[~{,;>|]|^\.([^0-9]|$)");

        let Some(GTriGState::Reifier(enter)) = self.extra.state.last_mut() else {
            unreachable!()
        };

        if *enter {
            *enter = false;
            if NO_REIFIER.find(txt).is_some() {
                self.mint_bnode_label();
                self.exit_reifier(callback, 0)
            } else {
                let ret = self.enter_simple_term(txt, false)?;
                if matches!(
                    self.extra.state.last(),
                    Some(GTriGState::Anon | GTriGState::RdfLiteral(_) | GTriGState::TripleTerm)
                ) {
                    Ok(ret)
                } else {
                    self.exit_reifier(callback, ret)
                }
            }
        } else {
            self.exit_reifier(callback, 0)
        }
    }

    fn enter_rt_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra.state.last().unwrap().in_reified_triple(true),
            "{:?}",
            self.extra.state
        );
        self.extra.state.push(GTriGState::RtSubject);
        self.enter_simple_term(txt, true)
    }

    fn enter_rt_object<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra.state.last().unwrap().in_reified_triple(true),
            "{:?}",
            self.extra.state
        );

        self.extra.state.push(GTriGState::RtObject);
        self.enter_simple_term(txt, true)
    }

    fn exit_reified_triple<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_reified_triple(false),
            "{:?}",
            self.extra().state
        );

        if txt.starts_with(">>") {
            self.extra_mut().state.pop();
            // We need to pop out of the term stack the whole triple term
            // but we want to keep the reifier that is currently on the the top of the stack.

            // backup reifier and pop
            let reif_idx = self.inner.terms.len() - 1;
            let pred_idx = self.inner.terms.generalized_predecessor(reif_idx).unwrap();
            let backup = Vec::from(&self.inner.terms[pred_idx + 1..]);
            self.pop_term();

            // pop rdf:reifies and triple term
            debug_assert!(matches!(
                self.inner().terms[self.inner.terms.len() - 1],
                IndexedTerm::Reifier
            ));
            self.inner.terms.pop();
            debug_assert!(matches!(
                self.inner().terms[self.inner.terms.len() - 1],
                IndexedTerm::TripleTerm
            ));
            self.pop_term();

            // recover reifier
            for term in backup {
                use IndexedTerm::*;
                self.inner.terms.push(match term {
                    Iri(i) => Iri(self.inner.buffers.recycle(i)),
                    BlankNode(i) => BlankNode(self.inner.buffers.recycle(i)),
                    TypedLiteral(i, j) => {
                        let new_i = self.inner.buffers.recycle(i);
                        let new_j = self.inner.buffers.recycle(j);
                        TypedLiteral(new_i, new_j)
                    }
                    LangString(i, j, base_direction) => {
                        let new_i = self.inner.buffers.recycle(i);
                        let new_j = self.inner.buffers.recycle(j);
                        LangString(new_i, new_j, base_direction)
                    }
                    XsdLiteral(i, iri_ref) => XsdLiteral(self.inner.buffers.recycle(i), iri_ref),
                    Variable(i) => Variable(self.inner.buffers.recycle(i)),
                    RdfBuiltin(_) | BoolLiteral(_) | TripleTerm | Reifier | Rest => term,
                })
            }
            Ok(2)
        } else {
            Err(ErrorKind::Expected("closing bracket '>>'".into())).wrap_in(self)
        }
    }

    fn enter_tt_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_triple_term(),
            "{:?}",
            self.extra().state
        );
        self.extra_mut().state.push(GTriGState::TtSubject);
        self.enter_simple_term(txt, false)
    }

    fn enter_tt_object<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_triple_term(),
            "{:?}",
            self.extra().state
        );
        self.extra_mut().state.push(GTriGState::TtObject);
        self.enter_simple_term(txt, false)
    }

    fn in_anon_reifier<E, C>(
        &mut self,
        _txt: &str,
        _callback: &mut C,
    ) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        unreachable!()
        // not used anymore, the processing of ANON reifiers as been moved to in_reifier
    }
}

impl<I: BufRead> Source for GTriGSource<I> {
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

impl<I> GTriGSource<I> {
    #[cfg(test)]
    fn new(input: I) -> Self {
        GTriGSource {
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

        lazy_regex!(
            TRIG_DOC_START = [
                r"^(?i)BASE[ \n\r\t#<]",
                r"^(?i)PREFIX[ \n\r\t#]",
                r#"^(?i)VERSION[ \n\r\t#"']"#,
                r"^(?i)GRAPH[ \n\r\t#<]",
                r"^@",
                r"^\{"
            ]
        );
        match TRIG_DOC_START.matches(txt).iter().next() {
            Some(0) => {
                self.extra.state.push(GTriGState::BaseIri);
                Ok("BASE".len())
            }
            Some(1) => {
                self.extra.state.push(GTriGState::PrefixDeclaration);
                Ok("PREFIX".len())
            }
            Some(2) => {
                self.extra.state.push(GTriGState::VersionSpecifier);
                Ok("VERSION".len())
            }
            Some(3) => {
                self.extra.state.push(GTriGState::NamedGraph(false));
                Ok("GRAPH".len())
            }
            Some(4) => self.enter_at_keyword(txt),
            Some(5) => {
                self.extra.state.push(GTriGState::DefaultGraph);
                Ok(1)
            }
            None => {
                self.extra.state.push(GTriGState::Block(false));
                match self.enter_term(txt)? {
                    (offset, None) => {
                        if self.extra.state.last().unwrap()
                            == &GTriGState::BlankNodePropertyListOrAnon
                        {
                            self.extra.state.pop();
                            self.extra.state[0] = GTriGState::Block(true);
                        }
                        Ok(offset)
                    }
                    (offset, Some(GTriGState::RdfLiteral(after_dt))) => {
                        debug_assert_eq!(offset, 0);
                        debug_assert!(!after_dt);
                        self.enter_rdf_literal(txt)
                    }
                    (offset, Some(state)) => {
                        match state {
                            GTriGState::ReifiedTriple(_) => {
                                self.extra.state[0] = GTriGState::Triples(false);
                            }
                            GTriGState::Collection => {
                                self.extra.state[0] = GTriGState::Triples(true);
                            }
                            _ => {}
                        }
                        self.extra.state.push(state);
                        Ok(offset)
                    }
                }
            }
            Some(_) => unreachable!(),
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
            Some(GTriGState::DefaultGraph)
        ));
        debug_assert!(self.extra.state.len() == 1);
        if txt.starts_with("}") {
            self.extra.state.pop();
            Ok(1)
        } else {
            self.enter_subject(txt)
        }
    }

    /// Expect any production corresponding to a subject.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        self.extra.state.push(GTriGState::Triples(true));
        match self.enter_term(txt)? {
            (offset, None) => Ok(offset),
            (offset, Some(GTriGState::RdfLiteral(after_dt))) => {
                debug_assert_eq!(offset, 0);
                debug_assert!(!after_dt);
                self.enter_rdf_literal(txt)
            }
            (offset, Some(state)) => {
                if matches!(
                    state,
                    GTriGState::ReifiedTriple(_) | GTriGState::BlankNodePropertyListOrAnon
                ) {
                    *self.extra.state.last_mut().unwrap() = GTriGState::Triples(false);
                }
                self.extra.state.push(state);
                Ok(offset)
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
            Some(&GTriGState::NamedGraph(has_name))
        );
        if !has_name {
            debug_assert!(self.extra.state.len() == 1);
            self.extra.state[0] = GTriGState::NamedGraph(true);
            self.extra.state.push(GTriGState::LabelNotSubject);
            self.enter_simple_term(txt, false)
        } else {
            debug_assert!(has_name);
            if txt.starts_with("}") {
                self.extra.state.pop();
                self.pop_triple_components(1);
                Ok(1)
            } else {
                self.enter_subject(txt)
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
        debug_assert_eq!(state.last(), Some(&GTriGState::Block(square_bracket)));
        debug_assert_eq!(state.len(), 1);
        #[allow(clippy::collapsible_else_if)]
        if square_bracket {
            if txt.starts_with(']') {
                state[0] = GTriGState::Block(false);
                Ok(1)
            } else {
                state[0] = GTriGState::Triples(false);
                state.push(GTriGState::BlankNodePropertyListOrAnon);
                self.enter_predicate_object_list(txt)
            }
        } else {
            debug_assert!(!square_bracket);
            if txt.starts_with('{') {
                state[0] = GTriGState::NamedGraph(true);
                Ok(1)
            } else {
                state[0] = GTriGState::Triples(true);
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
        debug_assert_eq!(self.extra.state.last(), Some(&GTriGState::LabelNotSubject));
        if txt.starts_with('{') {
            self.extra.state.pop();
            Ok(1)
        } else {
            Err(ErrorKind::Expected("'{'".into())).wrap_in(self)
        }
    }

    fn enter_term<E>(&mut self, txt: &str) -> StreamResult<(usize, Option<GTriGState>), Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);

        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    if txt[2..].starts_with('(') {
                        Ok((3, Some(GTriGState::TripleTerm)))
                    } else {
                        Ok((3, Some(GTriGState::ReifiedTriple(true))))
                    }
                } else {
                    self.iriref(txt).map(|offset| (offset, None))
                }
            }
            b'[' => {
                self.extra
                    .state
                    .push(GTriGState::BlankNodePropertyListOrAnon);
                self.mint_bnode_label();
                Ok((1, None))
            }
            b'_' => self.blank_node_label(txt).map(|offset| (offset, None)),
            b'(' => Ok((1, Some(GTriGState::Collection))),
            b'0'..=b'9' | b'-' | b'+' => self.numeric_literal(txt).map(|offset| (offset, None)),
            b'.' if txt.len() > 1 && txt.as_bytes()[1].is_ascii_digit() => {
                self.numeric_literal(txt).map(|offset| (offset, None))
            }
            b'"' | b'\'' => Ok((0, Some(GTriGState::RdfLiteral(false)))),
            b'?' => self.variable_name(txt).map(|offset| (offset, None)),
            _ => {
                lazy_regex!(BOOL = r"^(?:true|false)[ \n\r\t#~{,;.]");
                match BOOL.find(txt).iter().next() {
                    Some(cap) => self
                        .boolean_literal(cap.as_str())
                        .map(|offset| (offset, None)),
                    None => self.prefixed_name(txt).map(|offset| (offset, None)),
                }
            }
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
            matches!(self.extra.state.last(), Some(GTriGState::Verb)),
            "{:?}",
            self.extra.state
        );

        self.extra.state.pop();
        match self.extra.state.last() {
            Some(GTriGState::PredicateObjectList) => self.enter_object_list(txt, callback),
            Some(GTriGState::ReifiedTriple(true)) => self.enter_rt_object(txt),
            Some(GTriGState::TripleTerm) => self.enter_tt_object(txt),
            _ => unreachable!(),
        }
    }

    /// Compute the next context after production https://www.w3.org/TR/rdf12-trig/#grammar-production-object,
    /// after emitting the last triple in stack.
    ///
    /// See also [`GTriGSource::enter_object`]
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
            matches!(self.extra.state.last(), Some(GTriGState::Object)),
            "{:?}",
            self.extra.state
        );

        self.extra.state.pop();
        match self.extra.state.last() {
            Some(GTriGState::ObjectList) => {
                self.emit_tuple(callback, 0)?;
                self.in_object_list(txt, callback)
            }
            Some(GTriGState::Collection) => {
                self.emit_tuple(callback, 0)?;
                self.grow_collection()?;
                self.in_collection(txt, callback)
            }
            _ => unreachable!(),
        }
    }

    fn enter_simple_term<E>(&mut self, txt: &str, rt: bool) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);

        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    if txt[2..].starts_with('(') {
                        self.extra.state.push(GTriGState::TripleTerm);
                        Ok(3)
                    } else if rt {
                        self.extra.state.push(GTriGState::ReifiedTriple(true));
                        Ok(2)
                    } else {
                        Err(ErrorKind::Expected(
                            "ttObject (reified triple not allowed)".into(),
                        ))
                        .wrap_in(self)
                    }
                } else {
                    self.iriref(txt)
                }
            }
            b'[' => {
                self.extra.state.push(GTriGState::Anon);
                Ok(1)
            }
            b'_' => self.blank_node_label(txt),
            b'0'..=b'9' | b'-' | b'+' => self.numeric_literal(txt),
            b'.' if txt.len() > 1 && txt.as_bytes()[1].is_ascii_digit() => {
                self.numeric_literal(txt)
            }
            b'"' | b'\'' => self.enter_rdf_literal(txt),
            b'?' => self.variable_name(txt),
            _ => {
                lazy_regex!(BOOL = r"^(?:true|false)[ \n\r\t#~{,;.]");
                match BOOL.find(txt).iter().next() {
                    Some(cap) => self.boolean_literal(cap.as_str()),
                    None => self.prefixed_name(txt),
                }
            }
        }
    }

    fn variable_name<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(txt.starts_with('?'));

        lazy_regex!(
            VARNAME = r"(?x)
          ^
          [_A-Za-z0-9\u{C0}-\u{D6}\u{D8}-\u{F6}\u{F8}-\u{2FF}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\U{10000}-\U{EFFFF}]
          [_A-Za-z0-9\u{B7}\u{C0}-\u{D6}\u{D8}-\u{F6}\u{F8}-\u{2FF}\u{300}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{203F}-\u{2040}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\U{10000}-\U{EFFFF}]*
        "
        );

        let buffer_idx = self.inner.buffers.len();
        if let Some(cap) = VARNAME.find(&txt[1..]) {
            self.inner_mut().buffers.push().push_str(cap.as_str());
            self.inner_mut()
                .terms
                .push(IndexedTerm::Variable(buffer_idx));
            Ok(1 + cap.len())
        } else {
            Err(ErrorKind::Variable).wrap_in_at(self, 1)
        }
    }

    fn exit_reifier<E, C>(
        &mut self,
        callback: &mut C,
        offset: usize,
    ) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(
            matches!(self.extra.state.last(), Some(GTriGState::Reifier(false))),
            "{:?}",
            self.extra.state
        );
        self.extra_mut().state.pop(); // popping TurtleState::Reifier
        self.emit_reification_tuple(callback, offset)
    }
}

#[cfg(test)]
mod test;
