use std::fmt::Write;

use sophia_api::{
    prefix::{Prefix, PrefixMap},
    source::StreamResult,
    term::IriRef,
};
use sophia_iri::{Iri, resolve::BaseIriRef};

use crate::_term::IndexedTerm;
use crate::lazy_regex;
use crate::parser::{
    _common::{Extra, GenericSource, Inner, State, unescape_literal},
    _error::ResultExt,
    Error, ErrorKind,
};

/// Common trait of `TripleSource`/`QuadSource` for T-type formats (Turtle/TriG)
pub(crate) trait TxSource<S: State>: GenericSource {
    /// Get the extra (T-specific) attributes of the source
    fn extra(&self) -> &Extra<S>;

    /// Get mutably the extra (T-specific) attributes of the source
    fn extra_mut(&mut self) -> &mut Extra<S>;

    /// Get mutably both the inner and extra attributes of the source
    fn inner_and_extra_mut(&mut self) -> (&mut Inner, &mut Extra<S>);

    /// Feed `callback` with the triple/quad last parsed
    ///
    /// Precondition: `source` is in an appropriate state
    fn emit_reification_tuple<C, E, E2>(
        &self,
        callback: &mut C,
        offset: usize,
    ) -> StreamResult<usize, E, E2>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E2>,
        E: std::error::Error,
        E2: std::error::Error;

    /// Decide of which method to call based on the current state (`self.extra.state`).
    fn parse_token<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static;

    /// Expect the final '.' at the end of a `triples` production.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with '.'.
    fn end_triples<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static;

    // methods with default implementation

    fn parse_line<C, E>(&mut self, line: &str, mut callback: C) -> StreamResult<(), Error, E>
    where
        C: FnMut(<Self as GenericSource>::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert_eq!(self.inner().col, 0);
        match self.extra().state.last() {
            Some(s) if s.in_string_literal_long() => {}
            _ => self.inner_mut().col += self.ws(line),
        };
        while self.inner().col < line.len() {
            if cfg!(test) {
                println!(
                    "  “{}˄{}”\n    {:?}\n    {:?}\n    {:?}",
                    &line[..self.inner().col],
                    &line[self.inner().col..],
                    self.extra().state,
                    self.inner().terms,
                    self.inner().buffers,
                );
            }
            self.inner_mut().col += self.parse_token(&line[self.inner().col..], &mut callback)?;
            self.inner_mut().col += self.ws(&line[self.inner().col..]);
        }
        if cfg!(test) {
            println!(
                "  “{}˄{}”\n    {:?}\n    {:?}\n    {:?}",
                &line[..self.inner().col],
                &line[self.inner().col..],
                self.extra().state,
                self.inner().terms,
                self.inner().buffers
            );
        }
        Ok(())
    }

    // productions for non-terminal

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-prefixID
    /// or https://www.w3.org/TR/rdf12-turtle/#grammar-production-base
    /// assuming the leading '@'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_at_keyword<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(txt.starts_with('@'));
        debug_assert!(
            self.extra().state.last().is_none(),
            "{:?}",
            self.extra().state
        );

        let txt = &txt[1..];
        self.extra_mut().state.push(S::new_at_directive());

        lazy_regex!(
            KEYWORDS = [
                r"^base[ \n\r\t#<]",
                r"^prefix[ \n\r\t#:]",
                r#"^version[ \n\r\t#"']"#
            ]
        );
        match KEYWORDS.matches(txt).iter().next() {
            Some(0) => {
                self.extra_mut().state.push(S::new_base_iri());
                Ok("@base".len())
            }
            Some(1) => {
                self.extra_mut().state.push(S::new_prefix_declaration());
                Ok("@prefix".len())
            }
            Some(2) => {
                self.extra_mut().state.push(S::new_version_specifier());
                Ok("@version".len())
            }
            None => Err(ErrorKind::Expected("prefix, base or version".into())).wrap_in_at(self, 1),
            Some(_) => unreachable!(),
        }
    }

    /// Expect the final '.' at the end of an at-keyword directivei.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with '.'.
    fn end_at_directive<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty());
        debug_assert_eq!(self.extra().state.len(), 1);
        debug_assert!(
            self.extra().state.last().unwrap().in_at_directive(),
            "{:?}",
            self.extra().state
        );

        if !txt.starts_with('.') {
            return Err(ErrorKind::Expected("'.'".into())).wrap_in(self);
        }
        self.extra_mut().state.pop();
        Ok(1)
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-PNAME_NS
    /// in the context of a 'prefix' directive.
    fn in_prefix_declaration<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_prefix_declaration(),
            "{:?}",
            self.extra().state
        );

        let Some(end) = txt.find(':') else {
            Err(ErrorKind::Expected("trailing ':'".into())).wrap_in(self)?
        };

        Prefix::new(&txt[0..end]).wrap_in_at(self, 1)?;
        self.extra_mut().state.pop();
        self.extra_mut().state.push(S::new_prefix_iri());
        self.inner_mut().buffers.push().push_str(&txt[0..end]);
        Ok(end + 1)
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-IRIREF
    /// (or the equivalent in TriG)
    /// in the context of a 'prefix' directive.
    fn in_prefix_iri<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_prefix_iri(),
            "{:?}",
            self.extra().state
        );

        if !txt.starts_with('<') {
            return Err(ErrorKind::Expected("'<'".into())).wrap_in(self);
        }

        let ret = self.iriref(txt)?;
        debug_assert!(
            matches!(self.inner().terms.last(), Some(IndexedTerm::Iri(i)) if *i == self.inner().buffers.len()-1)
        );
        let ns_box = Box::from(self.inner().buffers.top());
        let ns = Iri::new_unchecked(ns_box);
        let (inner, extra) = self.inner_and_extra_mut();
        inner.terms.pop();
        inner.buffers.pop();
        let prefix_str = inner.buffers.top();
        if let Some((_, old_ns)) = extra
            .prefixes
            .iter_mut()
            .find(|(prefix, _)| prefix == prefix_str)
        {
            *old_ns = ns;
        } else {
            let prefix_box = Box::from(prefix_str);
            let prefix = Prefix::new_unchecked(prefix_box);

            extra.prefixes.push((prefix, ns));
        }
        inner.buffers.pop();
        extra.state.pop();
        Ok(ret)
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-IRIREF
    /// (or the equivalent in TriG)
    /// in the context of a 'base' directive.
    fn in_base_iri<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_base_iri(),
            "{:?}",
            self.extra().state
        );

        if !txt.starts_with('<') {
            return Err(ErrorKind::Expected("'<'".into())).wrap_in(self);
        }

        let ret = self.iriref(txt)?;
        debug_assert!(
            matches!(self.inner().terms.last(), Some(IndexedTerm::Iri(i)) if *i == self.inner().buffers.len()-1)
        );
        let base_str = Box::from(self.inner().buffers.top());
        let base = BaseIriRef::new(base_str).wrap_in_at(self, 1)?;
        self.extra_mut().base = Some(base);

        self.inner_mut().terms.pop();
        self.inner_mut().buffers.pop();
        self.extra_mut().state.pop();
        Ok(ret)
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-VersionSpecifier
    /// in the context of a 'prefix' directive.
    fn in_version_specifier<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(self.extra().state.last().unwrap().in_version_specifier());

        let first = txt.as_bytes()[0];
        let ret = match first {
            b'"' | b'\'' => self.in_string_literal_short(&txt[1..], first)? + 1,
            _ => Err(ErrorKind::Expected("version specifier".into())).wrap_in(self)?,
        };
        debug_assert_eq!(self.inner().buffers.len(), 1);
        self.inner_mut().version = self
            .inner()
            .buffers
            .last()
            .unwrap()
            .parse()
            .ok()
            .unwrap_or_default();
        self.inner_mut().buffers.pop();
        self.extra_mut().state.pop();
        Ok(ret)
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-predicateObjectList
    /// in the context of  https://www.w3.org/TR/rdf12-turtle/#grammar-production-triples
    /// where predicateObjectList may be optional (if require_pol is false).
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_triples<E>(&mut self, txt: &str, require_pol: bool) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_triples(),
            "{:?}",
            self.extra().state
        );

        self.enter_predicate_object_list(txt).or_else(|err| {
            if require_pol {
                Err(err)
            } else {
                // cleaning the states created by self.enter_predicate_object_list above
                debug_assert!(
                    self.extra().state.last().unwrap().in_verb(),
                    "{:?}",
                    self.extra().state
                );
                self.extra_mut().state.pop();
                debug_assert!(
                    self.extra()
                        .state
                        .last()
                        .unwrap()
                        .in_predicate_object_list(),
                    "{:?}",
                    self.extra().state
                );
                self.extra_mut().state.pop();
                self.end_triples(txt)
            }
        })
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-predicateObjectList
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_predicate_object_list<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra()
                .state
                .last()
                .unwrap()
                .in_predicate_object_list_context(),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.push(S::new_predicate_object_list());
        self.enter_verb(txt)
    }

    /// In production https://www.w3.org/TR/rdf12-turtle/#grammar-production-predicateObjectList ,
    /// just after a semincolon ';'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_predicate_object_list<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        self.in_predicate_object_list_common(txt)
    }

    /// Common (Turtle + TriG) implementation of [TxSource::in_predicate_object_list].
    fn in_predicate_object_list_common<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra()
                .state
                .last()
                .unwrap()
                .in_predicate_object_list(),
            "{:?}",
            self.extra().state
        );

        match txt.as_bytes()[0] {
            b';' => Ok(1),
            b'.' => {
                if !(2..=3).contains(&self.extra().state.len()) {
                    // 2 in Turtle, 3 in TriG
                    return Err(ErrorKind::Expected("verb, ';' or closing bracket".into()))
                        .wrap_in(self);
                }
                debug_assert!(
                    self.extra().state[self.extra().state.len() - 2].in_triples(),
                    "{:?}",
                    self.extra().state
                );
                self.pop_triple_components(1);
                debug_assert!(self.inner().terms.is_empty());
                for _ in 0..2 {
                    self.extra_mut().state.pop();
                }
                Ok(1)
            }
            b']' => {
                if self.extra().state.len() < 2 {
                    Err(ErrorKind::Expected("verb, ';' or '.'".into())).wrap_in(self)
                } else if !self.extra().state[self.extra().state.len() - 2]
                    .in_blank_node_property_list_or_anon()
                {
                    Err(ErrorKind::Expected("verb, ';' or '|}'".into())).wrap_in(self)
                } else {
                    for _ in 0..2 {
                        self.extra_mut().state.pop();
                    }
                    if let Some(true) = self.extra().state.last().map(|s| s.in_collection()) {
                        self.grow_collection()?;
                    }
                    Ok(1)
                }
            }
            b'|' => {
                if txt.len() <= 1 || txt.as_bytes()[1] != b'}' {
                    Err(ErrorKind::Expected("'}' after '|'".into())).wrap_in_at(self, 1)
                } else if self.extra().state.len() < 2 {
                    Err(ErrorKind::Expected("verb, ';' or '.'".into())).wrap_in(self)
                } else if !self.extra().state[self.extra().state.len() - 2].in_annotation_block() {
                    Err(ErrorKind::Expected("verb, ';' or ']'".into())).wrap_in(self)
                } else {
                    debug_assert!(
                        self.extra().state[self.extra().state.len() - 1].in_predicate_object_list(),
                        "{:?}",
                        self.extra().state
                    );
                    for _ in 0..2 {
                        self.extra_mut().state.pop();
                    }

                    // pop the reifier itself, the IndexedTerm::Reifier and the IndexedTerm::TripleTerm marker
                    // (we know that the reifier uses exactly one buffer (IRI or Bnode), and the others none)
                    debug_assert!(self.has_reifier());
                    let limit = self.inner().terms.len() - 3;
                    self.inner_mut().terms.truncate(limit);
                    self.inner_mut().buffers.pop();
                    Ok(2)
                }
            }
            _ => self.enter_verb(txt),
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-objectList
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_object_list<E, C>(
        &mut self,
        txt: &str,
        callback: &mut C,
    ) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra()
                .state
                .last()
                .unwrap()
                .in_predicate_object_list(),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.push(S::new_object_list());
        self.enter_object(txt, callback)
    }

    /// Expect anything valid after production https://www.w3.org/TR/rdf12-turtle/#grammar-production-object
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-objectList ;
    /// might be production https://www.w3.org/TR/rdf12-turtle/#grammar-production-annotation,
    /// punctuation or closing brackets.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_object_list<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        self.in_object_list_common(txt, callback)
    }

    /// Common (Turtle + TriG) implementation of [TxSource::in_object_list].
    fn in_object_list_common<E, C>(
        &mut self,
        txt: &str,
        callback: &mut C,
    ) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_object_list(),
            "{:?}",
            self.extra().state
        );

        match txt.as_bytes()[0] {
            b'~' => {
                self.extra_mut().state.push(S::new_reifier());
                if self.has_reifier() {
                    // pop previous reifier
                    self.pop_term();
                } else {
                    self.inner_mut().terms.push(IndexedTerm::TripleTerm);
                    self.inner_mut().terms.push(IndexedTerm::Reifier);
                };
                Ok(1)
            }
            b'{' => {
                if txt.len() <= 1 || txt.as_bytes()[1] != b'|' {
                    Err(ErrorKind::Expected("'|' after '{'".into())).wrap_in_at(self, 1)
                } else {
                    self.enter_annotation_block(txt, callback)
                }
            }
            b',' => {
                self.pop_triple_components(1);
                *self.extra_mut().state.last_mut().unwrap() = S::new_verb(); // pretend we just saw a verb
                Ok(1)
            }
            b';' => {
                self.pop_triple_components(2);
                self.extra_mut().state.pop();
                debug_assert!(
                    self.extra()
                        .state
                        .last()
                        .unwrap()
                        .in_predicate_object_list(),
                    "{:?}",
                    self.extra().state
                );
                Ok(1)
            }
            b'.' => {
                if !(3..=4).contains(&self.extra().state.len()) {
                    // 3 in Turtle, 4 in TriG
                    return Err(ErrorKind::Expected(
                        "reifier, annotation, punctuation".into(),
                    ))
                    .wrap_in(self);
                }
                debug_assert!(
                    self.extra().state[self.extra().state.len() - 2].in_predicate_object_list()
                );
                debug_assert!(self.extra().state[self.extra().state.len() - 3].in_triples());
                self.pop_triple_components(3);
                for _ in 0..3 {
                    self.extra_mut().state.pop();
                }
                Ok(1)
            }
            b']' => {
                if self.extra().state.len() < 3 {
                    Err(ErrorKind::Expected(
                        "reifier, annotation or punctuation".into(),
                    ))
                    .wrap_in(self)
                } else if !self.extra().state[self.extra().state.len() - 3]
                    .in_blank_node_property_list_or_anon()
                {
                    Err(ErrorKind::Expected(
                        "reifier, annotation, punctuation or '|}'".into(),
                    ))
                    .wrap_in(self)
                } else {
                    debug_assert!(
                        self.extra().state[self.extra().state.len() - 2].in_predicate_object_list()
                    );
                    self.pop_triple_components(2);
                    for _ in 0..3 {
                        self.extra_mut().state.pop();
                    }
                    if let Some(true) = self.extra().state.last().map(|s| s.in_collection()) {
                        self.grow_collection()?;
                    }
                    Ok(1)
                }
            }
            b'|' => {
                if txt.len() <= 1 || txt.as_bytes()[1] != b'}' {
                    Err(ErrorKind::Expected("'}' after '|'".into())).wrap_in_at(self, 1)
                } else if self.extra().state.len() < 3 {
                    Err(ErrorKind::Expected(
                        "reifier, annotation or punctuation".into(),
                    ))
                    .wrap_in(self)
                } else if !self.extra().state[self.extra().state.len() - 3].in_annotation_block() {
                    Err(ErrorKind::Expected(
                        "reifier, annotation, punctuation or ']'".into(),
                    ))
                    .wrap_in(self)
                } else {
                    debug_assert!(
                        self.extra().state[self.extra().state.len() - 2].in_predicate_object_list()
                    );
                    for _ in 0..3 {
                        self.extra_mut().state.pop();
                    }

                    // pop the last predicate-object pair
                    self.pop_triple_components(2);
                    // pop the reifier itself, the IndexedTerm::Reifier and the IndexedTerm::TripleTerm marker
                    // (we know that the reifier uses exactly one buffer (IRI or Bnode), and the others none)
                    debug_assert!(self.has_reifier());
                    let limit = self.inner().terms.len() - 3;
                    self.inner_mut().terms.truncate(limit);
                    self.inner_mut().buffers.pop();
                    Ok(2)
                }
            }
            _ => Err(ErrorKind::Expected(
                "reifier, annotation or punctuation".into(),
            ))
            .wrap_in(self),
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-verb
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_verb<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_verb_context(),
            "{:?}",
            self.extra().state
        );

        lazy_regex!(DELIMITER = r##"^([ \n\r\t\[\(<'"#]|$)"##);

        self.extra_mut().state.push(S::new_verb());
        match txt.as_bytes()[0] {
            b'a' if DELIMITER.is_match(&txt[1..]) => {
                self.inner_mut()
                    .terms
                    .push(IndexedTerm::RdfBuiltin(IriRef::new_unchecked("#type")));
                Ok(1)
            }
            b'<' => self.iriref(txt),
            _ => self.prefixed_name(txt),
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-objectList
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    ///
    /// There are three possible outcomes:
    /// * The object can be parsed entirely (it can not contain whitespace);
    ///   the triple is emitted, and the state stack is untouched
    ///   (e.g. IRI, blank node label...).
    /// * The object can not be parsed entirely (it may contain whitespace);
    ///   the triple is not emitted yet, and `TurtleState::Object` is pushed to the state stack
    ///   (plus another state depending on the type of object)
    ///   (e.g. collection, triple term, reified triple)
    /// * The object is a blank node property;
    ///   the triple is emitted straight away,
    ///   and `TurtleState::BlankNodePropertyListOrAnon` is pushed to the state stack
    ///   (no `TurtleState::Object` is required, as the triple was already emitted).
    fn enter_object<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_object_context(),
            "{:?}",
            self.extra().state
        );

        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    if txt[2..].starts_with('(') {
                        self.extra_mut().state.push(S::new_object());
                        self.extra_mut().state.push(S::new_triple_term());
                        Ok(3)
                    } else {
                        self.extra_mut().state.push(S::new_object());
                        self.extra_mut().state.push(S::new_reified_triple(true));
                        Ok(2)
                    }
                } else {
                    self.iriref(txt).and_then(|r| self.emit_tuple(callback, r))
                }
            }
            b'[' => {
                self.extra_mut()
                    .state
                    .push(S::new_blank_node_property_list_or_anon());
                self.mint_bnode_label();
                self.emit_tuple(callback, 1)
            }
            b'_' => self
                .blank_node_label(txt)
                .and_then(|r| self.emit_tuple(callback, r)),
            b'(' => {
                self.extra_mut().state.push(S::new_object());
                self.extra_mut().state.push(S::new_collection());
                Ok(1)
            }
            b'0'..=b'9' | b'-' | b'+' | b'.' => self
                .numeric_literal(txt)
                .and_then(|r| self.emit_tuple(callback, r)),
            b'"' | b'\'' => {
                self.extra_mut().state.push(S::new_object());
                self.enter_rdf_literal(txt)
            }
            _ => {
                lazy_regex!(BOOL = r"^(?:true|false)[ \n\r\t#~{,;.]");
                match BOOL.find(txt).iter().next() {
                    Some(cap) => self
                        .boolean_literal(cap.as_str())
                        .and_then(|r| self.emit_tuple(callback, r)),
                    None => self
                        .prefixed_name(txt)
                        .and_then(|r| self.emit_tuple(callback, r)),
                }
            }
        }
    }

    /// Decide what to do after an opening square bracket, based on next character:
    /// could be production https://www.w3.org/TR/rdf12-turtle/#grammar-production-blankNodePropertyList
    /// or https://www.w3.org/TR/rdf12-turtle/#grammar-production-ANON .
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_blank_node_property_list_or_anon<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra()
                .state
                .last()
                .unwrap()
                .in_blank_node_property_list_or_anon(),
            "{:?}",
            self.extra().state
        );

        if txt.starts_with(']') {
            // ANON case
            self.extra_mut().state.pop();
            if let Some(require_pol) = self
                .extra_mut()
                .state
                .last_mut()
                .and_then(|s| s.as_triples_require_pol_mut())
            {
                *require_pol = true;
            } else if let Some(true) = self.extra().state.last().map(|s| s.in_collection()) {
                self.grow_collection()?;
            }
            Ok(1)
        } else {
            self.enter_predicate_object_list(txt)
        }
    }

    /// Expect the closing bracket ']'
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-ANON
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_anon<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_anon(),
            "{:?}",
            self.extra().state
        );

        if txt.starts_with(']') {
            self.extra_mut().state.pop();
            self.mint_bnode_label();
            Ok(1)
        } else {
            Err(ErrorKind::Expected("']'".into())).wrap_in(self)
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-object
    /// or the closing bracket ')'
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-collection .
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_collection<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_collection(),
            "{:?}",
            self.extra().state
        );

        if txt.starts_with(')') {
            self.inner_mut()
                .terms
                .push(IndexedTerm::RdfBuiltin(IriRef::new_unchecked("#nil")));
            if self.inner().terms.len() > 2
                && self.inner().terms[self.inner().terms.len() - 2] == IndexedTerm::Rest
            {
                self.emit_tuple(callback, 0)?;
                self.pop_term();
                self.pop_term();
            }
            if self.inner().terms.len() > 2
                && self.inner().terms[self.inner().terms.len() - 2] == IndexedTerm::Rest
            {
                // because of the early clean-up of intermediate bnodes (see below),
                // we have at most one level to pop
                self.pop_term();
                self.pop_term();
                debug_assert!(
                    self.inner().terms.len() <= 2
                        || self.inner().terms[self.inner().terms.len() - 2] != IndexedTerm::Rest
                );
            }
            self.extra_mut().state.pop();
            Ok(1)
        } else {
            self.mint_bnode_label();
            if self.inner().terms.len() > 2
                && self.inner().terms[self.inner().terms.len() - 2] == IndexedTerm::Rest
            {
                self.emit_tuple(callback, 0)?;
                if self.inner().terms.len() > 4
                    && self.inner().terms[self.inner().terms.len() - 4] == IndexedTerm::Rest
                {
                    // early-clean up intermediate bnodes, as they will not be reused anyway
                    debug_assert_eq!(
                        self.inner().terms.last(),
                        Some(&IndexedTerm::BlankNode(self.inner().buffers.len() - 1))
                    );
                    debug_assert_eq!(
                        self.inner().terms[self.inner().terms.len() - 3],
                        IndexedTerm::BlankNode(self.inner().buffers.len() - 2)
                    );
                    self.inner_mut().buffers.swap_top2(); // swap bnode buffers
                    self.pop_term(); // pop intermediate bnode
                    self.pop_term(); // pop `IndexedTerm::Rest`
                }
            }
            self.inner_mut()
                .terms
                .push(IndexedTerm::RdfBuiltin(IriRef::new_unchecked("#first")));
            let old_state_len = self.extra().state.len();
            let ret = self.enter_object(txt, callback)?;
            if self.extra().state.len() == old_state_len {
                self.grow_collection()?;
            }
            Ok(ret)
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-RDFLiteral
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-object
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with either '"' or '\''.
    fn enter_rdf_literal<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && matches!(txt.as_bytes()[0], b'"' | b'\''));
        debug_assert!(
            self.extra().state.last().unwrap().in_rdf_literal_context(),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.push(S::new_rdf_literal(false));
        let txtb = txt.as_bytes();
        let quote = txtb[0];
        if txt.len() >= 3 && txtb[1] == quote && txt.as_bytes()[2] == quote {
            self.extra_mut()
                .state
                .push(S::new_string_literal_long(quote));
            self.inner_mut().buffers.push();
            self.in_string_literal_long(&txt[3..], quote).map(|n| n + 3)
        } else {
            self.in_string_literal_short(&txt[1..], quote)
                .map(|n| n + 1)
        }
    }

    /// Expect whatever can follow the marker `~`
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-reifier .
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_reifier<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_reifier(),
            "{:?}",
            self.extra().state
        );

        let ret = match txt.as_bytes()[0] {
            b'<' => self.iriref(txt)?,
            b'_' => self.blank_node_label(txt)?,
            b'[' => {
                self.extra_mut().state.push(S::new_anon(true));
                return Ok(1); // do not pop state Reifier or emit reification tuple yet
            }
            b'~' | b'{' | b',' | b';' | b'.' | b'>' | b'|' | b']' => {
                self.mint_bnode_label();
                0
            }
            _ => self.prefixed_name(txt)?,
        };
        self.extra_mut().state.pop();
        self.emit_reification_tuple(callback, ret)
    }

    /// Expect whatever can follow production `String` (if `datatype` is false)
    /// or the marker `^^` (if `datatype` is true)
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-RDFLiteral .
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_rdf_literal<E>(&mut self, txt: &str, datatype: bool) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_rdf_literal(),
            "{:?}",
            self.extra().state
        );

        let txtb = txt.as_bytes();
        if !datatype {
            match txtb[0] {
                b'@' => self.lang_dir(txt),
                b'^' => {
                    if txtb.len() <= 1 || txtb[1] != b'^' {
                        Err(ErrorKind::Expected("'^'".into())).wrap_in_at(self, 1)
                    } else {
                        *self.extra_mut().state.last_mut().unwrap() = S::new_rdf_literal(true);
                        Ok(2)
                    }
                }
                _ => {
                    let idx = self.inner().buffers.len() - 1;
                    let dt = IriRef::new_unchecked("#string");
                    self.inner_mut()
                        .terms
                        .push(IndexedTerm::XsdLiteral(idx, dt));
                    self.extra_mut().state.pop();
                    Ok(0)
                }
            }
        } else {
            let ret = match txtb[0] {
                b'<' => self.iriref(txt),
                _ => self.prefixed_name(txt),
            }?;
            let Some(IndexedTerm::Iri(idx)) = self.inner_mut().terms.pop() else {
                unreachable!(); // both self.iriref and self.prefixed_name push a IndexedTerm::Iri
            };
            self.check_invalid_literal(idx)?;
            self.inner_mut()
                .terms
                .push(IndexedTerm::TypedLiteral(idx - 1, idx));
            self.extra_mut().state.pop();
            Ok(ret)
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-reifiedTriple
    /// and enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-rtSubject
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_rt_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_reified_triple(true),
            "{:?}",
            self.extra().state
        );
        self.extra_mut().state.push(S::new_rt_subject());

        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    self.extra_mut().state.push(S::new_reified_triple(true));
                    Ok(2)
                } else {
                    self.iriref(txt)
                }
            }
            b'[' => {
                self.extra_mut().state.push(S::new_anon(false));
                Ok(1)
            }
            b'_' => self.blank_node_label(txt),
            _ => self.prefixed_name(txt),
        }
    }

    /// Exit production https://www.w3.org/TR/rdf12-turtle/#grammar-production-rtSubject
    /// and enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-verb
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn exit_rt_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(self.extra().state.last().unwrap().in_rt_subject());

        self.extra_mut().state.pop();
        self.enter_verb(txt)
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-rtObject
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_rt_object<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_reified_triple(true),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.push(S::new_rt_object());
        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    if txt[2..].starts_with('(') {
                        self.extra_mut().state.push(S::new_triple_term());
                        Ok(3)
                    } else {
                        self.extra_mut().state.push(S::new_reified_triple(true));
                        Ok(2)
                    }
                } else {
                    self.iriref(txt)
                }
            }
            b'[' => {
                self.extra_mut().state.push(S::new_anon(false));
                Ok(1)
            }
            b'_' => self.blank_node_label(txt),
            b'0'..=b'9' | b'-' | b'+' | b'.' => self.numeric_literal(txt),
            b'"' | b'\'' => self.enter_rdf_literal(txt),
            _ => {
                lazy_regex!(BOOL = r"^(?:true|false)[ \n\r\t#~{,;.]");
                match BOOL.find(txt).iter().next() {
                    Some(cap) => self.boolean_literal(cap.as_str()),
                    None => self.prefixed_name(txt),
                }
            }
        }
    }

    /// Exit production https://www.w3.org/TR/rdf12-turtle/#grammar-production-rtObject
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-reifiedTriple
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn exit_rt_object<E, C>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_rt_object(),
            "{:?}",
            self.extra().state
        );
        debug_assert!(
            self.extra().state[self.extra().state.len() - 2].in_reified_triple(true),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.pop();
        *self.extra_mut().state.last_mut().unwrap() = S::new_reified_triple(false);
        self.inner_mut().terms.push(IndexedTerm::TripleTerm);
        self.inner_mut().terms.push(IndexedTerm::Reifier);

        if txt.starts_with('~') {
            self.extra_mut().state.push(S::new_reifier());
            Ok(1)
        } else {
            self.mint_bnode_label();
            self.emit_reification_tuple(callback, 0)
        }
    }

    /// Exit production https://www.w3.org/TR/rdf12-turtle/#grammar-production-reifiedTriple
    /// and consume closing bracket.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
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

            // First, we save information about the reifier, and pop it.
            // (we know that it uses exactly one buffer, being an IRI or a bnode)
            debug_assert!(matches!(
                self.inner().terms.last(),
                Some(IndexedTerm::Iri(_) | IndexedTerm::BlankNode(_))
            ));
            let old_idx = self.inner().buffers.len() - 1;
            let reif = self.inner_mut().terms.pop().unwrap();
            self.inner_mut().buffers.pop();
            debug_assert_eq!(self.inner().buffers.len(), old_idx);
            // Then we pop the Reifier and TripleTerm from the term stack
            debug_assert!(matches!(
                self.inner().terms[self.inner().terms.len() - 1],
                IndexedTerm::Reifier
            ));
            debug_assert!(matches!(
                self.inner().terms[self.inner().terms.len() - 2],
                IndexedTerm::TripleTerm
            ));
            let limit = self.inner().terms.len() - 2;
            self.inner_mut().terms.truncate(limit); // pop Reifier and TripleTerm
            // We now pop the 3 components of the reified triple
            for _ in 0..3 {
                self.pop_term();
            }
            // Finally, we restore the reifier at the top of the term and buffer stack
            let new_idx = self.inner_mut().buffers.recycle(old_idx);
            self.inner_mut().terms.push(match reif {
                IndexedTerm::Iri(_) => IndexedTerm::Iri(new_idx),
                IndexedTerm::BlankNode(_) => IndexedTerm::BlankNode(new_idx),
                _ => unreachable!(),
            });
            Ok(2)
        } else {
            Err(ErrorKind::Expected("closing bracket '>>'".into())).wrap_in(self)
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-tripleTerm
    /// and enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-ttSubject
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
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
        self.extra_mut().state.push(S::new_tt_subject());

        match txt.as_bytes()[0] {
            b'<' => self.iriref(txt),
            b'[' => {
                self.extra_mut().state.push(S::new_anon(false));
                Ok(1)
            }
            b'_' => self.blank_node_label(txt),
            _ => self.prefixed_name(txt),
        }
    }

    /// Exit production https://www.w3.org/TR/rdf12-turtle/#grammar-production-ttSubject
    /// and enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-verb
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn exit_tt_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_tt_subject(),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.pop();
        self.enter_verb(txt)
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-ttObject
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
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

        self.extra_mut().state.push(S::new_tt_object());
        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with('<') {
                    if txt[2..].starts_with('(') {
                        self.extra_mut().state.push(S::new_triple_term());
                        Ok(3)
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
                self.extra_mut().state.push(S::new_anon(false));
                Ok(1)
            }
            b'_' => self.blank_node_label(txt),
            b'0'..=b'9' | b'-' | b'+' | b'.' => self.numeric_literal(txt),
            b'"' | b'\'' => self.enter_rdf_literal(txt),
            _ => {
                lazy_regex!(BOOL = r"^(?:true|false)[ \n\r\t#~{,;.]");
                match BOOL.find(txt).iter().next() {
                    Some(cap) => self.boolean_literal(cap.as_str()),
                    None => self.prefixed_name(txt),
                }
            }
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-ttObject
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-tripleTerm
    /// and exit both, consuming the closing bracket.
    fn exit_tt_object<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_tt_object(),
            "{:?}",
            self.extra().state
        );

        self.extra_mut().state.pop(); // ttObject
        if txt.starts_with(")>>") {
            self.extra_mut().state.pop(); // tripleTerm
            self.inner_mut().terms.push(IndexedTerm::TripleTerm);
            Ok(3)
        } else {
            Err(ErrorKind::Expected("')>>'".into())).wrap_in(self)
        }
    }

    /// Enter production https://www.w3.org/TR/rdf12-turtle/#grammar-production-annotationBlock
    /// assuming the leading '{|'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn enter_annotation_block<E, C>(
        &mut self,
        txt: &str,
        callback: &mut C,
    ) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra().state.last().unwrap().in_object_list(),
            "{:?}",
            self.extra().state
        );
        debug_assert!(txt.starts_with("{|"));

        const RET: usize = 2; // len of the open bracket '{|'
        self.extra_mut().state.push(S::new_annotation_block());
        if !self.has_reifier() {
            self.inner_mut().terms.push(IndexedTerm::TripleTerm);
            self.inner_mut().terms.push(IndexedTerm::Reifier);
            self.mint_bnode_label();
            self.emit_reification_tuple(callback, RET)
        } else {
            Ok(RET)
        }
    }

    // productions for terminals (or for alternative between terminals, like `prefixed_name` or `numeric_literal`)

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-IRIREF
    /// (or the equivalent in TriG)
    /// assuming the leading '<'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn iriref<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let buffer_idx = self.inner().buffers.len();
        let ret = self.iriref_raw(txt)?;
        let (inner, extra) = self.inner_and_extra_mut();
        match extra.base.as_ref() {
            None => {
                if let Err(err) = Iri::new(inner.buffers.top()) {
                    // .wrap_in_at can not be directly applied to the result of Iri::new
                    // because .self is mutably borrowed via buf
                    return Err(err).wrap_in_at(self, 1);
                }
            }
            Some(base) => {
                inner.buffers.push(); // prepare buffer for storing the absolute (resolved) IRI
                let [rel, abs] = inner.buffers.top2_mut();
                if let Err(err) = base.resolve_into(rel.as_str(), abs) {
                    // .wrap_in_at can not be directly applied to the result of .resolve_into,
                    // because .self is mutably borrowed via buf2
                    return Err(err).wrap_in_at(self, 1);
                }
                inner.buffers.swap_top2(); // swap 'rel' and 'abs' buffers
                inner.buffers.pop(); // drop 'rel' buffer
            }
        }
        inner.terms.push(IndexedTerm::Iri(buffer_idx));
        Ok(ret)
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-PrefixedName
    /// (or the equivalent in TriG)
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn prefixed_name<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);

        lazy_regex!(
            PREFIX = r"(?x)^(
            # PN_CHAR_BASE
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}]
            (
                # [ PN_CHARS | '.' ]*
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}.-]*
                # PN_CHARS
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}-]
            )?
        )?"
        );
        let Some(prefix) = PREFIX.find(txt) else {
            unreachable!()
        }; // the empty string is a match
        let colon = prefix.len();
        if colon >= txt.len() || txt.as_bytes()[colon] != b':' {
            if prefix.is_empty() {
                Err(ErrorKind::Expected("prefixed name".into())).wrap_in(self)?
            } else {
                Err(ErrorKind::Expected("trailing ':'".into())).wrap_in_at(self, prefix.len())?
            }
        }
        let prefix = prefix.as_str();
        let (inner, extra) = self.inner_and_extra_mut();
        let Some(ns) = extra.prefixes.get_namespace(prefix) else {
            Err(ErrorKind::UnknownPrefix(prefix.into())).wrap_in(self)?
        };
        let buffer_idx = inner.buffers.len();
        inner.buffers.push().push_str(&ns);

        lazy_regex!(
            SUFFIX = r"(?x) ^
            # (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
            (?:
                [
                 A-Z a-z \xC0-\xD6 \xD8-\xF6 \xF8-\u{02FF} \u{0370}-\u{037D}
                 \u{037F}-\u{1FFF} \u{200C}-\u{200D} \u{2070}-\u{218F} \u{2C00}-\u{2FEF}
                 \u{3001}-\u{D7FF} \u{F900}-\u{FDCF} \u{FDF0}-\u{FFFD} \u{10000}-\u{EFFFF}
                 _
                 : 0-9
                ] | %[0-9A-Fa-f]{2} | \\[_~.\-!$&'()*+,;=/?\#@%]
            )
            (?:
             (?:
                [
                 A-Z a-z \xC0-\xD6 \xD8-\xF6 \xF8-\u{02FF} \u{0370}-\u{037D}
                 \u{037F}-\u{1FFF} \u{200C}-\u{200D} \u{2070}-\u{218F} \u{2C00}-\u{2FEF}
                 \u{3001}-\u{D7FF} \u{F900}-\u{FDCF} \u{FDF0}-\u{FFFD} \u{10000}-\u{EFFFF}
                 _
                 \- 0-9 \xB7 \u{0300}-\u{036F} \u{203F}-\u{2040}
                 . :
                ] | %[0-9A-Fa-f]{2} | \\[_~.\-!$&'()*+,;=/?\#@%]
             )*
             (?:
                [
                 A-Z a-z \xC0-\xD6 \xD8-\xF6 \xF8-\u{02FF} \u{0370}-\u{037D}
                 \u{037F}-\u{1FFF} \u{200C}-\u{200D} \u{2070}-\u{218F} \u{2C00}-\u{2FEF}
                 \u{3001}-\u{D7FF} \u{F900}-\u{FDCF} \u{FDF0}-\u{FFFD} \u{10000}-\u{EFFFF}
                 _
                 \- 0-9 \xB7 \u{0300}-\u{036F} \u{203F}-\u{2040}
                 :
                ] | %[0-9A-Fa-f]{2} | \\[_~.\-!$&'()*+,;=/?\#@%]
             )
            )?
        "
        );
        let offset = if let Some(suffix) = SUFFIX.find(&txt[colon + 1..]) {
            // copy suffix into top buffer, unescaping antislash-escapes
            let buf = inner.buffers.top_mut();
            let mut rem = suffix.as_str();
            while let Some(i) = rem.find('\\') {
                buf.push_str(&rem[..i]);
                rem = &rem[i + 1..];
            }
            buf.push_str(rem);
            debug_assert!(Iri::new(inner.buffers.top()).is_ok());
            suffix.len()
        } else {
            0
        };
        inner.terms.push(IndexedTerm::Iri(buffer_idx));
        Ok(colon + 1 + offset)
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-BLANK_NODE_LABEL
    /// (or the equivalent TriG)
    /// assuming the leading '_'.
    ///
    /// Also ensures that labels that would clash with auto-generated labels are correctly disambiguated.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn blank_node_label<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        let ret = self.blank_node_label_raw(txt)?;
        if GENID_RE.find(&txt[2..ret]).is_some() {
            self.dedup_bnode_label();
        };
        Ok(ret)
    }

    /// Expect the continuation of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-STRING_LITERAL_LONG_SINGLE_QUOTE
    /// or https://www.w3.org/TR/rdf12-turtle/#grammar-production-STRING_LITERAL_LONG_QUOTE .
    /// (or the equivalent in N-Triples, N-Quads or TriG)
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col.
    fn in_string_literal_long<E>(&mut self, txt: &str, quote: u8) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty());
        debug_assert!(
            self.extra().state.last().unwrap().in_string_literal_long(),
            "{:?}",
            self.extra().state
        );

        let buf = self.inner_mut().buffers.top_mut();
        let txtb = txt.as_bytes();
        let mut i = 0;
        loop {
            match txt[i..].find(['\\', quote as char]).map(|x| x + i) {
                Some(j) if txtb[j] == b'\\' => {
                    buf.push_str(&txt[i..j]);
                    if let Err(err) = unescape_literal(&txt[j + 1..], &mut i, buf, j + 1) {
                        return Err(err).wrap_in_at(self, j);
                    }
                }
                Some(j) if txtb[j] == quote => {
                    if txtb.len() >= j + 3 {
                        if txtb[j + 1] == quote && txtb[j + 2] == quote {
                            buf.push_str(&txt[i..j]);
                            self.extra_mut().state.pop();
                            return Ok(j + 3);
                        } else {
                            // NB: in case we encounter [quote, something_else, quote]
                            // it is still possible that the final quote is the start of a sequence of 3,
                            // so we do not consume the 3rd character.
                            buf.push_str(&txt[i..j + 2]);
                            i = j + 2;
                        }
                    } else {
                        // less than 3 characters remaining, so we can consume them
                        buf.push_str(&txt[i..]);
                        return Ok(txt.len());
                    }
                }
                opt => {
                    debug_assert!(opt.is_none());
                    buf.push_str(&txt[i..]);
                    return Ok(txt.len());
                }
            }
        }
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-LANG_DIR
    /// assuming the leading '@'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn lang_dir<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(
            self.extra().state.last().unwrap().in_lang_dir(),
            "{:?}",
            self.extra().state
        );
        let ret = self.lang_dir_raw(txt)?;
        self.extra_mut().state.pop();
        Ok(ret)
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-BooleanLiteral
    /// (or the equivalent in TriG)
    /// assuming either "true" or "false".
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn boolean_literal<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(txt.starts_with("true") || txt.starts_with("false"));
        debug_assert!(
            self.extra()
                .state
                .last()
                .unwrap()
                .in_boolean_literal_context(),
            "{:?}",
            self.extra().state
        );

        self.inner_mut()
            .terms
            .push(IndexedTerm::BoolLiteral(txt.starts_with('t')));
        Ok(txt.len() - 1) // minus 1 because the text includes the first separator after the bool value
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-NumericLiteral
    /// (or the equivalent in TriG)
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn numeric_literal<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);
        debug_assert!(
            self.extra()
                .state
                .last()
                .unwrap()
                .in_numeric_literal_context(),
            "{:?}",
            self.extra().state
        );

        // NB: it is important that this regex tests xsd:double first,
        // then xsd:decimal, then xsd:integer.
        // The reason is that the regex is matched greedily,
        // and the the legs for xsd:integers and xsd:decimal
        // also match the the start of xsd:double.
        lazy_regex!(
            NUMERIC = r"(?x) ^
            [+-]?
            (?:
              (?: [0-9]+ (?: \. [0-9]*)? | \. [0-9]+ ) [eE] [+-]? [0-9]+
            |
              [0-9]* \. [0-9]+
            |
              [0-9]+
            )
        "
        );

        match NUMERIC.find(txt) {
            None => Err(ErrorKind::Expected("numeric literal".into())).wrap_in(self),
            Some(cap) => {
                let lex = cap.as_str();
                let buffer_idx = self.inner().buffers.len();
                self.inner_mut().buffers.push().push_str(lex);
                let dt = if lex.contains(['e', 'E']) {
                    "#double"
                } else if lex.contains(['.']) {
                    "#decimal"
                } else {
                    "#integer"
                };
                self.inner_mut().terms.push(IndexedTerm::XsdLiteral(
                    buffer_idx,
                    IriRef::new_unchecked(dt),
                ));
                Ok(cap.len())
            }
        }
    }

    /// Expect the closing bracket ']'
    /// in the context of production https://www.w3.org/TR/rdf12-turtle/#grammar-production-ANON
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col, and starting with a non-white-space character.
    fn in_anon_reifier<E, C>(
        &mut self,
        txt: &str,
        callback: &mut C,
    ) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
    {
        self.in_anon(txt).and_then(|offset| {
            self.extra_mut().state.pop(); // popping TurtleState::Reifier
            self.emit_reification_tuple(callback, offset)
        })
    }

    /// Consume whitespace or comments.
    ///
    /// `txt` is (the remaining of) a line of text as read by [`BufRead::read_line`],
    /// starting at self.col.
    fn ws(&mut self, txt: &str) -> usize {
        lazy_regex!(WS = r"^[ \n\r\t]*");
        let ret = match WS.find(txt) {
            None => 0,
            Some(m) => m.len(),
        };
        if txt[ret..].starts_with('#') {
            txt.len()
        } else {
            ret
        }
    }

    // other methods

    fn mint_bnode_label(&mut self) {
        let (inner, extra) = self.inner_and_extra_mut();
        extra.bnode_id += 1;
        let buffer_idx = inner.buffers.len();
        inner
            .buffers
            .push()
            .write_fmt(format_args!("bn{:04}", extra.bnode_id))
            .unwrap();
        debug_assert!(GENID_RE.find(inner.buffers.top()).is_some());
        inner.terms.push(IndexedTerm::BlankNode(buffer_idx));
    }

    fn dedup_bnode_label(&mut self) {
        debug_assert!(!self.inner().buffers.is_empty());
        self.inner_mut().buffers.top_mut().push('d');
        debug_assert!(GENID_RE.find(self.inner().buffers.top()).is_some());
    }

    fn grow_collection<E>(&mut self) -> StreamResult<(), Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        self.pop_term(); // pop collection element
        debug_assert_eq!(
            self.inner().terms.last(),
            Some(&IndexedTerm::RdfBuiltin(IriRef::new_unchecked("#first")))
        );
        *self.inner_mut().terms.last_mut().unwrap() = IndexedTerm::Rest;
        Ok(())
    }

    fn pop_triple_components(&mut self, n: u8) {
        if self.has_reifier() {
            // pop the reifier itself, the IndexedTerm::Reifier and the IndexedTerm::TripleTerm marker
            // (we know that the reifier uses exactly one buffer (IRI or Bnode), and the others none)
            let limit = self.inner().terms.len() - 3;
            self.inner_mut().terms.truncate(limit);
            self.inner_mut().buffers.pop();
        }
        for _ in 0..n {
            self.pop_term();
        }
    }

    fn has_reifier(&self) -> bool {
        self.inner().terms.len() >= 2
            && self.inner().terms[self.inner().terms.len() - 2] == IndexedTerm::Reifier
    }
}

// Regex for generated bnode generated IDs
lazy_regex!(GENID_RE = "^bn[0-9]{4,}d*$");
