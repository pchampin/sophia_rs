//! Generalized [N-Quads] parser
//!
//! [N-Quads]: https://www.w3.org/TR/rdf12-n-quads

use std::io::BufRead;

use sophia_api::{
    parser::QuadParser,
    quad::Spog,
    source::{Source, StreamError::SinkError, StreamResult},
    term::{IriRef, VarName},
    version::Version,
};

use crate::{
    _term::{IndexedTerm, StashedTerm},
    parser::{
        _common::{GenericSource, Inner, NxSource},
        _error::{AdjustCol, Error, ErrorKind, ResultExt},
    },
};

sophia_api::def_mod_functions_for_bufread_parser!(GNQuadsParser, QuadParser);

/// [N-Quads] parser.
///
/// [N-Quads]: https://www.w3.org/TR/rdf12-n-quads/
#[derive(Clone, Debug, Default)]
pub struct GNQuadsParser {
    /// The default version specifier.
    ///
    /// Will be applied until overridden with a `@version`/`VERSION` directive.
    pub version: Version,
}

impl GNQuadsParser {
    /// Construct a [`GNQuadsParser`] with default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Change the [`version`](GNQuadsParser::version) option of this parser.
    #[must_use]
    pub fn with_version(self, version: Version) -> Self {
        Self { version }
    }
}

impl<I: BufRead> QuadParser<I> for GNQuadsParser {
    type Source = GNQSource<I>;

    fn parse(&self, data: I) -> Self::Source {
        GNQSource {
            input: data,
            inner: Inner {
                version: self.version,
                ..Default::default()
            },
            is_quad: false,
        }
    }
}

/// [`TripleSource`](sophia_api::prelude::TripleSource) returned by a [`GNQuadsParser`]
#[derive(Clone, Debug, Default)]
pub struct GNQSource<I> {
    pub(crate) input: I,
    pub(crate) inner: Inner,
    is_quad: bool,
}

impl<I> GenericSource for GNQSource<I> {
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
        if self.is_quad {
            callback(StashedTerm::new_generalized_spog(
                &self.inner.terms,
                &self.inner.buffers,
            ))
        } else {
            callback((
                StashedTerm::new_generalized_triple(&self.inner.terms, &self.inner.buffers),
                None,
            ))
        }
        .map(|_| offset)
        .map_err(SinkError)
    }
}

impl<I> NxSource for GNQSource<I> {
    fn parse_tokens<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);

        let mut col = 0;
        if let Some(tail) = txt.strip_prefix("V") {
            // except version directive
            if !tail.starts_with("ERSION") {
                return Err(ErrorKind::Expected("'VERSION' or subject".into())).wrap_in(self);
            }
            col = "VERSION".len();
            col += self.ws(&txt[col..]);
            if txt[col..].starts_with('"') {
                col += 1 + self
                    .in_string_literal_short(&txt[col + 1..], b'"')
                    .adjust_col(col)?;
                debug_assert_eq!(self.inner.buffers.len(), 1);
                self.inner.version = self
                    .inner
                    .buffers
                    .last()
                    .unwrap()
                    .parse()
                    .ok()
                    .unwrap_or_default();
                self.inner.buffers.pop();
            } else {
                return Err(ErrorKind::Expected("versionSpecifier".into())).wrap_in_at(self, col);
            }
        } else {
            // expect quad
            col += self.enter_term(&txt[col..]).adjust_col(col)?;
            col += self.ws(&txt[col..]);
            col += self.enter_term(&txt[col..]).adjust_col(col)?;
            col += self.ws(&txt[col..]);
            col += self.enter_term(&txt[col..]).adjust_col(col)?;
            col += self.ws(&txt[col..]);
            if txt[col..].is_empty() {
                return Err(ErrorKind::Expected(".".into())).wrap_in_at(self, col);
            } else if txt[col..].starts_with('.') {
                self.is_quad = false;
                col += 1;
            } else {
                self.is_quad = true;
                col += self.enter_term(&txt[col..]).adjust_col(col)?;
                col += self.ws(&txt[col..]);
                if txt[col..].starts_with('.') {
                    col += 1;
                } else {
                    return Err(ErrorKind::Expected(".".into())).wrap_in_at(self, col);
                }
            }
            self.emit_tuple(callback, col)?;
            self.inner.terms.truncate(0);
            self.inner.buffers.empty();
        }
        Ok(col)
    }

    /// Override NxSource::enter_triple_term to support generalized triples
    fn enter_triple_term<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_triple_term", txt)
        }

        let mut col = 3;
        col += self.ws(&txt[col..]);
        col += self.enter_term(&txt[col..]).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        col += self.enter_term(&txt[col..]).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        col += self.enter_term(&txt[col..]).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        if txt[col..].starts_with(")>>") {
            self.inner.terms.push(IndexedTerm::TripleTerm);
            Ok(col + 3)
        } else {
            Err(ErrorKind::Expected(")>>".into())).wrap_in_at(self, col)
        }
    }

    /// Override NxSource::iriref to support relative IRIs.
    fn iriref<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        let buffer_idx = self.inner.buffers.len();
        let ret = self.iriref_raw(txt)?;
        if let Err(err) = IriRef::new(self.inner.buffers.top()) {
            // .wrap_in_at can not be directly applied to the result of Iri::new
            // because .self is mutably borrowed via buf
            return Err(err).wrap_in_at(self, 1);
        }
        self.inner.terms.push(IndexedTerm::Iri(buffer_idx));
        Ok(ret)
    }
}

impl<I: BufRead> Source for GNQSource<I> {
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
            Ok(false)
        } else {
            self.parse_line(&line, f)?;
            self.inner.line += 1;
            Ok(true)
        }
    }
}

impl<I> GNQSource<I> {
    fn enter_term<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_term", txt)
        }

        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with("<(") {
                    self.enter_triple_term(txt)
                } else {
                    self.iriref(txt)
                }
            }
            b'_' => self.blank_node_label(txt),
            b'"' => {
                let mut col = 1;
                col += self.in_string_literal_short(&txt[1..], b'"')?;
                col += self.ws(&txt[col..]);
                match txt.as_bytes()[col] {
                    b'^' if txt[col + 1..].starts_with('^') => {
                        col += 2 + self.ws(&txt[col + 2..]);
                        if txt[col..].starts_with('<') {
                            col += self.iriref(&txt[col..]).adjust_col(col)?;
                            let Some(IndexedTerm::Iri(idx)) = self.inner.terms.pop() else {
                                unreachable!(); // both self.iriref and self.prefixed_name push a IndexedTerm::Iri
                            };
                            self.check_invalid_literal(idx).adjust_col(col)?;
                            self.inner
                                .terms
                                .push(IndexedTerm::TypedLiteral(idx - 1, idx));
                            Ok(col)
                        } else {
                            Err(ErrorKind::Expected("IRIREF".into())).wrap_in_at(self, col)
                        }
                    }
                    b'@' => self
                        .lang_dir(&txt[col..])
                        .map(|offset| col + offset)
                        .adjust_col(col),
                    _ => {
                        let idx = self.inner.buffers.len() - 1;
                        let dt = IriRef::new_unchecked("#string");
                        self.inner.terms.push(IndexedTerm::XsdLiteral(idx, dt));
                        Ok(col)
                    }
                }
            }
            b'?' => {
                let end = txt
                    .as_bytes()
                    .iter()
                    .position(|b| b" \t).#".contains(b))
                    .unwrap_or(txt.len());
                if VarName::new(&txt[1..end]).is_ok() {
                    let buffer_idx = self.inner.buffers.len();
                    self.inner.buffers.push().push_str(&txt[1..end]);
                    self.inner.terms.push(IndexedTerm::Variable(buffer_idx));
                    Ok(end)
                } else {
                    Err(ErrorKind::Variable).wrap_in_at(self, 1)
                }
            }
            _ => Err(ErrorKind::Expected("term".into())).wrap_in(self),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        serializer::nq::{NQuadsConfig, NQuadsSerializer},
        test::{LazyMap, gnq_samples},
    };

    use super::*;
    use sophia_api::{
        prelude::{Dataset, QuadSerializer, Stringifier},
        source::QuadSource,
        term::SimpleTerm,
    };
    use sophia_isomorphism::isomorphic_datasets;
    use test_case::test_case;

    static TESTS: LazyMap = gnq_samples();

    #[test_case("empty")]
    #[test_case("comment")]
    #[test_case("version")]
    #[test_case("triple i i i")]
    #[test_case("triple b i i")]
    #[test_case("triple i i b")]
    #[test_case("triple b i b")]
    #[test_case("triple i i l")]
    #[test_case("triple b i l")]
    #[test_case("triple i i ld")]
    #[test_case("triple b i ld")]
    #[test_case("triple i i ll")]
    #[test_case("triple b i ll")]
    #[test_case("triple i i lb")]
    #[test_case("triple b i lb")]
    #[test_case("triple i i t")]
    #[test_case("triple b i t")]
    #[test_case("escape")]
    #[test_case("escape useless")]
    #[test_case("quad i i i i")]
    #[test_case("quad b i i i")]
    #[test_case("quad i i b i")]
    #[test_case("quad b i b i")]
    #[test_case("quad i i l i")]
    #[test_case("quad b i l i")]
    #[test_case("quad i i ld i")]
    #[test_case("quad b i ld i")]
    #[test_case("quad i i ll i")]
    #[test_case("quad b i ll i")]
    #[test_case("quad i i lb i")]
    #[test_case("quad b i lb i")]
    #[test_case("quad i i t i")]
    #[test_case("quad b i t i")]
    #[test_case("quad i i i b")]
    #[test_case("quad b i i b")]
    #[test_case("quad i i b b")]
    #[test_case("quad b i b b")]
    #[test_case("quad i i l b")]
    #[test_case("quad b i l b")]
    #[test_case("quad i i ld b")]
    #[test_case("quad b i ld b")]
    #[test_case("quad i i ll b")]
    #[test_case("quad b i ll b")]
    #[test_case("quad i i lb b")]
    #[test_case("quad b i lb b")]
    #[test_case("quad i i t b")]
    #[test_case("quad b i t b")]
    #[test_case("triple of bnodes")]
    #[test_case("triple of literals")]
    #[test_case("triple of triple terms")]
    #[test_case("triple of variables")]
    #[test_case("quad of bnodes")]
    #[test_case("quad of literals")]
    #[test_case("quad of triple terms")]
    #[test_case("quad of variables")]
    #[test_case("nested generalized triple term")]
    fn samples(name: &str) {
        let (line, count) = TESTS.get(name).unwrap();
        let mut c = 0;
        let nq_parser = GNQuadsParser::new();
        nq_parser
            .parse_str(line)
            .for_each_quad(|t| {
                println!("{t:?}");
                c += 1;
            })
            .unwrap();
        debug_assert_eq!(c, *count)
    }

    #[test_case("empty")]
    #[test_case("comment")]
    #[test_case("version")]
    #[test_case("triple i i i")]
    #[test_case("triple b i i")]
    #[test_case("triple i i b")]
    #[test_case("triple b i b")]
    #[test_case("triple i i l")]
    #[test_case("triple b i l")]
    #[test_case("triple i i ld")]
    #[test_case("triple b i ld")]
    #[test_case("triple i i ll")]
    #[test_case("triple b i ll")]
    #[test_case("triple i i lb")]
    #[test_case("triple b i lb")]
    #[test_case("triple i i t")]
    #[test_case("triple b i t")]
    #[test_case("escape")]
    #[test_case("escape useless")]
    #[test_case("quad i i i i")]
    #[test_case("quad b i i i")]
    #[test_case("quad i i b i")]
    #[test_case("quad b i b i")]
    #[test_case("quad i i l i")]
    #[test_case("quad b i l i")]
    #[test_case("quad i i ld i")]
    #[test_case("quad b i ld i")]
    #[test_case("quad i i ll i")]
    #[test_case("quad b i ll i")]
    #[test_case("quad i i lb i")]
    #[test_case("quad b i lb i")]
    #[test_case("quad i i t i")]
    #[test_case("quad b i t i")]
    #[test_case("quad i i i b")]
    #[test_case("quad b i i b")]
    #[test_case("quad i i b b")]
    #[test_case("quad b i b b")]
    #[test_case("quad i i l b")]
    #[test_case("quad b i l b")]
    #[test_case("quad i i ld b")]
    #[test_case("quad b i ld b")]
    #[test_case("quad i i ll b")]
    #[test_case("quad b i ll b")]
    #[test_case("quad i i lb b")]
    #[test_case("quad b i lb b")]
    #[test_case("quad i i t b")]
    #[test_case("quad b i t b")]
    #[test_case("triple of bnodes")]
    #[test_case("triple of literals")]
    #[test_case("triple of triple terms")]
    #[test_case("triple of variables")]
    #[test_case("quad of bnodes")]
    #[test_case("quad of literals")]
    // #[test_case("quad of triple terms")] // not supported by isomorphic_datasets
    #[test_case("quad of variables")]
    #[test_case("nested generalized triple term")]
    fn roundtrip(key: &str) -> Result<(), Box<dyn std::error::Error>> {
        let (nq, _) = TESTS.get(key).unwrap();
        let d1: Vec<Spog<SimpleTerm>> = crate::parser::gnq::parse_str(nq).collect_quads()?;

        let config = NQuadsConfig::new();
        let out = NQuadsSerializer::new_stringifier_with_config(config)
            .serialize_quads(d1.quads())?
            .to_string();
        println!("\n>>> DEBUG\n{}", &out);

        let d2: Vec<Spog<SimpleTerm>> = crate::parser::gnq::parse_str(&out).collect_quads()?;

        assert!(isomorphic_datasets(&d1, &d2)?);
        Ok(())
    }

    #[test_case("<x:s> <x:p> <x:o>. <x:s2>", (0, 19); "eol")]
    #[test_case("[]", (0, 0); "version or subject")]
    #[test_case("<x:s> a", (0, 6); "IRIREF as predicate")]
    #[test_case("<x:s> <x:p> \"a\" ^ <err:>", (0, 16); "not a single caret")]
    #[test_case("<x:s> <x:p> \"a\" ^^ x", (0, 19); "IRIREF as datatype")]
    #[test_case("<x:s> <x:p> 1", (0, 12); "object")]
    #[test_case("<x:s> <x:p> <<(<x:s> <x:p> <x:o> #", (0, 33); "closing triple-term bracket")]
    #[test_case("<x:s> <x:p> <<( []", (0, 16); "subject")]
    #[test_case("VERSION 1.2", (0, 8); "version specifier in quotes")]
    #[test_case("<x:s> <x:p> <x:o>", (0, 17); "period after object, got eol")]
    #[test_case("<x:s> <x:p> <x:o> <x:g> <x:x>", (0, 24); "period after object, got spurious iri")]
    #[test_case("<x:s> <x:p> <x:o> #", (0, 18); "period after object, got comment")]
    fn err_expected(
        input: &str,
        exp_pos: (usize, usize),
    ) -> Result<(), Box<dyn std::error::Error>> {
        for i in [input, &format!("{input}\n # this is a comment")] {
            let mut qs = GNQuadsParser::new().parse_str(i);
            let Err(err) = qs.for_each_quad(|_| ()) else {
                panic!()
            };
            assert!(
                matches!(dbg!(err.kind()), ErrorKind::Expected(_)),
                "{err:#?}"
            );
            assert_eq!(err.position(), exp_pos);
        }
        Ok(())
    }
}
