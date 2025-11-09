//! [N-Triples] parser
//!
//! [N-Triples]: https://www.w3.org/TR/rdf12-n-triples

use std::io::BufRead;

use sophia_api::{
    parser::TripleParser,
    source::{Source, StreamError::SinkError, StreamResult},
    version::Version,
};

use crate::{
    _term::StashedTerm,
    parser::{
        _common::{GenericSource, Inner, NxSource},
        _error::{AdjustCol, Error, ErrorKind, ResultExt},
    },
};

sophia_api::def_mod_functions_for_bufread_parser!(NTriplesParser, TripleParser);

/// [N-Triples] parser.
///
/// [N-Triples]: https://www.w3.org/TR/rdf12-n-triples/
#[derive(Clone, Debug, Default)]
pub struct NTriplesParser {
    /// The default version specifier.
    ///
    /// Will be applied until overridden with a `@version`/`VERSION` directive.
    pub version: Version,
}

impl NTriplesParser {
    /// Construct a [`NTriplesParser`] with default options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Change the [`version`](NTriplesParser::version) option of this parser.
    #[must_use]
    pub fn with_version(self, version: Version) -> Self {
        Self { version }
    }
}

impl<I: BufRead> TripleParser<I> for NTriplesParser {
    type Source = NTriplesSource<I>;

    fn parse(&self, data: I) -> Self::Source {
        NTriplesSource {
            input: data,
            inner: Inner {
                version: self.version,
                ..Default::default()
            },
        }
    }
}

/// [`TripleSource`](sophia_api::prelude::TripleSource) returned by a [`NTriplesParser`]
#[derive(Clone, Debug, Default)]
pub struct NTriplesSource<I> {
    pub(crate) input: I,
    pub(crate) inner: Inner,
}

impl<I> GenericSource for NTriplesSource<I> {
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
        .map(|_| offset)
        .map_err(SinkError)
    }
}

impl<I> NxSource for NTriplesSource<I> {
    fn parse_tokens<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty() && self.ws(txt) == 0);

        let mut col = 0;
        col += self.enter_subject_or_version(txt).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        if self.inner.terms.is_empty() {
            // except version specifier
            if txt[col..].starts_with('"') {
                col += 1 + self
                    .in_string_literal_short(&txt[col + 1..], b'"')
                    .adjust_col(col + 1)?;
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
            // expect predicate, object and period.
            col += self.enter_predicate(&txt[col..]).adjust_col(col)?;
            col += self.ws(&txt[col..]);
            col += self.enter_object(&txt[col..]).adjust_col(col)?;
            col += self.ws(&txt[col..]);
            if txt[col..].starts_with('.') {
                col += 1;
            } else {
                return Err(ErrorKind::Expected(".".into())).wrap_in_at(self, col);
            }
            self.emit_tuple(callback, col)?;
            self.inner.terms.truncate(0);
            self.inner.buffers.empty();
        }
        Ok(col)
    }
}

impl<I: BufRead> Source for NTriplesSource<I> {
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
            Ok(false)
        } else {
            self.parse_line(&line, f)?;
            self.inner.line += 1;
            Ok(true)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::test::{LazyMap, nt_samples};

    use super::*;
    use sophia_api::source::TripleSource;
    use test_case::test_case;

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
    fn samples(name: &str) {
        static TESTS: LazyMap = nt_samples();

        let (line, count) = TESTS.get(name).unwrap();
        let mut c = 0;
        let nt_parser = NTriplesParser::new();
        nt_parser
            .parse_str(line)
            .for_each_triple(|t| {
                println!("{t:?}");
                c += 1;
            })
            .unwrap();
        debug_assert_eq!(c, *count)
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
    #[test_case("<x:s> <x:p> <x:o> <x:g>", (0, 18); "period after object, got spurious iri")]
    #[test_case("<x:s> <x:p> <x:o> #", (0, 18); "period after object, got comment")]
    fn err_expected(
        input: &str,
        exp_pos: (usize, usize),
    ) -> Result<(), Box<dyn std::error::Error>> {
        for i in [input, &format!("{input}\n # this is a comment")] {
            let mut ts = NTriplesParser::new().parse_str(i);
            let Err(err) = ts.for_each_triple(|_| ()) else {
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
