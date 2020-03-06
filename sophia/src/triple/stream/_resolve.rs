use super::streaming_mode::{ByValue, TripleStreamingMode, UnsafeTriple};
use super::{SinkError, SourceError, StreamError, StreamResult, StreamedTriple, TripleSource};
use crate::term::iri::{Iri, IriParsed, Resolve};
use crate::term::{Result, Term, TermData, TermError};

/// An Adapter for `Iterator`s, `Triple-` and `QuadSource`s.
///
/// It takes an element that possibly contains an IRI and resolves it against
/// its base IRI.
pub struct Resolver<'a, S> {
    base: IriParsed<'a>,
    source: S,
}

impl<'a, S> Resolver<'a, S> {
    /// Build a new resolver with a pre-existing base.
    pub fn new(base: IriParsed<'a>, source: S) -> Self {
        Self { base, source }
    }

    /// Build a new resolver with building the base IRI from a given IRI.
    ///
    /// # Errors
    ///
    /// As parsing requires [mono-formed IRI](term/iri/struct.iri.html#method.parse_components)
    /// this function may fail.
    pub fn from_iri<TD: TermData>(base: &'a Iri<TD>, source: S) -> Result<Self> {
        let base = base.parse_components()?;
        Ok(Self::new(base, source))
    }

    /// Build a new resolver with building the base IRI from a given IRI.
    ///
    /// # Errors
    ///
    /// As parsing requires [mono-formed IRI](term/iri/struct.iri.html#method.parse_components)
    /// this function may fail.
    pub fn from_term<TD: TermData>(base: &'a Term<TD>, source: S) -> Result<Self> {
        match base {
            Term::Iri(base) => {
                let base = base.parse_components()?;
                Ok(Self::new(base, source))
            }
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: base.to_string(),
                expect: "IRI".to_string(),
            }),
        }
    }
}

impl<'a, S> TripleSource for Resolver<'a, S>
where
    S: TripleSource,
    <<<S as TripleSource>::Triple as TripleStreamingMode>::UnsafeTriple as UnsafeTriple>::TermData:
        From<String>,
{
    type Error = StreamError<S::Error, TermError>;
    type Triple = ByValue<[Term<<<<S as TripleSource>::Triple as TripleStreamingMode>::UnsafeTriple as UnsafeTriple>::TermData>; 3]>;

    /// This is an adapter that might fail. Accordingly, there are three error
    /// sources. The source `TripleSource`, resolving the triples and the
    /// closure. Errors are propagated as follows:
    ///
    /// - Source - Source => From original source.
    /// - Source - Sink   => From resolving IRIs.
    /// - Sink => From closure.
    fn try_for_some_triple<F, E>(&mut self, f: &mut F) -> StreamResult<bool, Self::Error, E>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: std::error::Error,
    {
        let source = &mut self.source;
        let base = &self.base;

        source
            .try_for_some_triple(&mut |t| match base.resolve(&t) {
                Ok(resolved) => f(StreamedTriple::by_value(resolved)).map_err(SinkError),
                Err(e) => Err(SourceError(e)),
            })
            .map_err(|e| match e {
                SourceError(e) => SourceError(SourceError(e)),
                SinkError(SourceError(e)) => SourceError(SinkError(e)),
                SinkError(SinkError(e)) => SinkError(e),
            })
    }
}
