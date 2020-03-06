use super::streaming_mode::ByValue;
use super::{
    SinkError, SourceError, StreamError, StreamResult, StreamedTriple, TSData, TripleSource,
};
use crate::quad::stream::{QSData, QuadSource};
use crate::quad::streaming_mode::{ByValue as QByValue, StreamedQuad};
use crate::quad::TupleQuad;
use crate::term::iri::{IriParsed, Resolve};
use crate::term::{Result, Term, TermError};

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
}

impl<'a, S> TripleSource for Resolver<'a, S>
where
    S: TripleSource,
    TSData<S>: From<String>,
{
    type Error = StreamError<S::Error, TermError>;
    type Triple = ByValue<[Term<TSData<S>>; 3]>;

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

impl<'a, S> QuadSource for Resolver<'a, S>
where
    S: QuadSource,
    QSData<S>: From<String>,
{
    type Error = StreamError<S::Error, TermError>;
    type Quad = QByValue<TupleQuad<QSData<S>>>;

    /// This is an adapter that might fail. Accordingly, there are three error
    /// sources. The source `QuadSource`, resolving the quads and the
    /// closure. Errors are propagated as follows:
    ///
    /// - Source - Source => From original source.
    /// - Source - Sink   => From resolving IRIs.
    /// - Sink => From closure.
    fn try_for_some_quad<F, E>(&mut self, f: &mut F) -> StreamResult<bool, Self::Error, E>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: std::error::Error,
    {
        let source = &mut self.source;
        let base = &self.base;

        source
            .try_for_some_quad(&mut |t| match base.resolve(&t) {
                Ok(resolved) => f(StreamedQuad::by_value(resolved)).map_err(SinkError),
                Err(e) => Err(SourceError(e)),
            })
            .map_err(|e| match e {
                SourceError(e) => SourceError(SourceError(e)),
                SinkError(SourceError(e)) => SourceError(SinkError(e)),
                SinkError(SinkError(e)) => SinkError(e),
            })
    }
}
