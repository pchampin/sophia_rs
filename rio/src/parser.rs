//! Common implementations for adapting
//! [RIO](https://docs.rs/rio_api/) parsers.
//!
//! NB: Rio provides its own adapter for Sophia's traits (using the `sophia` features).
//! However,
//! published versions of Rio will always depend on the previously published version of Sophia,
//! which makes it impossible for Sophia itself to rely on that feature.

use crate::model::Trusted;
use sophia_api::source::{StreamError, StreamError::*, StreamResult};
use std::error::Error;

/// Wrap a Rio [`TriplesParser`](rio_api::parser::TriplesParser)
/// or [`QuadsParser`](rio_api::parser::QuadsParser)
/// into a Sophia [`TripleSource`](sophia_api::source::TripleSource)
/// or [`QuadSource`](sophia_api::source::QuadSource)
/// respectivelly.
pub struct StrictRioSource<T>(pub T);

impl<T> sophia_api::source::TripleSource for StrictRioSource<T>
where
    T: rio_api::parser::TriplesParser,
    T::Error: Error + 'static,
{
    type Triple<'x> = Trusted<rio_api::model::Triple<'x>>;

    type Error = T::Error;

    fn try_for_some_triple<EF, F>(&mut self, mut f: F) -> StreamResult<bool, T::Error, EF>
    where
        EF: Error,
        F: FnMut(Self::Triple<'_>) -> Result<(), EF>,
    {
        let parser = &mut self.0;
        if parser.is_end() {
            return Ok(false);
        }
        parser
            .parse_step(&mut |t| -> Result<(), RioStreamError<T::Error, EF>> {
                f(Trusted(t)).map_err(RioStreamError::Sink)
                // NB: RioStreamError::Source is produced implicitly by parse_step,
                // using the fact that RioStreamError<A, B> implements From<A>
            })
            .map_err(StreamError::from)
            .and(Ok(true))
    }
}

impl<T> sophia_api::source::QuadSource for StrictRioSource<T>
where
    T: rio_api::parser::QuadsParser,
    T::Error: Error + 'static,
{
    type Quad<'x> = Trusted<rio_api::model::Quad<'x>>;

    type Error = T::Error;

    fn try_for_some_quad<EF, F>(&mut self, mut f: F) -> StreamResult<bool, T::Error, EF>
    where
        EF: Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), EF>,
    {
        let parser = &mut self.0;
        if parser.is_end() {
            return Ok(false);
        }
        parser
            .parse_step(&mut |q| -> Result<(), RioStreamError<T::Error, EF>> {
                f(Trusted(q)).map_err(RioStreamError::Sink)
                // NB: RioStreamError::Source is produced implicitly by parse_step,
                // using the fact that RioStreamError<A, B> implements From<A>
            })
            .map_err(StreamError::from)
            .and(Ok(true))
    }
}

/// Wrap a Rio [`GeneralizedQuadsParser`](rio_api::parser::GeneralizedQuadsParser)
/// into a Sophia [`QuadSource`](sophia_api::source::QuadSource).
pub struct GeneralizedRioSource<T>(pub T);

impl<T> sophia_api::source::QuadSource for GeneralizedRioSource<T>
where
    T: rio_api::parser::GeneralizedQuadsParser,
    T::Error: Error + 'static,
{
    type Quad<'x> = Trusted<rio_api::model::GeneralizedQuad<'x>>;

    type Error = T::Error;

    fn try_for_some_quad<EF, F>(&mut self, mut f: F) -> StreamResult<bool, T::Error, EF>
    where
        EF: Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), EF>,
    {
        let parser = &mut self.0;
        if parser.is_end() {
            return Ok(false);
        }
        parser
            .parse_step(&mut |q| -> Result<(), RioStreamError<T::Error, EF>> {
                f(Trusted(q)).map_err(RioStreamError::Sink)
                // NB: RioStreamError::Source is produced implicitly by parse_step,
                // using the fact that RioStreamError<A, B> implements From<A>
            })
            .map_err(StreamError::from)
            .and(Ok(true))
    }
}

/// This intermediate type is required,
/// because Rio requires that the error type of triple_handler/quad_handler
/// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
///
/// This is costless, though,
/// because RioStreamError's internal representation is identical to StreamError,
/// so the final type conversion performed by into_stream_error is actually
/// just for pleasing the compiler.
enum RioStreamError<E1, E2> {
    /// Equivalent to [`StreamError::SourceError`]
    Source(E1),
    /// Equivalent to [`StreamError::SinkError`]
    Sink(E2),
}
impl<E1, E2> From<E1> for RioStreamError<E1, E2>
where
    E1: Error,
    E2: Error,
{
    fn from(other: E1) -> Self {
        RioStreamError::Source(other)
    }
}
impl<E1, E2> From<RioStreamError<E1, E2>> for StreamError<E1, E2>
where
    E1: Error,
    E2: Error,
{
    fn from(other: RioStreamError<E1, E2>) -> Self {
        match other {
            RioStreamError::Source(err) => SourceError(err),
            RioStreamError::Sink(err) => SinkError(err),
        }
    }
}
