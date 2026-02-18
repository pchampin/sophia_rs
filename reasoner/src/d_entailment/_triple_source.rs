use std::marker::PhantomData;

use sophia_api::{
    source::{
        Source,
        StreamError::{SinkError, SourceError},
        StreamResult, TripleSource,
    },
    term::{SimpleTerm, Term},
    triple::Triple,
};

use crate::d_entailment::IllTypedLiteral;

use super::Recognized;

/// The iterator type returned by [`Recognized::normalize_triples`].
pub struct NormalizeTriples<D: ?Sized, TS>(TS, PhantomData<D>);

impl<D: ?Sized, TS> NormalizeTriples<D, TS> {
    pub(crate) fn new(ts: TS) -> Self {
        Self(ts, PhantomData)
    }
}

impl<D: Recognized + ?Sized, TS: TripleSource> Source for NormalizeTriples<D, TS> {
    type Item<'x> = [SimpleTerm<'x>; 3];

    type Error = NormalizeError<TS::Error>;

    fn try_for_some_item<E, F>(
        &mut self,
        mut f: F,
    ) -> sophia_api::source::StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        self.0
            .try_for_some_triple(|t| -> StreamResult<(), IllTypedLiteral, E> {
                let [s, p, o] = t.to_spo();
                let ns = D::normalize_simple_term(s.as_simple()).map_err(SourceError)?;
                let np = D::normalize_simple_term(p.as_simple()).map_err(SourceError)?;
                let no = D::normalize_simple_term(o.as_simple()).map_err(SourceError)?;
                f([ns, np, no]).map_err(SinkError)
            })
            .map_err(|err| match err {
                SourceError(ts_err) => SourceError(NormalizeError::Upstream(ts_err)),
                SinkError(SourceError(ill_formed)) => SourceError(ill_formed.into()),
                SinkError(SinkError(err)) => SinkError(err),
            })
    }
}

/// The error returned by [`NormalizeTriples`].
#[derive(Clone, Debug, thiserror::Error)]
pub enum NormalizeError<E> {
    /// An error from the normalized triple source
    #[error("Upstream")]
    Upstream(#[source] E),
    /// The triple source contains an ill-typed literal
    #[error("Ill-typed literal")]
    IllTypedLiteral(
        #[from]
        #[source]
        IllTypedLiteral,
    ),
}
