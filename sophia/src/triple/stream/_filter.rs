// this module is transparently re-exported by its parent `stream`

use super::*;

/// The result of
/// [`TripleSource::filter_triples`](./trait.TripleSource.html#method.filter_triples)
pub struct FilterSource<S, F> {
    pub source: S,
    pub filter: F,
}

impl<S, F> TripleSource for FilterSource<S, F>
where
    S: TripleSource,
    F: FnMut(&StreamedTriple<S::Triple>) -> bool,
{
    type Error = S::Error;
    type Triple = S::Triple;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let filter = &mut self.filter;
        self.source.try_for_some_triple(&mut |t| {
            if (filter)(&t) {
                f(t)
            } else {
                Ok(())
            }
        })
    }
}

