// this module is transparently re-exported by its parent `stream`

use super::*;

/// The result of
/// [`TripleSource::filter_map_triples`](./trait.TripleSource.html#method.filter_map_triples)
/// or
/// [`QuadSource::filter_map_quads`](../../quad/stream/trait.QuadSource.html#method.filter_map_quads)
pub struct FilterMapSource<S, F> {
    pub(crate) source: S,
    pub(crate) filter_map: F,
}

impl<S, F, U> TripleSource for FilterMapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> Option<U>,
    U: Triple,
{
    type Error = S::Error;
    type Triple = ByValue<U>;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let filter_map = &mut self.filter_map;
        self.source.try_for_some_triple(&mut |t| {
            if let Some(u) = (filter_map)(t) {
                f(StreamedTriple::by_value(u))
            } else {
                Ok(())
            }
        })
    }
}

// TODO impl IntoIter for FilterMapSource where U: 'static


