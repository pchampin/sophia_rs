// this module is transparently re-exported by its parent `stream`

use super::*;

/// The result of
/// [`TripleSource::map_triples`](./trait.TripleSource.html#method.map_triples)
/// or
/// [`QuadSource::map_quads`](../../quad/stream/trait.QuadSource.html#method.map_quads)
pub struct MapSource<S, F> {
    pub(crate) source: S,
    pub(crate) map: F,
}

impl<S, F, U> TripleSource for MapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> U,
    U: Triple,
{
    type Error = S::Error;
    type Triple = ByValue<U>;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let map = &mut self.map;
        self.source
            .try_for_some_triple(&mut |t| f(StreamedTriple::by_value((map)(t))))
    }
}

// TODO impl IntoIter for MapSource where U: 'static'

