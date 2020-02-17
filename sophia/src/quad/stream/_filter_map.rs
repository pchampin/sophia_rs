// this module is transparently re-exported by its parent `stream`

use super::*;

impl<S, F, U> QuadSource for FilterMapSource<S, F>
where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> Option<U>,
    U: Quad,
{
    type Error = S::Error;
    type Quad = ByValue<U>;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let filter_map = &mut self.filter_map;
        self.source.try_for_some_quad(&mut |t| {
            if let Some(u) = (filter_map)(t) {
                f(StreamedQuad::by_value(u))
            } else {
                Ok(())
            }
        })
    }
}

// TODO impl TripleSource where S: QuadSource and U: Triple

// TODO impl QuadSource where S: TripleSource and U: Quad

