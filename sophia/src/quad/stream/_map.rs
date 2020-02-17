// this module is transparently re-exported by its parent `stream`

use super::*;

impl<S, F, U> QuadSource for MapSource<S, F>
where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> U,
    U: Quad,
{
    type Error = S::Error;
    type Quad = ByValue<U>;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let map = &mut self.map;
        self.source
            .try_for_some_quad(&mut |t| f(StreamedQuad::by_value((map)(t))))
    }
}

// TODO impl TripleSource where S: QuadSource and U: Triple

// TODO impl QuadSource where S: TripleSource and U: Quad
