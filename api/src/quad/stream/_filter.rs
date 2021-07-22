// this module is transparently re-exported by its parent `stream`

use super::*;

/// The result of [`QuadSource::filter_quads`]
pub struct FilterSource<S, F> {
    pub source: S,
    pub filter: F,
}

impl<S, F> QuadSource for FilterSource<S, F>
where
    S: QuadSource,
    F: FnMut(&StreamedQuad<S::Quad>) -> bool,
{
    type Error = S::Error;
    type Quad = S::Quad;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let filter = &mut self.filter;
        self.source.try_for_some_quad(&mut |t| {
            if (filter)(&t) {
                f(t)
            } else {
                Ok(())
            }
        })
    }

    fn size_hint_quads(&self) -> (usize, Option<usize>) {
        (0, self.source.size_hint_quads().1)
    }
}
