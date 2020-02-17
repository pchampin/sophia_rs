// this module is transparently re-exported by its parent `stream`

use super::*;

use std::collections::VecDeque;

/// The result of
/// [`QuadSource::filter_map_quads`](./trait.QuadSource.html#method.filter_map_quads)
pub struct FilterMapSource<S, F> {
    pub source: S,
    pub filter_map: F,
}

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

impl<S, F, T> IntoIterator for FilterMapSource<S, F> where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> Option<T>,
    T: 'static,
{
    type Item = Result<T, S::Error>;
    type IntoIter = FilterMapSourceIterator<S, F, T, S::Error>;
    fn into_iter(self) -> Self::IntoIter {
        FilterMapSourceIterator {
            source: self.source,
            filter_map: self.filter_map,
            buffer: VecDeque::new(),
        }
    }
}

/// An iterator over the result of
/// [`QuadSource::filter_map_quads`](./trait.QuadSource.html#method.filter_map_quads)
pub struct FilterMapSourceIterator<S, F, T, E> {
    pub source: S,
    pub filter_map: F,
    pub buffer: VecDeque<Result<T, E>>,
}

impl<S, F, T, E> Iterator for FilterMapSourceIterator<S, F, T, E>
where
    S: QuadSource<Error=E>,
    F: FnMut(StreamedQuad<S::Quad>) -> Option<T>,
    T: 'static,
    E: 'static + std::error::Error
{
    type Item = Result<T, S::Error>;
    fn next(&mut self) -> Option<Result<T, S::Error>> {
        let mut remaining = true;
        let mut buffer = VecDeque::new();
        std::mem::swap(&mut self.buffer, &mut buffer);
        let filter_map = &mut self.filter_map;
        while self.buffer.is_empty() && remaining {
            match self.source.for_some_quad(&mut |t| {
                if let Some(v) = (filter_map)(t) {
                    buffer.push_back(Ok(v));
                }
            }) {
                Ok(b) => { remaining = b; }
                Err(err) => { buffer.push_back(Err(err)); }
            };
        }
        std::mem::swap(&mut self.buffer, &mut buffer);
        self.buffer.pop_front()
    }
}

