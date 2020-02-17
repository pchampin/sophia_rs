// this module is transparently re-exported by its parent `stream`

use super::*;

use std::collections::VecDeque;

/// The result of
/// [`QuadSource::map_quads`](./trait.QuadSource.html#method.map_quads)
pub struct MapSource<S, F> {
    pub source: S,
    pub map: F,
}

impl<S, F, T> QuadSource for MapSource<S, F>
where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> T,
    T: Quad,
{
    type Error = S::Error;
    type Quad = ByValue<T>;
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

impl<S, F, T> IntoIterator for MapSource<S, F> where
    S: QuadSource,
    F: FnMut(StreamedQuad<S::Quad>) -> T,
    T: 'static,
{
    type Item = Result<T, S::Error>;
    type IntoIter = MapSourceIterator<S, F, T, S::Error>;
    fn into_iter(self) -> Self::IntoIter {
        MapSourceIterator {
            source: self.source,
            map: self.map,
            buffer: VecDeque::new(),
        }
    }
}

/// An iterator over the result of
/// [`QuadSource::map_quads`](./trait.QuadSource.html#method.map_quads)
pub struct MapSourceIterator<S, F, T, E> {
    pub source: S,
    pub map: F,
    pub buffer: VecDeque<Result<T, E>>,
}

impl<S, F, T, E> Iterator for MapSourceIterator<S, F, T, E>
where
    S: QuadSource<Error=E>,
    F: FnMut(StreamedQuad<S::Quad>) -> T,
    T: 'static,
    E: 'static + std::error::Error
{
    type Item = Result<T, S::Error>;
    fn next(&mut self) -> Option<Result<T, S::Error>> {
        let mut remaining = true;
        let mut buffer = VecDeque::new();
        std::mem::swap(&mut self.buffer, &mut buffer);
        let map = &mut self.map;
        while self.buffer.is_empty() && remaining {
            match self.source.for_some_quad(&mut |q| {
                buffer.push_back(Ok((map)(q)));
            }) {
                Ok(b) => { remaining = b; }
                Err(err) => { buffer.push_back(Err(err)); }
            }
        }
        std::mem::swap(&mut self.buffer, &mut buffer);
        self.buffer.pop_front()
    }
}
