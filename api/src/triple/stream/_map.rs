// this module is transparently re-exported by its parent `stream`

use super::*;

use crate::quad::streaming_mode::StreamedQuad;

use std::collections::VecDeque;

/// The result of [`TripleSource::map_triples`]
pub struct MapSource<S, F> {
    pub source: S,
    pub map: F,
}

impl<S, F, T> TripleSource for MapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> T,
    T: Triple,
{
    type Error = S::Error;
    type Triple = ByValue<T>;
    fn try_for_some_triple<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedTriple<Self::Triple>) -> Result<(), E>,
        E: Error,
    {
        let map = &mut self.map;
        self.source
            .try_for_some_triple(&mut |t| f(StreamedTriple::by_value((map)(t))))
    }

    fn size_hint_triples(&self) -> (usize, Option<usize>) {
        self.source.size_hint_triples()
    }
}

impl<S, F, T> crate::quad::stream::QuadSource for MapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> T,
    T: crate::quad::Quad,
{
    type Error = S::Error;
    type Quad = crate::quad::streaming_mode::ByValue<T>;
    fn try_for_some_quad<G, E>(&mut self, f: &mut G) -> StreamResult<bool, Self::Error, E>
    where
        G: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: Error,
    {
        let map = &mut self.map;
        self.source
            .try_for_some_triple(&mut |t| f(StreamedQuad::by_value((map)(t))))
    }

    fn size_hint_quads(&self) -> (usize, Option<usize>) {
        self.source.size_hint_triples()
    }
}

impl<S, F, T> IntoIterator for MapSource<S, F>
where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> T,
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

/// An iterator over the result of [`TripleSource::map_triples`]
pub struct MapSourceIterator<S, F, T, E> {
    pub source: S,
    pub map: F,
    pub buffer: VecDeque<Result<T, E>>,
}

impl<S, F, T, E> Iterator for MapSourceIterator<S, F, T, E>
where
    S: TripleSource<Error = E>,
    F: FnMut(StreamedTriple<S::Triple>) -> T,
    T: 'static,
    E: 'static + std::error::Error,
{
    type Item = Result<T, S::Error>;
    fn next(&mut self) -> Option<Result<T, S::Error>> {
        let mut remaining = true;
        let mut buffer = VecDeque::new();
        std::mem::swap(&mut self.buffer, &mut buffer);
        let map = &mut self.map;
        while buffer.is_empty() && remaining {
            match self.source.for_some_triple(&mut |t| {
                buffer.push_back(Ok((map)(t)));
            }) {
                Ok(b) => {
                    remaining = b;
                }
                Err(err) => {
                    buffer.push_back(Err(err));
                }
            };
        }
        std::mem::swap(&mut self.buffer, &mut buffer);
        self.buffer.pop_front()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.source.size_hint_triples()
    }
}
