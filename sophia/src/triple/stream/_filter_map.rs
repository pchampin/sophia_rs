// this module is transparently re-exported by its parent `stream`

use super::*;

use std::collections::VecDeque;

/// The result of
/// [`TripleSource::filter_map_triples`](./trait.TripleSource.html#method.filter_map_triples)
pub struct FilterMapSource<S, F> {
    pub source: S,
    pub filter_map: F,
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

impl<S, F, T> IntoIterator for FilterMapSource<S, F> where
    S: TripleSource,
    F: FnMut(StreamedTriple<S::Triple>) -> Option<T>,
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
/// [`TripleSource::filter_map_triples`](./trait.TripleSource.html#method.filter_map_triples)
pub struct FilterMapSourceIterator<S, F, T, E> {
    pub source: S,
    pub filter_map: F,
    pub buffer: VecDeque<Result<T, E>>,
}

impl<S, F, T, E> Iterator for FilterMapSourceIterator<S, F, T, E>
where
    S: TripleSource<Error=E>,
    F: FnMut(StreamedTriple<S::Triple>) -> Option<T>,
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
            match self.source.for_some_triple(&mut |t| {
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
