// this module is transparently re-exported by its parent `stream`

use std::convert::Infallible;
use std::iter::Map;

use super::*;

impl<I, T, E> TripleSource for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Triple,
    E: 'static + Error,
{
    type Error = E;
    type Triple = ByValue<T>;

    fn try_for_some_triple<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> Result<(), EF>,
        EF: Error,
    {
        match self.next() {
            Some(Ok(triple)) => f(StreamedTriple::by_value(triple))
                .map_err(SinkError)
                .and(Ok(true)),
            Some(Err(err)) => Err(SourceError(err)),
            None => Ok(false),
        }
    }

    fn size_hint_triples(&self) -> (usize, Option<usize>) {
        self.size_hint()
    }
}

pub type AsInfallibleSource<I, T> = Map<I, fn(T) -> Result<T, Infallible>>;

/// A utility extension trait for converting any iterator of [`Triple`]s
/// into [`TripleSource`], by wrapping its items in `Ok` results.
///
/// [`TripleSource`]: trait.TripleSource.html
/// [`Triple`]: ../trait.Triple.html
pub trait AsTripleSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn as_triple_source(self) -> AsInfallibleSource<Self, T>;
}

impl<T, I> AsTripleSource<T> for I
where
    I: Iterator<Item = T> + Sized,
    T: Triple,
{
    fn as_triple_source(self) -> AsInfallibleSource<Self, T> {
        self.map(Ok)
    }
}
