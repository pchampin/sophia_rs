// this module is transparently re-exported by its parent `stream`

use std::convert::Infallible;
use std::iter::Map;

use super::*;

impl<I, T, E> QuadSource for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Quad,
    E: 'static + Error,
{
    type Error = E;
    type Quad = ByValue<T>;

    fn try_for_some_quad<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), EF>,
        EF: Error,
    {
        match self.next() {
            Some(Ok(quad)) => f(StreamedQuad::by_value(quad))
                .map_err(SinkError)
                .and(Ok(true)),
            Some(Err(err)) => Err(SourceError(err)),
            None => Ok(false),
        }
    }
}

pub type IntoInfallibleSource<I, T> = Map<I, fn(T) -> Result<T, Infallible>>;

#[deprecated(since = "0.6.3", note = "has been renamed to IntoInfallibleSource")]
pub use self::IntoInfallibleSource as AsInfallibleSource;

/// A utility extension trait for converting any iterator of [`Quad`]s
/// into [`QuadSource`], by wrapping its items in `Ok` results.
pub trait IntoQuadSource<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn into_quad_source(self) -> IntoInfallibleSource<Self, T>;

    #[deprecated(since = "0.6.3", note = "has been renamed to into_quad_source")]
    #[allow(clippy::wrong_self_convention)]
    fn as_quad_source(self) -> IntoInfallibleSource<Self, T> {
        self.into_quad_source()
    }
}

#[deprecated(since = "0.6.3", note = "has been renamed to IntoQuadSource")]
pub use self::IntoQuadSource as AsQuadSource;

impl<T, I> IntoQuadSource<T> for I
where
    I: Iterator<Item = T> + Sized,
    T: Quad,
{
    fn into_quad_source(self) -> IntoInfallibleSource<Self, T> {
        self.map(Ok)
    }
}
