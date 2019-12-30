//! This is the quad counterpart of
//! [`triple::streaming_mode`](../../triple/streaming_mode/index.html).

use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::quad::Quad;
use crate::term::{Term, TermData};

mod _unsafe_quad;
pub(crate) use _unsafe_quad::*;

// TODO update documentation of Graph::Quad

/// See [module](./index.html) documentation.
pub trait QuadStreamingMode {
    type UnsafeQuad: UnsafeQuad;
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByValue<Q: Quad>(PhantomData<Q>);
impl<Q: Quad> QuadStreamingMode for ByValue<Q> {
    type UnsafeQuad = Q;
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByRef<Q: Quad>(PhantomData<Q>);
impl<Q: Quad> QuadStreamingMode for ByRef<Q> {
    type UnsafeQuad = NonNull<Q>;
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByTermRefs<TD: TermData>(PhantomData<TD>);
impl<TD: TermData> QuadStreamingMode for ByTermRefs<TD> {
    #[allow(clippy::type_complexity)]
    type UnsafeQuad = ([NonNull<Term<TD>>; 3], Option<NonNull<Term<TD>>>);
}

/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct StreamedQuad<'a, T: QuadStreamingMode> {
    _phantom: PhantomData<&'a T::UnsafeQuad>,
    wrapped: T::UnsafeQuad,
}
impl<'a, T> StreamedQuad<'a, T>
where
    T: QuadStreamingMode,
{
    /// Raw constructor
    ///
    /// # Safety
    ///
    /// This must only be used if the unsafe quad `wrapped`
    /// is guaranteed to live for at least `'a`
    /// (the lifetime of this streamed quad).
    pub unsafe fn wrap(wrapped: T::UnsafeQuad) -> Self {
        StreamedQuad {
            _phantom: PhantomData,
            wrapped,
        }
    }
}
impl<'a, Q> StreamedQuad<'a, ByValue<Q>>
where
    Q: Quad,
{
    pub fn by_value(quad: Q) -> Self {
        StreamedQuad {
            _phantom: PhantomData,
            wrapped: quad,
        }
    }
}
impl<'a, Q> StreamedQuad<'a, ByRef<Q>>
where
    Q: Quad,
{
    pub fn by_ref(quad: &'a Q) -> Self {
        StreamedQuad {
            _phantom: PhantomData,
            wrapped: quad.into(),
        }
    }
}
impl<'a, T> StreamedQuad<'a, ByTermRefs<T>>
where
    T: TermData,
{
    pub fn by_term_refs(
        s: &'a Term<T>,
        p: &'a Term<T>,
        o: &'a Term<T>,
        g: Option<&'a Term<T>>,
    ) -> Self {
        StreamedQuad {
            _phantom: PhantomData,
            wrapped: ([s.into(), p.into(), o.into()], g.map(|g| g.into())),
        }
    }
}
impl<'a, T> Quad for StreamedQuad<'a, T>
where
    T: QuadStreamingMode,
{
    type TermData = <T::UnsafeQuad as UnsafeQuad>::TermData;
    fn s(&self) -> &Term<Self::TermData> {
        unsafe { self.wrapped.u_s() }
    }
    fn p(&self) -> &Term<Self::TermData> {
        unsafe { self.wrapped.u_p() }
    }
    fn o(&self) -> &Term<Self::TermData> {
        unsafe { self.wrapped.u_o() }
    }
    fn g(&self) -> Option<&Term<Self::TermData>> {
        unsafe { self.wrapped.u_g() }
    }
}

// adapter

pub(crate) use crate::triple::streaming_mode::FromTriple;

/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct FromQuad<T: QuadStreamingMode>(PhantomData<T>);
impl<T: QuadStreamingMode> crate::triple::streaming_mode::TripleStreamingMode for FromQuad<T> {
    type UnsafeTriple = UnsafeTripleAdapter<T::UnsafeQuad>;
}

impl<'a, Q> crate::triple::streaming_mode::StreamedTriple<'a, FromQuad<Q>>
where
    Q: QuadStreamingMode,
{
    pub(crate) fn from_quad(quad: StreamedQuad<Q>) -> Self {
        unsafe { Self::wrap(UnsafeTripleAdapter(quad.wrapped)) }
    }
}
