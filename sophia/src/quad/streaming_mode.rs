//! Streaming modes for quad iterators and quad streams.
//!
//! See [`triple::streaming_mode`](../../triple/streaming_mode/index.html)
//! for more detail.

use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::quad::Quad;
use sophia_api::term::TTerm;
use sophia_term::RefTerm;

mod _unsafe_quad;
pub(crate) use _unsafe_quad::*;

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
pub struct ByRefTerms {}
impl QuadStreamingMode for ByRefTerms {
    type UnsafeQuad = ([RefTerm<'static>; 3], Option<RefTerm<'static>>);
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByTermRefs<T: TTerm + ?Sized>(PhantomData<*const T>);
impl<T: TTerm + ?Sized> QuadStreamingMode for ByTermRefs<T> {
    #[allow(clippy::type_complexity)]
    type UnsafeQuad = TermRefs<([NonNull<T>; 3], Option<NonNull<T>>)>;
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
impl<'a> StreamedQuad<'a, ByRefTerms> {
    pub fn by_ref_terms(
        s: RefTerm<'a>,
        p: RefTerm<'a>,
        o: RefTerm<'a>,
        g: Option<RefTerm<'a>>,
    ) -> Self {
        let s = unsafe { std::mem::transmute(s) };
        let p = unsafe { std::mem::transmute(p) };
        let o = unsafe { std::mem::transmute(o) };
        let g = unsafe { std::mem::transmute(g) };
        StreamedQuad {
            _phantom: PhantomData,
            wrapped: ([s, p, o], g),
        }
    }
}
impl<'a, T> StreamedQuad<'a, ByTermRefs<T>>
where
    T: TTerm + ?Sized,
{
    pub fn by_term_refs(
        s: &'a T,
        p: &'a T,
        o: &'a T,
        g: Option<&'a T>,
    ) -> Self {
        StreamedQuad {
            _phantom: PhantomData,
            wrapped: TermRefs(([s.into(), p.into(), o.into()], g.map(|g| g.into()))),
        }
    }
}
impl<'a, T> Quad for StreamedQuad<'a, T>
where
    T: QuadStreamingMode,
{
    type Term = <T::UnsafeQuad as UnsafeQuad>::Term;
    fn s(&self) -> &Self::Term {
        unsafe { self.wrapped.u_s() }
    }
    fn p(&self) -> &Self::Term {
        unsafe { self.wrapped.u_p() }
    }
    fn o(&self) -> &Self::Term {
        unsafe { self.wrapped.u_o() }
    }
    fn g(&self) -> Option<&Self::Term> {
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
