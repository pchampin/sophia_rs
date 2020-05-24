//! Streaming modes for triple iterators and triple streams.
//!
//! # What this is about
//!
//! The [`Graph`] traits provides a number of methods for iterating over [`Triple`]s,
//! as for example the [`triples`] method.
//! Since [`Triple`] is itself a trait,
//! the actual type returned by such iterating methods needs to be specified for each
//! [`Graph`] implementation.
//! This is what this module is about.
//!
//! # In the future, the ideal solution
//!
//! Ideally, the concrete type of [`Triples`] returned by iterating methods
//! would be directly given by the [`Graph::Triple`] associated type.
//!
//! Unfortunately, in the current version of Rust,
//! this is not practical:
//! in many implementations, iterating methods would return *references*
//! to triples that are actually owned by the graph itself.
//! But the associated type [`Graph::Triple`] can not be a reference type,
//! because the *lifetime* of such references can not be specified once and for all.
//!
//! The ideal solution to this problem would be [Generic Associated Types] (GAT),
//! but until those are stabilized, we need another solution.
//!
//! # In the meantime, the workaround
//!
//! The associated type [`Graph::Triple`] is not a type implementing the [`Triple`] trait,
//! it is one of the possible [`TripleStreamingMode`]s provided by this module
//! (see below).
//! Then iterating methods are restricted to returning a particular version of
//! [`StreamedTriple<'a>`], satisfying the given streaming mode.

//! The available streaming mode are:
//!
//! * [`ByValue<T>`]: [`StreamedTriple<'a>`] will wrap an (owned) instance of `T`;
//!   it is constructed with [`StreamedTriple::by_value`];
//! * [`ByRef<T>`]: [`StreamedTriple<'a>`] will wrap a reference to `T`, valid as long as `'a`;
//!   it is constructed with [`StreamedTriple::by_ref`];
//! * [`ByRefTerms`]: [`StreamedTriple<'a>`] will wrap an array of 3 [`Term<&'a str>`];
//!   it is constructed with [`StreamedTriple::by_ref_terms`].
//! * [`ByTermRefs<T>`]: [`StreamedTriple<'a>`] will wrap an array of 3 [`&'a T`] references,
//!   valid as long as `'a`;
//!   it is constructed with [`StreamedTriple::by_term_refs`].
//!
//! NB: actually, another mode exists,
//! but is specifically designed for the [`graph::adapter`](../../graph/adapter/index.html) module,
//! should never be needed in other contexts.
//!
//! [`ByRef<T>`]: struct.ByRef.html
//! [`ByRefTerms`]: struct.ByRefTerms.html
//! [`ByTermRefs<TD>`]: struct.ByTermRefs.html
//! [`ByValue<T>`]: struct.ByValue.html
//! [Generic Associated Types]: https://github.com/rust-lang/rust/issues/44265
//! [`Graph`]: ../../graph/trait.Graph.html
//! [`Graph::Triple`]: ../../graph/trait.Graph.html#associatedtype.Triple
//! [`StreamedTriple<'a>`]: struct.StreamedTriple.html
//! [`StreamedTriple::by_value`]: struct.StreamedTriple.html#method.by_value
//! [`StreamedTriple::by_ref`]: struct.StreamedTriple.html#method.by_ref
//! [`StreamedTriple::by_term_refs`]: struct.StreamedTriple.html#method.by_term_refs
//! [`Term<TD>`]: ../../term
//! [`Term<&'a str>`]: ../../term
//! [`Triple`]: ../trait.Triple.html
//! [`triples`]: ../../graph/trait.Graph.html#tymethod.triples
//! [`TripleStreamingMode`]: trait.TripleStreamingMode.html

use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::triple::Triple;
use sophia_api::term::TTerm;
use sophia_term::RefTerm;

mod _unsafe_triple;
pub(crate) use _unsafe_triple::*;

/// See [module](./index.html) documentation.
pub trait TripleStreamingMode {
    type UnsafeTriple: UnsafeTriple;
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByValue<T: Triple>(PhantomData<T>);
impl<T: Triple> TripleStreamingMode for ByValue<T> {
    type UnsafeTriple = T;
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByRef<T: Triple>(PhantomData<T>);
impl<T: Triple> TripleStreamingMode for ByRef<T> {
    type UnsafeTriple = NonNull<T>;
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByRefTerms {}
impl TripleStreamingMode for ByRefTerms {
    type UnsafeTriple = [RefTerm<'static>; 3];
}
/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct ByTermRefs<T: TTerm + ?Sized>(PhantomData<*const T>);
impl<T: TTerm + ?Sized> TripleStreamingMode for ByTermRefs<T> {
    type UnsafeTriple = TermRefs<[NonNull<T>; 3]>;
}

/// See [module](./index.html) documentation.
#[derive(Debug)]
pub struct StreamedTriple<'a, T: TripleStreamingMode> {
    _phantom: PhantomData<&'a T::UnsafeTriple>,
    wrapped: T::UnsafeTriple,
}
impl<'a, T> StreamedTriple<'a, T>
where
    T: TripleStreamingMode,
{
    /// Raw constructor
    ///
    /// # Safety
    ///
    /// This must only be used if the unsafe triple `wrapped`
    /// is guaranteed to live for at least `'a`
    /// (the lifetime of this streamed triple).
    pub unsafe fn wrap(wrapped: T::UnsafeTriple) -> Self {
        StreamedTriple {
            _phantom: PhantomData,
            wrapped,
        }
    }
}
impl<'a, T> StreamedTriple<'a, ByValue<T>>
where
    T: Triple,
{
    pub fn by_value(triple: T) -> Self {
        StreamedTriple {
            _phantom: PhantomData,
            wrapped: triple,
        }
    }
}
impl<'a, T> StreamedTriple<'a, ByRef<T>>
where
    T: Triple,
{
    pub fn by_ref(triple: &'a T) -> Self {
        StreamedTriple {
            _phantom: PhantomData,
            wrapped: triple.into(),
        }
    }
}
impl<'a> StreamedTriple<'a, ByRefTerms> {
    pub fn by_ref_terms(s: RefTerm<'a>, p: RefTerm<'a>, o: RefTerm<'a>) -> Self {
        let s = unsafe { std::mem::transmute(s) };
        let p = unsafe { std::mem::transmute(p) };
        let o = unsafe { std::mem::transmute(o) };
        StreamedTriple {
            _phantom: PhantomData,
            wrapped: [s, p, o],
        }
    }
}
impl<'a, T> StreamedTriple<'a, ByTermRefs<T>>
where
    T: TTerm + ?Sized,
{
    pub fn by_term_refs(s: &'a T, p: &'a T, o: &'a T) -> Self {
        StreamedTriple {
            _phantom: PhantomData,
            wrapped: TermRefs([s.into(), p.into(), o.into()]),
        }
    }
}
impl<'a, T> Triple for StreamedTriple<'a, T>
where
    T: TripleStreamingMode,
{
    type Term = <T::UnsafeTriple as UnsafeTriple>::Term;
    fn s(&self) -> &Self::Term {
        unsafe { self.wrapped.u_s() }
    }
    fn p(&self) -> &Self::Term {
        unsafe { self.wrapped.u_p() }
    }
    fn o(&self) -> &Self::Term {
        unsafe { self.wrapped.u_o() }
    }
}

// adapter

pub(crate) use crate::quad::streaming_mode::FromQuad;

#[derive(Debug)]
pub struct FromTriple<T: TripleStreamingMode>(PhantomData<T>);
impl<T: TripleStreamingMode> crate::quad::streaming_mode::QuadStreamingMode for FromTriple<T> {
    type UnsafeQuad = UnsafeQuadAdapter<T::UnsafeTriple>;
}

impl<'a, T> crate::quad::streaming_mode::StreamedQuad<'a, FromTriple<T>>
where
    T: TripleStreamingMode,
{
    pub(crate) fn from_triple(triple: StreamedTriple<T>) -> Self {
        unsafe { Self::wrap(UnsafeQuadAdapter(triple.wrapped)) }
    }
}
