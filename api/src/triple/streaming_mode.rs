//! Streaming modes for triple iterators and triple streams.
//!
//! # What this is about
//!
//! The [`Graph`](crate::graph::Graph) traits provides a number of methods for iterating over [`Triple`]s,
//! as for example the [`Graph::triples`](crate::graph::Graph::triples) method.
//! Since [`Triple`] is itself a trait,
//! the actual type returned by such iterating methods needs to be specified for each
//! [`Graph`](crate::graph::Graph) implementation.
//! This is what this module is about.
//!
//! # In the future, the ideal solution
//!
//! Ideally, the concrete type of [`Triple`]s returned by iterating methods
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
//! * [`ByValue`]`<T>`: [`StreamedTriple<'a>`] will wrap an (owned) instance of `T`;
//!   it is constructed with [`StreamedTriple::by_value`];
//! * [`ByRef`]`<T>`: [`StreamedTriple<'a>`] will wrap a reference to `T`, valid as long as `'a`;
//!   it is constructed with [`StreamedTriple::by_ref`];
//! * [`ByTermRefs`]`<T>`: [`StreamedTriple`]`<'a>` will wrap an array of 3 [`&'a T`] references,
//!   valid as long as `'a`;
//!   it is constructed with [`StreamedTriple::by_term_refs`].
//!
//! In addition, the macro [`make_scoped_triple_streaming_mode`]
//! allows to create a streaming mode for any lifetime-parameterized type implementing
//! [`Triple`] (see its documentation for more details).
//!
//! NB: actually, another mode exists,
//! but is specifically designed for the [`dataset::adapter`](crate::dataset::adapter),
//! should never be needed in other contexts.
//!
//! [`Graph::Triple`]: crate::graph::Graph::Triple
//! [Generic Associated Types]: https://github.com/rust-lang/rust/issues/44265

use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::term::TTerm;
use crate::triple::Triple;

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
impl<'a, T> StreamedTriple<'a, T>
where
    T: ScopedTripleMode<'a>,
{
    pub fn scoped(triple: T::SourceTriple) -> Self {
        T::scoped(triple)
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

/// Create a [streaming mode] for lifetime-parameterized Triple types.
///
/// This macro expects two identifiers:
/// * the first one (`$mode`) will be the identifier of the streaming mode;
/// * the second one (`$tt`) is the name of a generic type implementing [`Triple`],
///   and expecting a single lifetime parameter.
///
/// It declares the streaming mode type `$mode`,
/// and add an associated function named`scoped` to `StreamedTriple<'a, $mode>`,
/// to convert an instance of `$tt<'a>` to a streamed triple.
#[macro_export]
macro_rules! make_scoped_triple_streaming_mode {
    ($(#[$attrs: meta])* $mode: ident, $tt: ident) => {
        $(#[$attrs])*
        #[derive(Debug)]
        pub struct $mode(std::marker::PhantomData<$tt<'static>>);
        impl $crate::triple::streaming_mode::TripleStreamingMode for $mode {
            type UnsafeTriple = $tt<'static>;
        }

        impl<'a> $crate::triple::streaming_mode::ScopedTripleMode<'a> for $mode {
            type SourceTriple = $tt<'a>;
            fn scoped(
                triple: $tt<'a>,
            ) -> $crate::triple::streaming_mode::StreamedTriple<'a, $mode> {
                unsafe {
                    $crate::triple::streaming_mode::StreamedTriple::wrap(std::mem::transmute(
                        triple,
                    ))
                }
            }
        }
    };
}

/// A utility trait used internally by [`make_scoped_triple_streaming_mode`].
/// It should not be implemented manually.
pub trait ScopedTripleMode<'a>: TripleStreamingMode + Sized {
    type SourceTriple: Triple + 'a;
    /// Convert a triple
    fn scoped(triple: Self::SourceTriple) -> StreamedTriple<'a, Self>;
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
