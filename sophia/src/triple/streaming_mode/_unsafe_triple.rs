// this module is privatedly used by its parent `streaming_mode`

use std::ptr::NonNull;

use crate::triple::Triple;
use sophia_api::term::TTerm;

pub trait UnsafeTriple {
    type Term: TTerm + ?Sized;
    unsafe fn u_s(&self) -> &Self::Term;
    unsafe fn u_p(&self) -> &Self::Term;
    unsafe fn u_o(&self) -> &Self::Term;
}

impl<T: Triple> UnsafeTriple for T {
    type Term = T::Term;
    #[inline]
    unsafe fn u_s(&self) -> &Self::Term {
        self.s()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Self::Term {
        self.p()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Self::Term {
        self.o()
    }
}

impl<T: Triple> UnsafeTriple for NonNull<T> {
    type Term = T::Term;
    #[inline]
    unsafe fn u_s(&self) -> &Self::Term {
        self.as_ref().s()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Self::Term {
        self.as_ref().p()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Self::Term {
        self.as_ref().o()
    }
}

pub struct TermRefs<T>(pub(crate) T);

impl<T> UnsafeTriple for TermRefs<[NonNull<T>; 3]>
where
    T: TTerm + ?Sized,
{
    type Term = T;
    #[inline]
    unsafe fn u_s(&self) -> &Self::Term {
        self.0[0].as_ref()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Self::Term {
        self.0[1].as_ref()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Self::Term {
        self.0[2].as_ref()
    }
}

// adapters

/// Expose an `UnsafeTriple` as an `UnsafeQuad`.
/// Used internally by [`graph::adapter`](../../graph/adapter/index.html).
pub struct UnsafeQuadAdapter<T: UnsafeTriple>(pub(crate) T);
impl<T: UnsafeTriple> crate::quad::streaming_mode::UnsafeQuad for UnsafeQuadAdapter<T> {
    type Term = T::Term;
    #[inline]
    unsafe fn u_s(&self) -> &Self::Term {
        self.0.u_s()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Self::Term {
        self.0.u_p()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Self::Term {
        self.0.u_o()
    }
    #[inline]
    unsafe fn u_g(&self) -> Option<&Self::Term> {
        None
    }
}
