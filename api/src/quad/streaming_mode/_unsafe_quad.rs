// this module is privatedly used by its parent `streaming_mode`

use std::ptr::NonNull;

use crate::quad::Quad;
use crate::term::TTerm;

pub trait UnsafeQuad {
    type Term: TTerm + ?Sized;
    unsafe fn u_s(&self) -> &Self::Term;
    unsafe fn u_p(&self) -> &Self::Term;
    unsafe fn u_o(&self) -> &Self::Term;
    unsafe fn u_g(&self) -> Option<&Self::Term>;
}

impl<Q: Quad> UnsafeQuad for Q {
    type Term = Q::Term;
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
    #[inline]
    unsafe fn u_g(&self) -> Option<&Self::Term> {
        self.g()
    }
}

impl<Q: Quad> UnsafeQuad for NonNull<Q> {
    type Term = Q::Term;
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
    #[inline]
    unsafe fn u_g(&self) -> Option<&Self::Term> {
        self.as_ref().g()
    }
}

pub struct TermRefs<T>(pub(crate) T);

impl<T> UnsafeQuad for TermRefs<([NonNull<T>; 3], Option<NonNull<T>>)>
where
    T: TTerm + ?Sized,
{
    type Term = T;
    #[inline]
    unsafe fn u_s(&self) -> &Self::Term {
        (self.0).0[0].as_ref()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Self::Term {
        (self.0).0[1].as_ref()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Self::Term {
        (self.0).0[2].as_ref()
    }
    #[inline]
    unsafe fn u_g(&self) -> Option<&Self::Term> {
        (self.0).1.as_ref().map(|g| g.as_ref())
    }
}

// adapter

/// Expose an `UnsafeQuad` as an `UnsafeTriple`.
/// Used internally by [`dataset::adapter`].
pub struct UnsafeTripleAdapter<Q: UnsafeQuad>(pub(crate) Q);
impl<Q: UnsafeQuad> crate::triple::streaming_mode::UnsafeTriple for UnsafeTripleAdapter<Q> {
    type Term = Q::Term;
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
}
