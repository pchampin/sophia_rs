// this module is privatedly used by its parent `streaming_mode`

use std::ptr::NonNull;

use crate::quad::Quad;
use sophia_term::{Term, TermData};

pub trait UnsafeQuad {
    type TermData: TermData;
    unsafe fn u_s(&self) -> &Term<Self::TermData>;
    unsafe fn u_p(&self) -> &Term<Self::TermData>;
    unsafe fn u_o(&self) -> &Term<Self::TermData>;
    unsafe fn u_g(&self) -> Option<&Term<Self::TermData>>;
}

impl<T: Quad> UnsafeQuad for T {
    type TermData = T::TermData;
    #[inline]
    unsafe fn u_s(&self) -> &Term<Self::TermData> {
        self.s()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Term<Self::TermData> {
        self.p()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Term<Self::TermData> {
        self.o()
    }
    #[inline]
    unsafe fn u_g(&self) -> Option<&Term<Self::TermData>> {
        self.g()
    }
}

impl<T: Quad> UnsafeQuad for NonNull<T> {
    type TermData = T::TermData;
    #[inline]
    unsafe fn u_s(&self) -> &Term<Self::TermData> {
        self.as_ref().s()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Term<Self::TermData> {
        self.as_ref().p()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Term<Self::TermData> {
        self.as_ref().o()
    }
    #[inline]
    unsafe fn u_g(&self) -> Option<&Term<Self::TermData>> {
        self.as_ref().g()
    }
}

impl<TD: TermData> UnsafeQuad for ([NonNull<Term<TD>>; 3], Option<NonNull<Term<TD>>>) {
    type TermData = TD;
    #[inline]
    unsafe fn u_s(&self) -> &Term<Self::TermData> {
        self.0[0].as_ref()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Term<Self::TermData> {
        self.0[1].as_ref()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Term<Self::TermData> {
        self.0[2].as_ref()
    }
    #[inline]
    unsafe fn u_g(&self) -> Option<&Term<Self::TermData>> {
        self.1.as_ref().map(|g| g.as_ref())
    }
}

// adapter

/// Expose an `UnsafeQuad` as an `UnsafeTriple`.
/// Used internally by [`dataset::adapter`](../../dataset/adapter/index.html).
pub struct UnsafeTripleAdapter<Q: UnsafeQuad>(pub(crate) Q);
impl<Q: UnsafeQuad> crate::triple::streaming_mode::UnsafeTriple for UnsafeTripleAdapter<Q> {
    type TermData = Q::TermData;
    #[inline]
    unsafe fn u_s(&self) -> &Term<Self::TermData> {
        self.0.u_s()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Term<Self::TermData> {
        self.0.u_p()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Term<Self::TermData> {
        self.0.u_o()
    }
}
