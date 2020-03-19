// this module is privatedly used by its parent `streaming_mode`

use std::ptr::NonNull;

use crate::triple::Triple;
use sophia_term::{Term, TermData};

pub trait UnsafeTriple {
    type TermData: TermData;
    unsafe fn u_s(&self) -> &Term<Self::TermData>;
    unsafe fn u_p(&self) -> &Term<Self::TermData>;
    unsafe fn u_o(&self) -> &Term<Self::TermData>;
}

impl<T: Triple> UnsafeTriple for T {
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
}

impl<T: Triple> UnsafeTriple for NonNull<T> {
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
}

impl<TD: TermData> UnsafeTriple for [NonNull<Term<TD>>; 3] {
    type TermData = TD;
    #[inline]
    unsafe fn u_s(&self) -> &Term<Self::TermData> {
        self[0].as_ref()
    }
    #[inline]
    unsafe fn u_p(&self) -> &Term<Self::TermData> {
        self[1].as_ref()
    }
    #[inline]
    unsafe fn u_o(&self) -> &Term<Self::TermData> {
        self[2].as_ref()
    }
}

// adapters

/// Expose an `UnsafeTriple` as an `UnsafeQuad`.
/// Used internally by [`graph::adapter`](../../graph/adapter/index.html).
pub struct UnsafeQuadAdapter<T: UnsafeTriple>(pub(crate) T);
impl<T: UnsafeTriple> crate::quad::streaming_mode::UnsafeQuad for UnsafeQuadAdapter<T> {
    type TermData = T::TermData;
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
    #[inline]
    unsafe fn u_g(&self) -> Option<&Term<Self::TermData>> {
        None
    }
}
