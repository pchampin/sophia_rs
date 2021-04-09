//! Trait implementations for the wrapper types of `sophia_iri`.

use super::{RawValue, TTerm, TermKind};
use sophia_iri::{Iri, IriBox, IriRef, IriRefBox};

impl<'a> TTerm for Iri<'a> {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn value_raw(&self) -> RawValue {
        RawValue(&*self, None)
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<'a> TTerm for IriRef<'a> {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn value_raw(&self) -> RawValue {
        RawValue(&*self, None)
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl TTerm for IriBox {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn value_raw(&self) -> RawValue {
        RawValue(&*self, None)
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl TTerm for IriRefBox {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn value_raw(&self) -> RawValue {
        RawValue(&*self, None)
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}
