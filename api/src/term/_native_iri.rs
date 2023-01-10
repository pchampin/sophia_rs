use super::*;
use mownstr::MownStr;
use sophia_iri::{Iri, IriRef};
use std::borrow::Borrow;
use std::fmt::Debug;

impl<T> Term for Iri<T>
where
    T: Borrow<str> + Debug,
{
    type BorrowTerm<'x> = &'x Self where T: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Iri
    }
    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        Some(IriRef::new_unchecked(MownStr::from_str(self.as_str())))
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }
}

impl<T> Term for IriRef<T>
where
    T: Borrow<str> + Debug,
{
    type BorrowTerm<'x> = &'x Self where T: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Iri
    }
    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        Some(IriRef::new_unchecked(MownStr::from_str(self.as_str())))
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iri_as_term() {
        let iri_str = "https://example.org/";
        let iri = Iri::new_unchecked(iri_str);
        assert_eq!(iri.kind(), TermKind::Iri);
        assert_eq!(iri.lexical_form(), None);
        assert_eq!(iri.iri().unwrap().as_str(), iri_str);
        assert_eq!(iri.borrow_term(), &iri);
    }

    #[test]
    fn iriref_as_term() {
        let iri_str = "#me";
        let iriref = IriRef::new_unchecked(iri_str);
        assert_eq!(iriref.kind(), TermKind::Iri);
        assert_eq!(iriref.lexical_form(), None);
        assert_eq!(iriref.iri().unwrap().as_str(), iri_str);
        assert_eq!(iriref.borrow_term(), &iriref);
    }
}
