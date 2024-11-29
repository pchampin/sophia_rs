use super::*;
use crate::term::{Term, TermKind};

/// A [`Term`] produced by a [`Namespace`].
///
/// The raison d'être of this type, compared to [`IriRef<&str>`],
/// is that it stored the IRI in two parts (namespace and suffix),
/// so that the namespace can be reused by multiple distinct terms.
///
/// It makes sense for [`Namespace`]s, whose terms all have the same prefix.
#[derive(Clone, Copy, Debug)]
pub struct NsTerm<'a> {
    pub(crate) ns: IriRef<&'a str>,
    /// NB: suffix must satisfy that ns+suffix is still a valid IRI reference
    pub(crate) suffix: &'a str,
}

impl fmt::Display for NsTerm<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.ns.as_str(), self.suffix)
    }
}

impl<'a> NsTerm<'a> {
    /// Make an NsTerm without checking that it produces a valid IRI.
    pub const fn new_unchecked(ns: IriRef<&'a str>, suffix: &'a str) -> Self {
        NsTerm { ns, suffix }
    }

    /// Return an [`IriRef`] representing this term.
    pub fn iriref(&self) -> IriRef<MownStr> {
        IriRef::new_unchecked(if self.suffix.is_empty() {
            self.ns.as_str().into()
        } else {
            self.to_string().into()
        })
    }

    /// Return an [`IriRef`] representing this term.
    pub fn to_iriref(self) -> IriRef<MownStr<'a>> {
        if self.suffix.is_empty() {
            self.ns.map_unchecked(MownStr::from)
        } else {
            IriRef::new_unchecked(self.to_string().into())
        }
    }
}

impl<'a> Term for NsTerm<'a> {
    type BorrowTerm<'x>
        = &'x Self
    where
        'a: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Iri
    }
    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        Some(self.iriref())
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }
    fn eq<T: Term>(&self, other: T) -> bool {
        match other.iri() {
            Some(iri) => {
                let ns = self.ns.as_str();
                iri.as_str().starts_with(ns) && &iri[ns.len()..] == self.suffix
            }
            None => false,
        }
    }
}

impl<'a, T: Term> PartialEq<T> for NsTerm<'a> {
    fn eq(&self, other: &T) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl<'a> Eq for NsTerm<'a> {}

impl<'a> std::ops::Mul<NsTerm<'a>> for &'a str {
    type Output = crate::term::SimpleTerm<'a>;

    fn mul(self, rhs: NsTerm<'a>) -> Self::Output {
        crate::term::SimpleTerm::LiteralDatatype(self.into(), rhs.to_iriref())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ns_term_eq() {
        let ns = IriRef::new_unchecked("http://example.org/");
        let t1a = NsTerm { ns, suffix: "foo" };
        let t2a = NsTerm {
            ns,
            suffix: "foo/bar",
        };
        let t3a = NsTerm { ns, suffix: "bar" };
        let t1b = IriRef::new_unchecked("http://example.org/foo");
        let t2b = IriRef::new_unchecked("http://example.org/foo/bar");
        let t3b = IriRef::new_unchecked("http://example.org/bar");

        assert!(t1a == t1b);
        assert!(t2a == t2b);
        assert!(t3a == t3b);
        assert!(t1a != t2b);
        assert!(t2a != t3b);
        assert!(t3a != t1b);
    }
}
