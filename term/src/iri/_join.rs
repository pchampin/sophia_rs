//! Implementation of IRIs as per
//! [\[RFC 3987\]](https://tools.ietf.org/html/rfc3987).
//!
//! This module is transparently reexported by its parent module.
//!

use super::Iri;
use crate::ns::Namespace;
use crate::{Literal, MownTerm, TTerm, Term, TermData};
use mownstr::MownStr;
use sophia_iri::resolve::*;

impl<'a, 'b> Resolve<Iri<&'a str>, Iri<MownStr<'a>>> for IriParsed<'b> {
    /// Resolve the given IRI.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if `other` is suffixed.
    fn resolve(&self, other: Iri<&'a str>) -> Iri<MownStr<'a>> {
        if other.is_absolute() {
            return other.map_into();
        }
        let mut buffer = String::new();
        let parsed = other.parse_components(&mut buffer);
        let joined = self.join(&parsed);
        Iri::new_unchecked(joined.to_string())
    }
}

impl<'a, 'b, TD> Resolve<&'a Iri<TD>, Iri<MownStr<'a>>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve the given IRI.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if `other` is suffixed.
    fn resolve(&self, other: &'a Iri<TD>) -> Iri<MownStr<'a>> {
        self.resolve(other.as_ref_str())
    }
}

impl<'a, 'b, TD> Resolve<&'a Namespace<TD>, Namespace<MownStr<'a>>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve the IRI of the given `Namespace`.
    fn resolve(&self, other: &'a Namespace<TD>) -> Namespace<MownStr<'a>> {
        let iri = other.0.as_ref();
        let resolved: MownStr = self.resolve(iri).expect("Is valid as from Namespace");
        Namespace(resolved)
    }
}

impl<'a, 'b, TD> Resolve<&'a Literal<TD>, Literal<MownStr<'a>>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve the data type's IRI if it is relative.
    ///
    /// Note that this only affects datatyped literals;
    /// language-tagged literals are absolute by construction.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if `other.dt()` is suffixed.
    fn resolve(&self, other: &'a Literal<TD>) -> Literal<MownStr<'a>> {
        if other.is_absolute() {
            other.clone_into()
        } else {
            Literal::new_dt(other.txt().as_ref(), self.resolve(other.dt()))
        }
    }
}

impl<'a, 'b, TD> Resolve<&'a Term<TD>, MownTerm<'a>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve IRIs and the IRIs of typed literals.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if an IRI is suffixed.
    fn resolve(&self, other: &'a Term<TD>) -> MownTerm<'a> {
        match other {
            Term::Iri(iri) => Resolve::<_, Iri<MownStr>>::resolve(self, iri).into(),
            Term::Literal(lit) => Resolve::<_, Literal<MownStr>>::resolve(self, lit).into(),
            term => term.clone_into(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{RefTerm, TTerm};
    use sophia_iri::test::RELATIVE_IRIS;

    #[test]
    fn resolve_namespace() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = Namespace::<&str>::new(*rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.as_ref(), abs);
        }
    }

    #[test]
    fn resolve_iri() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = Iri::<&str>::new(*rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.value(), abs);
        }
    }

    #[test]
    fn resolve_literal() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = Iri::<&str>::new(*rel).unwrap();
            let lit = Literal::<&str>::new_dt("hello", rel);
            let got = base.resolve(&lit);
            assert_eq!(&got.dt().value(), abs);
        }
    }

    #[test]
    fn resolve_iri_term() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = RefTerm::new_iri(*rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.value(), abs);
        }
    }
}
