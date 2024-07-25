use super::*;
use crate::ns::xsd;

lazy_static::lazy_static! {
    static ref XSD_DOUBLE: Box<str> = xsd::double.iri().unwrap().unwrap().into();
    static ref XSD_INTEGER: Box<str> = xsd::integer.iri().unwrap().unwrap().into();
    static ref XSD_STRING: Box<str> = xsd::string.iri().unwrap().unwrap().into();
    static ref XSD_BOOLEAN: Box<str> = xsd::boolean.iri().unwrap().unwrap().into();
}

/// [`f64`] implements [`Term`]
/// so that Rust literals can be used as RDF literals in code.
///
/// E.g.:
/// ```
/// # use sophia_api::graph::{MutableGraph, Graph};
/// # use sophia_api::term::SimpleTerm;
/// # use sophia_api::ns::{rdf, rdfs};
/// # use sophia_iri::IriRef;
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn crate::Error>> {
/// # let subject: IriRef<&'static str> = IriRef::new("")?;
/// #
/// graph.insert(&subject, &rdf::value, 3.14)?;
/// #
/// # Ok(()) }
/// ```
impl Term for f64 {
    type BorrowTerm<'x> = Self;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn lexical_form(&self) -> Option<MownStr> {
        Some(MownStr::from(format!("{}", self)))
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(IriRef::new_unchecked(MownStr::from_str(&XSD_DOUBLE)))
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        None
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

/// [`i32`] implements [`Term`]
/// so that Rust literals can be used as RDF literals in code.
///
/// E.g.:
/// ```
/// # use sophia_api::graph::{MutableGraph, Graph};
/// # use sophia_api::term::SimpleTerm;
/// # use sophia_api::ns::{rdf, rdfs};
/// # use sophia_iri::IriRef;
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn crate::Error>> {
/// # let subject: IriRef<&'static str> = IriRef::new("")?;
/// #
/// graph.insert(&subject, &rdf::value, 42)?;
/// #
/// # Ok(()) }
/// ```
impl Term for i32 {
    type BorrowTerm<'x> = Self;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn lexical_form(&self) -> Option<MownStr> {
        Some(MownStr::from(format!("{}", self)))
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(IriRef::new_unchecked(MownStr::from_str(&XSD_INTEGER)))
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        None
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

/// [`isize`] implements [`Term`]
/// so that Rust values can be used as RDF literals in code.
///
/// E.g.:
/// ```
/// # use sophia_api::graph::{MutableGraph, Graph};
/// # use sophia_api::term::SimpleTerm;
/// # use sophia_api::ns::{rdf, rdfs};
/// # use sophia_iri::IriRef;
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn crate::Error>> {
/// # let subject: IriRef<&'static str> = IriRef::new("")?;
/// #
/// let answer: isize = 42;
/// graph.insert(&subject, &rdf::value, answer)?;
/// #
/// # Ok(()) }
/// ```
impl Term for isize {
    type BorrowTerm<'x> = Self;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn lexical_form(&self) -> Option<MownStr> {
        Some(MownStr::from(format!("{}", self)))
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(IriRef::new_unchecked(MownStr::from_str(&XSD_INTEGER)))
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        None
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

/// [`usize`] implements [`Term`]
/// so that Rust values can be used as RDF literals in code.
///
/// E.g.:
/// ```
/// # use sophia_api::graph::{MutableGraph, Graph};
/// # use sophia_api::term::SimpleTerm;
/// # use sophia_api::ns::{rdf, rdfs};
/// # use sophia_iri::IriRef;
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn crate::Error>> {
/// # let subject: IriRef<&'static str> = IriRef::new("")?;
/// #
/// let answer: usize = 42;
/// graph.insert(&subject, &rdf::value, answer)?;
/// #
/// # Ok(()) }
/// ```
impl Term for usize {
    type BorrowTerm<'x> = Self;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn lexical_form(&self) -> Option<MownStr> {
        Some(MownStr::from(format!("{}", self)))
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(IriRef::new_unchecked(MownStr::from_str(&XSD_INTEGER)))
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        None
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

/// [`str`] implements [`Term`]
/// so that Rust literals can be used as RDF literals in code.
///
/// E.g.:
/// ```
/// # use sophia_api::graph::{MutableGraph, Graph};
/// # use sophia_api::term::SimpleTerm;
/// # use sophia_api::ns::{rdf, rdfs};
/// # use sophia_iri::IriRef;
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn crate::Error>> {
/// # let subject: IriRef<&'static str> = IriRef::new("")?;
/// #
/// graph.insert(&subject, &rdfs::label, "hello world")?;
/// #
/// # Ok(()) }
/// ```
impl Term for str {
    type BorrowTerm<'x> = &'x Self where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn lexical_form(&self) -> Option<MownStr> {
        Some(MownStr::from(self))
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(IriRef::new_unchecked(MownStr::from_str(&XSD_STRING)))
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        None
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }
}

/// [`bool`] implements [`Term`]
/// so that Rust literals can be used as RDF literals in code.
///
/// E.g.:
/// ```
/// # use sophia_api::graph::{MutableGraph, Graph};
/// # use sophia_api::term::SimpleTerm;
/// # use sophia_api::ns::{rdf, rdfs};
/// # use sophia_iri::IriRef;
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn crate::Error>> {
/// # let subject: IriRef<&'static str> = IriRef::new("")?;
/// #
/// graph.insert(&subject, &rdf::value, true)?;
/// #
/// # Ok(()) }
/// ```
impl Term for bool {
    type BorrowTerm<'x> = Self;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn lexical_form(&self) -> Option<MownStr> {
        Some(MownStr::from(if *self { "true" } else { "false" }))
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(IriRef::new_unchecked(MownStr::from_str(&XSD_BOOLEAN)))
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        None
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

/// [`f64`] implements [`TryFromTerm`]
/// so that compatible datatypes can easily be converted to native Rust values.
impl TryFromTerm for f64 {
    type Error = std::num::ParseFloatError;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        if let Some(lex) = term.lexical_form() {
            if Term::eq(&term.datatype().unwrap(), xsd::double)
                || Term::eq(&term.datatype().unwrap(), xsd::float)
                || Term::eq(&term.datatype().unwrap(), xsd::decimal)
            {
                lex.parse()
            } else {
                "wrong datatype".parse()
            }
        } else {
            "not a literal".parse()
        }
    }
}

/// [`i32`] implements [`TryFromTerm`]
/// so that compatible datatypes can easily be converted to native Rust values.
impl TryFromTerm for i32 {
    type Error = std::num::ParseIntError;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        if let Some(lex) = term.lexical_form() {
            if Term::eq(&term.datatype().unwrap(), xsd::integer)
                || Term::eq(&term.datatype().unwrap(), xsd::long)
                || Term::eq(&term.datatype().unwrap(), xsd::int)
                || Term::eq(&term.datatype().unwrap(), xsd::short)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedLong)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedInt)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedShort)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedByte)
                || Term::eq(&term.datatype().unwrap(), xsd::nonNegativeInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::nonPositiveInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::negativeInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::positiveInteger)
            {
                lex.parse()
            } else {
                "wrong datatype".parse()
            }
        } else {
            "not a literal".parse()
        }
    }
}

/// [`isize`] implements [`TryFromTerm`]
/// so that compatible datatypes can easily be converted to native Rust values.
impl TryFromTerm for isize {
    type Error = std::num::ParseIntError;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        if let Some(lex) = term.lexical_form() {
            if Term::eq(&term.datatype().unwrap(), xsd::integer)
                || Term::eq(&term.datatype().unwrap(), xsd::long)
                || Term::eq(&term.datatype().unwrap(), xsd::int)
                || Term::eq(&term.datatype().unwrap(), xsd::short)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedLong)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedInt)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedShort)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedByte)
                || Term::eq(&term.datatype().unwrap(), xsd::nonNegativeInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::nonPositiveInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::negativeInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::positiveInteger)
            {
                lex.parse()
            } else {
                "wrong datatype".parse()
            }
        } else {
            "not a literal".parse()
        }
    }
}

/// [`usize`] implements [`TryFromTerm`]
/// so that compatible datatypes can easily be converted to native Rust values.
impl TryFromTerm for usize {
    type Error = std::num::ParseIntError;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        if let Some(lex) = term.lexical_form() {
            if Term::eq(&term.datatype().unwrap(), xsd::integer)
                || Term::eq(&term.datatype().unwrap(), xsd::long)
                || Term::eq(&term.datatype().unwrap(), xsd::int)
                || Term::eq(&term.datatype().unwrap(), xsd::short)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedLong)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedInt)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedShort)
                || Term::eq(&term.datatype().unwrap(), xsd::unsignedByte)
                || Term::eq(&term.datatype().unwrap(), xsd::nonNegativeInteger)
                || Term::eq(&term.datatype().unwrap(), xsd::positiveInteger)
            {
                lex.parse()
            } else {
                "wrong datatype".parse()
            }
        } else {
            "not a literal".parse()
        }
    }
}

/// [`bool`] implements [`TryFromTerm`]
/// so that compatible datatypes can easily be converted to native Rust values.
impl TryFromTerm for bool {
    type Error = std::str::ParseBoolError;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        if let Some(lex) = term.lexical_form() {
            if Term::eq(&term.datatype().unwrap(), xsd::boolean) {
                lex.parse()
            } else {
                "wrong datatype".parse()
            }
        } else {
            "not a literal".parse()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn i32_as_literal() {
        let lit = 42;
        assert_consistent_term_impl::<i32>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_form().unwrap(), "42");
        assert_eq!(lit.datatype(), xsd::integer.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn isize_as_literal() {
        let lit = 42;
        assert_consistent_term_impl::<isize>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_form().unwrap(), "42");
        assert_eq!(lit.datatype(), xsd::integer.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn usize_as_literal() {
        let lit = 42;
        assert_consistent_term_impl::<usize>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_form().unwrap(), "42");
        assert_eq!(lit.datatype(), xsd::integer.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn f64_as_literal() {
        #[allow(clippy::approx_constant)]
        let lit = 3.14;
        assert_consistent_term_impl::<f64>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_form().unwrap(), "3.14");
        assert_eq!(lit.datatype(), xsd::double.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn str_as_literal() {
        let lit = "hello world";
        assert_consistent_term_impl::<&str>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_form().unwrap(), lit);
        assert_eq!(lit.datatype(), xsd::string.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn bool_as_literal() {
        let lit = false;
        assert_consistent_term_impl::<bool>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_form().unwrap(), "false");
        assert_eq!(lit.datatype(), xsd::boolean.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn iri_to_native() {
        assert!(f64::try_from_term(xsd::ID).is_err());
        assert!(i32::try_from_term(xsd::ID).is_err());
        assert!(isize::try_from_term(xsd::ID).is_err());
        assert!(usize::try_from_term(xsd::ID).is_err());
    }

    #[test]
    fn wrong_datatype_to_native() {
        assert!(f64::try_from_term("foo").is_err());
        assert!(i32::try_from_term("foo").is_err());
        assert!(isize::try_from_term("foo").is_err());
        assert!(usize::try_from_term("foo").is_err());
    }

    #[test]
    fn correct_datatype_to_native() {
        assert_eq!(f64::try_from_term(3.15).unwrap(), 3.15);
        assert_eq!(i32::try_from_term(42).unwrap(), 42);
        assert_eq!(isize::try_from_term(42).unwrap(), 42);
        assert_eq!(usize::try_from_term(42).unwrap(), 42);
    }
}
