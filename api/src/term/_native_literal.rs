use super::*;
use crate::ns::xsd;

lazy_static::lazy_static! {
    static ref XSD_DOUBLE: Box<str> = xsd::double.iri().unwrap().unwrap().into();
    static ref XSD_INTEGER: Box<str> = xsd::integer.iri().unwrap().unwrap().into();
    static ref XSD_STRING: Box<str> = xsd::string.iri().unwrap().unwrap().into();
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
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn std::error::Error>> {
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
    fn lexical_value(&self) -> Option<MownStr> {
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
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn std::error::Error>> {
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
    fn lexical_value(&self) -> Option<MownStr> {
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
/// # fn test<T: MutableGraph>(graph: &mut T) -> Result<(), Box<dyn std::error::Error>> {
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
    fn lexical_value(&self) -> Option<MownStr> {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn i32_as_literal() {
        let lit = 42;
        test_term_impl::<i32>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_value().unwrap(), "42");
        assert_eq!(lit.datatype(), xsd::integer.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn f64_as_literal() {
        #[allow(clippy::approx_constant)]
        let lit = 3.14;
        test_term_impl::<f64>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_value().unwrap(), "3.14");
        assert_eq!(lit.datatype(), xsd::double.iri());
        assert_eq!(lit.borrow_term(), lit);
    }

    #[test]
    fn str_as_literal() {
        let lit = "hello world";
        test_term_impl::<&str>(&lit);
        assert_eq!(lit.kind(), TermKind::Literal);
        assert_eq!(lit.lexical_value().unwrap(), lit);
        assert_eq!(lit.datatype(), xsd::string.iri());
        assert_eq!(lit.borrow_term(), lit);
    }
}
