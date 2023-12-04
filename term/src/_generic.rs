use std::borrow::Borrow;
use std::fmt::Debug;

use sophia_api::ns::rdf;
use sophia_api::term::{
    BnodeId, FromTerm, IriRef, LanguageTag, Term, TermKind, TryFromTerm, VarName,
};
use sophia_api::MownStr;

lazy_static::lazy_static! {
    static ref RDF_LANG_STRING: IriRef<Box<str>> = rdf::langString.iri().unwrap().map_unchecked(Box::from);
}

/// A generic implementation of [`Term`].
///
/// Note that, although it is possible to use `GenericTerm<Mownstr>`,
/// it is recommended to use [`sophia_api::term::SimpleTerm`] instead.
#[derive(Clone, Debug)]
pub enum GenericTerm<T: Borrow<str>> {
    /// A straighforward implementation of [`Term`] as an enum.
    /// An [RDF IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs)
    Iri(IriRef<T>),
    /// An RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes)
    BlankNode(BnodeId<T>),
    /// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
    Literal(GenericLiteral<T>),
    /// An RDF-star [quoted triple](https://www.w3.org/2021/12/rdf-star.html#dfn-quoted)
    Triple(Box<[Self; 3]>),
    /// A SPARQL or Notation3 variable
    Variable(VarName<T>),
}

impl<T: Borrow<str> + Debug> Term for GenericTerm<T> {
    type BorrowTerm<'x> = &'x Self where Self: 'x;

    fn kind(&self) -> sophia_api::term::TermKind {
        match self {
            GenericTerm::Iri(_) => TermKind::Iri,
            GenericTerm::BlankNode(_) => TermKind::BlankNode,
            GenericTerm::Literal(_) => TermKind::Literal,
            GenericTerm::Triple(_) => TermKind::Triple,
            GenericTerm::Variable(_) => TermKind::Variable,
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn is_iri(&self) -> bool {
        matches!(self, GenericTerm::Iri(..))
    }

    fn is_blank_node(&self) -> bool {
        matches!(self, GenericTerm::BlankNode(..))
    }

    fn is_literal(&self) -> bool {
        matches!(self, GenericTerm::Literal(..))
    }

    fn is_variable(&self) -> bool {
        matches!(self, GenericTerm::Variable(..))
    }

    fn is_atom(&self) -> bool {
        !matches!(self, GenericTerm::Triple(..))
    }

    fn is_triple(&self) -> bool {
        matches!(self, GenericTerm::Triple(..))
    }

    fn iri(&self) -> Option<IriRef<sophia_api::MownStr>> {
        if let GenericTerm::Iri(iri) = self {
            Some(iri.as_ref().map_unchecked(MownStr::from_str))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<sophia_api::MownStr>> {
        if let GenericTerm::BlankNode(id) = self {
            Some(id.as_ref().map_unchecked(MownStr::from_str))
        } else {
            None
        }
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr> {
        if let GenericTerm::Literal(lit) = self {
            lit.lexical_form()
        } else {
            None
        }
    }

    fn datatype(&self) -> Option<IriRef<sophia_api::MownStr>> {
        if let GenericTerm::Literal(lit) = self {
            lit.datatype()
        } else {
            None
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<sophia_api::MownStr>> {
        if let GenericTerm::Literal(lit) = self {
            lit.language_tag()
        } else {
            None
        }
    }

    fn variable(&self) -> Option<VarName<sophia_api::MownStr>> {
        if let GenericTerm::Variable(name) = self {
            Some(name.as_ref().map_unchecked(MownStr::from_str))
        } else {
            None
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        if let GenericTerm::Triple(spo) = self {
            Some([
                spo[0].borrow_term(),
                spo[1].borrow_term(),
                spo[2].borrow_term(),
            ])
        } else {
            None
        }
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        if let GenericTerm::Triple(spo) = self {
            Some(*spo)
        } else {
            None
        }
    }
}

impl<T: Borrow<str> + Debug, T2: Term> PartialEq<T2> for GenericTerm<T> {
    fn eq(&self, other: &T2) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl<T: Borrow<str> + Debug> Eq for GenericTerm<T> {}

impl<T: Borrow<str> + Debug> std::hash::Hash for GenericTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}

impl<T: Borrow<str> + Debug, T2: Term> PartialOrd<T2> for GenericTerm<T> {
    fn partial_cmp(&self, other: &T2) -> Option<std::cmp::Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl<T: Borrow<str> + Debug> Ord for GenericTerm<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Term::cmp(self, other.borrow_term())
    }
}

impl<T: Borrow<str> + for<'x> From<&'x str>> FromTerm for GenericTerm<T> {
    fn from_term<U: Term>(term: U) -> Self {
        match term.kind() {
            TermKind::Iri => {
                // the following is safe because we checked term.kind()
                let iri = unsafe { term.iri().unwrap_unchecked() };
                GenericTerm::Iri(iri.map_unchecked(|txt| T::from(&txt)))
            }
            TermKind::Literal => {
                // the following is safe because we checked term.kind()
                let lit = unsafe { GenericLiteral::try_from_term(term).unwrap_unchecked() };
                GenericTerm::Literal(lit)
            }
            TermKind::BlankNode => {
                // the following is safe because we checked term.kind()
                let id = unsafe { term.bnode_id().unwrap_unchecked() };
                GenericTerm::BlankNode(id.map_unchecked(|txt| T::from(&txt)))
            }
            TermKind::Triple => {
                // the following is safe because we checked term.kind()
                let spo = unsafe { term.triple().unwrap_unchecked() };
                GenericTerm::Triple(Box::new(spo.map(Self::from_term)))
            }
            TermKind::Variable => {
                // the following is safe because we checked term.kind()
                let name = unsafe { term.variable().unwrap_unchecked() };
                GenericTerm::Variable(name.map_unchecked(|txt| T::from(&txt)))
            }
        }
    }
}

impl<T: Borrow<str>> From<IriRef<T>> for GenericTerm<T> {
    fn from(value: IriRef<T>) -> Self {
        GenericTerm::Iri(value)
    }
}

impl<T: Borrow<str>> From<BnodeId<T>> for GenericTerm<T> {
    fn from(value: BnodeId<T>) -> Self {
        GenericTerm::BlankNode(value)
    }
}

impl<T: Borrow<str>> From<(T, IriRef<T>)> for GenericTerm<T> {
    fn from(value: (T, IriRef<T>)) -> Self {
        GenericTerm::Literal(GenericLiteral::Typed(value.0, value.1))
    }
}

impl<T: Borrow<str>> From<(T, LanguageTag<T>)> for GenericTerm<T> {
    fn from(value: (T, LanguageTag<T>)) -> Self {
        GenericTerm::Literal(GenericLiteral::LanguageString(value.0, value.1))
    }
}

impl<T: Borrow<str>> From<VarName<T>> for GenericTerm<T> {
    fn from(value: VarName<T>) -> Self {
        GenericTerm::Variable(value)
    }
}

impl<T: Borrow<str>> From<Box<[GenericTerm<T>; 3]>> for GenericTerm<T> {
    fn from(value: Box<[GenericTerm<T>; 3]>) -> Self {
        GenericTerm::Triple(value)
    }
}

//

/// This type is mostly required as one of the variants of [`GenericTerm`].
///
/// It can however be used as a specialized [`Term`] implementation.
#[derive(Clone, Debug)]
pub enum GenericLiteral<T: Borrow<str>> {
    /// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
    Typed(T, IriRef<T>),
    /// An RDF [language-tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string)
    LanguageString(T, LanguageTag<T>),
}

impl<T: Borrow<str>> GenericLiteral<T> {
    /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form) of this literal
    pub fn get_lexical_form(&self) -> &str {
        match self {
            GenericLiteral::Typed(lex, ..) => lex,
            GenericLiteral::LanguageString(lex, ..) => lex,
        }
        .borrow()
    }

    /// The [datatype](https://www.w3.org/TR/rdf11-concepts/#dfn-datatype-iri) of this literal
    pub fn get_datatype(&self) -> IriRef<&str> {
        match self {
            GenericLiteral::Typed(_, dt) => dt.as_ref(),
            GenericLiteral::LanguageString(..) => RDF_LANG_STRING.as_ref(),
        }
    }

    /// The [language tag](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tag) of this literal, if any
    pub fn get_language_tag(&self) -> Option<LanguageTag<&str>> {
        match self {
            GenericLiteral::Typed(..) => None,
            GenericLiteral::LanguageString(_, tag) => Some(tag.as_ref()),
        }
    }
}

impl<T: Borrow<str> + Debug> Term for GenericLiteral<T> {
    type BorrowTerm<'x> = &'x Self where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr> {
        Some(MownStr::from_str(self.get_lexical_form()))
    }

    fn datatype(&self) -> Option<IriRef<sophia_api::MownStr>> {
        Some(self.get_datatype().map_unchecked(MownStr::from_str))
    }

    fn language_tag(&self) -> Option<LanguageTag<sophia_api::MownStr>> {
        self.get_language_tag()
            .map(|tag| tag.map_unchecked(MownStr::from_str))
    }
}

impl<T: Borrow<str> + for<'x> From<&'x str>> TryFromTerm for GenericLiteral<T> {
    type Error = GenericLiteralError;

    fn try_from_term<U: Term>(term: U) -> Result<Self, Self::Error> {
        if term.is_literal() {
            // the following is safe because we checked term.kind()
            let lex = unsafe { term.lexical_form().unwrap_unchecked() };
            let lex = T::from(&lex);
            if let Some(tag) = term.language_tag() {
                Ok(GenericLiteral::LanguageString(
                    lex,
                    tag.map_unchecked(|txt| T::from(&txt)),
                ))
            } else {
                // the following is safe because we checked term.kind()
                let dt = unsafe { term.datatype().unwrap_unchecked() };
                Ok(GenericLiteral::Typed(
                    lex,
                    dt.map_unchecked(|txt| T::from(&txt)),
                ))
            }
        } else {
            Err(GenericLiteralError(term.kind()))
        }
    }
}

impl<T: Borrow<str> + Debug, T2: Term> PartialEq<T2> for GenericLiteral<T> {
    fn eq(&self, other: &T2) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl<T: Borrow<str> + Debug> Eq for GenericLiteral<T> {}

impl<T: Borrow<str> + Debug> std::hash::Hash for GenericLiteral<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}

impl<T: Borrow<str> + Debug, T2: Term> PartialOrd<T2> for GenericLiteral<T> {
    fn partial_cmp(&self, other: &T2) -> Option<std::cmp::Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl<T: Borrow<str> + Debug> Ord for GenericLiteral<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Term::cmp(self, other.borrow_term())
    }
}

//

/// Error raised when trying to convert another kind of term to [`GenericLiteral`]
#[derive(Debug)]
pub struct GenericLiteralError(TermKind);

impl std::fmt::Display for GenericLiteralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Only literals can be convert to GenericLiteral, found {:?}",
            self.0
        )
    }
}

impl std::error::Error for GenericLiteralError {}

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::{
        ns::xsd,
        term::{assert_consistent_term_impl, SimpleTerm},
    };

    #[test]
    fn generic_literal_typed() {
        let glit: GenericLiteral<String> = 42.try_into_term().unwrap();
        assert_consistent_term_impl(&glit);
        assert_eq!(glit.kind(), TermKind::Literal);
        assert_eq!(glit.lexical_form().unwrap(), "42");
        assert!(Term::eq(&glit.datatype().unwrap(), xsd::integer));
    }

    #[test]
    fn generic_literal_language_string() {
        let en = LanguageTag::new_unchecked("en");
        let glit: GenericLiteral<String> = ("hello" * en).try_into_term().unwrap();
        assert_consistent_term_impl(&glit);
        assert_eq!(glit.kind(), TermKind::Literal);
        assert_eq!(glit.lexical_form().unwrap(), "hello");
        assert!(Term::eq(&glit.datatype().unwrap(), rdf::langString));
        assert_eq!(glit.language_tag().unwrap(), en);
    }

    #[test]
    fn generic_literal_from_iri_errs() {
        assert!(GenericLiteral::<String>::try_from_term(rdf::type_).is_err())
    }

    #[test]
    fn generic_term_iri() {
        let gt: GenericTerm<String> = rdf::type_.into_term();
        assert_consistent_term_impl(&gt);
        assert_eq!(gt.kind(), TermKind::Iri);
        assert!(Term::eq(&gt.iri().unwrap(), rdf::type_));
    }

    #[test]
    fn generic_term_bnode() {
        let bn = BnodeId::new_unchecked("x");
        let gt: GenericTerm<String> = bn.into_term();
        assert_consistent_term_impl(&gt);
        assert_eq!(gt.kind(), TermKind::BlankNode);
        assert_eq!(&gt.bnode_id().unwrap(), "x");
    }

    #[test]
    fn generic_term_typed_literal() {
        let gt: GenericTerm<String> = 42.into_term();
        assert_consistent_term_impl(&gt);
        assert_eq!(gt.kind(), TermKind::Literal);
        assert_eq!(gt.lexical_form().unwrap(), "42");
        assert!(Term::eq(&gt.datatype().unwrap(), xsd::integer));
    }

    #[test]
    fn generic_term_language_string() {
        let en = LanguageTag::new_unchecked("en");
        let gt: GenericTerm<String> = ("hello" * en).into_term();
        assert_consistent_term_impl(&gt);
        assert_eq!(gt.kind(), TermKind::Literal);
        assert_eq!(gt.lexical_form().unwrap(), "hello");
        assert!(Term::eq(&gt.datatype().unwrap(), rdf::langString));
        assert_eq!(gt.language_tag().unwrap(), en);
    }

    #[test]
    fn generic_term_triple() {
        let spo = [rdf::type_, rdf::type_, rdf::Property].map(Term::into_term);
        let qt = SimpleTerm::Triple(Box::new(spo));
        let gt: GenericTerm<String> = qt.into_term();
        assert_consistent_term_impl(&gt);
        assert_eq!(gt.kind(), TermKind::Triple);
        let spo = gt.triple().unwrap();
        assert!(Term::eq(spo[0], rdf::type_));
        assert!(Term::eq(spo[1], rdf::type_));
        assert!(Term::eq(spo[2], rdf::Property));
    }

    #[test]
    fn generic_term_variable() {
        let v = VarName::new_unchecked("x");
        let gt: GenericTerm<String> = v.into_term();
        assert_consistent_term_impl(&gt);
        assert_eq!(gt.kind(), TermKind::Variable);
        assert_eq!(&gt.variable().unwrap(), "x");
    }
}
