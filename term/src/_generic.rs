use std::borrow::Borrow;
use std::fmt::Debug;

use sophia_api::MownStr;
use sophia_api::ns::rdf;
use sophia_api::term::{BaseDirection, IriRef, LanguageTag, Term, TermKind, TryFromTerm};

lazy_static::lazy_static! {
    static ref RDF_LANG_STRING: IriRef<Box<str>> = rdf::langString.iri().unwrap().map_unchecked(Box::from);
    static ref RDF_DIR_LANG_STRING: IriRef<Box<str>> = rdf::langString.iri().unwrap().map_unchecked(Box::from);
}

/// This type is used in the `Literal` variant of [`ArcTerm`](crate::ArcTerm) and [`RcTerm`](crate::RcTerm).
///
/// It can however be used as a specialized [`Term`] implementation.
#[derive(Clone, Debug)]
pub enum GenericLiteral<T: Borrow<str>> {
    /// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
    Typed(T, IriRef<T>),
    /// An RDF [language-tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string) (with or without base direction)
    LanguageString(T, LanguageTag<T>, Option<BaseDirection>),
}

impl<T: Borrow<str>> GenericLiteral<T> {
    /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form) of this literal
    pub fn get_lexical_form(&self) -> &str {
        match self {
            Self::LanguageString(lex, ..) | Self::Typed(lex, ..) => lex,
        }
        .borrow()
    }

    /// The [datatype](https://www.w3.org/TR/rdf11-concepts/#dfn-datatype-iri) of this literal
    pub fn get_datatype(&self) -> IriRef<&str> {
        match self {
            Self::Typed(_, dt) => dt.as_ref(),
            Self::LanguageString(_, _, None) => RDF_LANG_STRING.as_ref(),
            Self::LanguageString(_, _, Some(_)) => RDF_DIR_LANG_STRING.as_ref(),
        }
    }

    /// The [language tag](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tag) of this literal, if any
    pub fn get_language_tag(&self) -> Option<LanguageTag<&str>> {
        match self {
            Self::Typed(..) => None,
            Self::LanguageString(_, tag, _) => Some(tag.as_ref()),
        }
    }

    /// The [base direction](https://www.w3.org/TR/rdf12-concepts/#dfn-base-direction) of this literal, if any
    pub fn get_base_direction(&self) -> Option<BaseDirection> {
        match self {
            Self::Typed(..) => None,
            Self::LanguageString(_, _, dir) => *dir,
        }
    }

    /// The [lexical form](https://www.w3.org/TR/rdf11-concepts/#dfn-lexical-form) of this literal
    pub fn unwrap_lexical_form(self) -> T {
        match self {
            Self::LanguageString(lex, ..) | Self::Typed(lex, ..) => lex,
        }
    }
}

impl<T: Borrow<str> + Debug> Term for GenericLiteral<T> {
    type BorrowTerm<'x>
        = &'x Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        Some(MownStr::from_ref(self.get_lexical_form()))
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        Some(self.get_datatype().map_unchecked(MownStr::from_ref))
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        self.get_language_tag()
            .map(|tag| tag.map_unchecked(MownStr::from_ref))
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        self.get_base_direction()
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
                let dir = term.base_direction();
                Ok(Self::LanguageString(
                    lex,
                    tag.map_unchecked(|txt| T::from(&txt)),
                    dir,
                ))
            } else {
                // the following is safe because we checked term.kind()
                let dt = unsafe { term.datatype().unwrap_unchecked() };
                Ok(Self::Typed(lex, dt.map_unchecked(|txt| T::from(&txt))))
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
        Term::hash(self, state);
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
    use sophia_api::{ns::xsd, term::assert_consistent_term_impl};

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
        assert!(GenericLiteral::<String>::try_from_term(rdf::type_).is_err());
    }
}
