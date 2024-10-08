//! A private enum type used internally by `JsonLdSerializer`
use sophia_api::term::{IriRef, LanguageTag, TermKind, TryFromTerm};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RdfObject {
    LangString(Box<str>, LanguageTag<Box<str>>),
    TypedLiteral(Box<str>, IriRef<Box<str>>),
    Node(usize, Box<str>),
}

impl RdfObject {
    pub const fn is_literal(&self) -> bool {
        matches!(self, Self::LangString(..) | Self::TypedLiteral(..))
    }
    pub const fn is_node(&self) -> bool {
        matches!(self, Self::Node(..))
    }
    pub fn is_iri(&self) -> bool {
        matches!(self, Self::Node(_, id) if !id.starts_with("_:"))
    }
    pub fn eq_node(&self, other_id: &str) -> bool {
        matches!(self, Self::Node(_, id) if id.as_ref()==other_id)
    }
    pub fn as_str(&self) -> &str {
        match self {
            Self::LangString(lit, _) => lit.as_ref(),
            Self::TypedLiteral(lit, _) => lit.as_ref(),
            Self::Node(_, id) => id,
        }
    }
}

impl From<(usize, String)> for RdfObject {
    fn from(other: (usize, String)) -> Self {
        Self::Node(other.0, other.1.into())
    }
}

impl TryFromTerm for RdfObject {
    type Error = RdfObjectError;

    fn try_from_term<T: sophia_api::term::Term>(term: T) -> Result<Self, Self::Error> {
        match term.kind() {
            TermKind::Literal => {
                let lex: Box<str> = term.lexical_form().unwrap().into();
                if let Some(tag) = term.language_tag() {
                    let tag = tag.map_unchecked(Into::into);
                    Ok(Self::LangString(lex, tag))
                } else {
                    let dt = term.datatype().unwrap().map_unchecked(Into::into);
                    Ok(Self::TypedLiteral(lex, dt))
                }
            }
            _ => Err(RdfObjectError {}),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Only literals can be converted to RdfObject")]
pub struct RdfObjectError {}
