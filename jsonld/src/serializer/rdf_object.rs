//! A private enum type used internally by JsonLdSerializer
use sophia_api::term::{IriRef, LanguageTag, TermKind, TryFromTerm};

#[derive(Clone, Debug, PartialEq)]
pub enum RdfObject {
    LangString(Box<str>, LanguageTag<Box<str>>),
    TypedLiteral(Box<str>, IriRef<Box<str>>),
    Node(usize, Box<str>),
}

impl RdfObject {
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            RdfObject::LangString(..) | RdfObject::TypedLiteral(..)
        )
    }
    pub fn is_node(&self) -> bool {
        matches!(self, RdfObject::Node(..))
    }
    pub fn eq_node(&self, other_id: &str) -> bool {
        matches!(self, RdfObject::Node(_, id) if id.as_ref()==other_id)
    }
    pub fn as_str(&self) -> &str {
        match self {
            RdfObject::LangString(lit, _) => lit.as_ref(),
            RdfObject::TypedLiteral(lit, _) => lit.as_ref(),
            RdfObject::Node(_, id) => id,
        }
    }
}

impl From<(usize, String)> for RdfObject {
    fn from(other: (usize, String)) -> Self {
        RdfObject::Node(other.0, other.1.into())
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
                    Ok(RdfObject::LangString(lex, tag))
                } else {
                    let dt = term.datatype().unwrap().map_unchecked(Into::into);
                    Ok(RdfObject::TypedLiteral(lex, dt))
                }
            }
            _ => Err(RdfObjectError {}),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Only literals can be converted to RdfObject")]
pub struct RdfObjectError {}
