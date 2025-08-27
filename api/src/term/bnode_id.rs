//! I define the [`BnodeId`] wrapper type,
//! which guarantees that the underlying `str`
//! satisfies the `BLANK_NODE_LABEL` rule in [Turtle](https://www.w3.org/TR/turtle/#grammar-production-BLANK_NODE_LABEL)
//! (without the leading `_:`).
use super::*;
use lazy_static::lazy_static;
use regex::Regex;
use sophia_iri::wrap;
use std::borrow::Borrow;
use std::fmt::Debug;
use thiserror::Error;

lazy_static! {
    /// A modified production of Turtle's BLANK_NODE_LABEL according to the
    /// [Turtle spec](https://www.w3.org/TR/turtle/#grammar-production-BLANK_NODE_LABEL).
    ///
    /// In contrast to the original rule this regular expression does not look
    /// for a leading `_:`. Accordingly it only checks if the label is valid.
    ///
    /// Actually, this regex is also valid for Notation3 nodes. Even Turtle is
    /// a derivate of N3, it does not change the syntax of blank nodes.
    ///
    /// # Captures
    ///
    /// This regular expression matches the whole input (`^...$`),
    /// therefore, it can not be used to capture `BLANK_NODE_LABEL`s in an arbitrary string.
    ///
    /// # Rule
    ///
    /// `BLANK_NODE_LABEL ::= (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?`
    static ref BNODE_ID: Regex = Regex::new(r"(?x)
      ^
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9]
      (
          [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
          |
          \u{2e} [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
      )*
      $
    ").unwrap();
}

wrap! { BnodeId borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `BLANK_NODE_LABEL` rule in [Turtle](https://www.w3.org/TR/turtle/#grammar-production-BLANK_NODE_LABEL)
    /// (without the leading `_:`).
    pub fn new(id: T) -> Result<Self, InvalidBnodeId> {
        if BNODE_ID.is_match(id.borrow()) {
            Ok(BnodeId(id))
        } else {
            Err(InvalidBnodeId(id.borrow().to_string()))
        }
    }
}
/// This error is raised when trying to parse an invalid blank node identifier.
#[derive(Debug, Error)]
#[error("The given blank node identifier '{0}' does not comply with Turtle's BLANK_NODE_LABEL")]
pub struct InvalidBnodeId(pub String);

impl<T> Term for BnodeId<T>
where
    T: Borrow<str>,
{
    type BorrowTerm<'x>
        = &'x Self
    where
        T: 'x;

    fn kind(&self) -> TermKind {
        TermKind::BlankNode
    }
    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        Some(self.as_ref().map_unchecked(MownStr::from_ref))
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("x")]
    #[test_case("_"; "underscore")]
    #[test_case("foo_bar_baz")]
    #[test_case("hé_hé")]
    #[test_case("1")]
    #[test_case("abc42")]
    #[test_case("a.b"; "with dot")]
    fn valid(tag: &str) {
        assert!(BnodeId::new(tag).is_ok());
    }

    #[test_case(""; "empty")]
    #[test_case(" "; "space")]
    #[test_case("a."; "trailing dot")]
    #[test_case(".b"; "leading dot")]
    #[test_case("a,b"; "with comma")]
    #[test_case("a:b"; "with colon")]
    #[test_case("a b"; "with space")]
    fn invalid(tag: &str) {
        assert!(BnodeId::new(tag).is_err());
    }
}
