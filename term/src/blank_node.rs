//! Blank node like specified in [RDF](https://www.w3.org/TR/rdf11-primer/#section-blank-node).
//!

use super::{Result, Term, TermData, TermError};
use lazy_static::lazy_static;
use mownstr::MownStr;
use regex::Regex;
use std::convert::TryFrom;
use std::fmt;
use std::io;
use std::ops::Deref;

lazy_static! {
    /// A modified production of Turtle's BLANK_NODE_LABEL according to the
    /// [Turtle spec](https://www.w3.org/TR/turtle/#grammar-production-BlankNode).
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
    /// therefore, it can not be used to capture `BlankNode`s in an arbitrary
    /// string.
    ///
    /// # Rule
    ///
    /// `BLANK_NODE_LABEL ::= (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?`
    static ref BLANK_NODE_LABEL: Regex = Regex::new(r"(?x)
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

/// Internal representation of a blank node identifier.
///
/// May be encountered when pattern-matching on [`Term`](../enum.Term.html)s
/// of the [`BNode`](../enum.Term.html#variant.BNode) variant.
/// For that purpose, note that `BlankNode`
///  - derefs implicitly to its internal type `T`;
///  - can be directly compared to a `&str` with the `==` operator.
///
/// Example :
/// ```
/// use sophia_term::*;
///
/// fn is_foobar(t: BoxTerm) -> bool {
///     match t {
///         Term::BNode(bn) =>
///             bn.starts_with("foo") || &bn == "bar",
///         _ =>
///             false,
///     }
/// }
/// ```
///
/// See [module documentation](index.html)
/// for more detail.
#[derive(Clone, Copy, Debug, Eq, Hash)]
pub struct BlankNode<TD: TermData>(TD);

impl<TD> BlankNode<TD>
where
    TD: TermData,
{
    /// Return a new blank node with the given identifier.
    ///
    /// May fail if `id` is not a valid identifier according to
    /// [`BLANK_NODE_LABEL`](https://www.w3.org/TR/n-triples/#grammar-production-BLANK_NODE_LABEL). This means that it
    /// must not include the typical leading `_:`.
    pub fn new<U>(id: U) -> Result<Self>
    where
        U: AsRef<str>,
        TD: From<U>,
    {
        if BLANK_NODE_LABEL.is_match(id.as_ref()) {
            Ok(BlankNode(id.into()))
        } else {
            Err(TermError::InvalidBlankNodeId(id.as_ref().to_string()))
        }
    }

    /// Return a new blank node with the given identifier.
    ///
    /// # Pre-condition
    ///
    /// This function requires that `id` is a valid blank node identifier.
    pub fn new_unchecked<U>(id: U) -> Self
    where
        U: AsRef<str>,
        TD: From<U>,
    {
        debug_assert!(
            BLANK_NODE_LABEL.is_match(id.as_ref()),
            "invalid bnode label {:?}",
            id.as_ref()
        );

        BlankNode(id.into())
    }

    /// Borrow the inner contents of the blank node.
    pub fn as_ref(&self) -> BlankNode<&TD> {
        BlankNode(&self.0)
    }

    /// Borrow the inner contents of the blank node as `&str`.
    pub fn as_ref_str(&self) -> BlankNode<&str> {
        BlankNode(self.0.as_ref())
    }

    /// Create a new blank node by applying `f` to the `TermData` of `self`.
    pub fn map<F, TD2>(self, f: F) -> BlankNode<TD2>
    where
        F: FnMut(TD) -> TD2,
        TD2: TermData,
    {
        let mut f = f;
        BlankNode(f(self.0))
    }

    /// Maps the blank node using the `Into` trait.
    pub fn map_into<TD2>(self) -> BlankNode<TD2>
    where
        TD: Into<TD2>,
        TD2: TermData,
    {
        self.map(Into::into)
    }

    /// Clone self while transforming the inner `TermData` with the given
    /// factory.
    ///
    /// This is done in one step in contrast to calling `clone().map(factory)`.
    pub fn clone_map<'a, U, F>(&'a self, factory: F) -> BlankNode<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        let mut factory = factory;
        BlankNode(factory(self.0.as_ref()))
    }

    /// Apply `clone_map()` using the `Into` trait.
    pub fn clone_into<'src, U>(&'src self) -> BlankNode<U>
    where
        U: TermData + From<&'src str>,
    {
        self.clone_map(Into::into)
    }

    /// Borrow the blank nodes ID.
    ///
    /// _Note:_ The ID does not have a leading `_:`.
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    /// Writes the blank node to the `fmt::Write` using the N3 syntax.
    pub fn write_fmt<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        w.write_str("_:")?;
        w.write_str(self.as_str())
    }

    /// Writes the blank node to the `io::Write` using the N3 syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        w.write_all(b"_:")?;
        w.write_all(self.as_str().as_bytes())
    }

    /// Return this blank nodes's identifier as text.
    pub fn value(&self) -> MownStr {
        self.as_str().into()
    }
}

impl<TD> fmt::Display for BlankNode<TD>
where
    TD: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_fmt(f)
    }
}

impl<TD> Deref for BlankNode<TD>
where
    TD: TermData,
{
    type Target = TD;

    fn deref(&self) -> &TD {
        &self.0
    }
}

impl<T, U> PartialEq<BlankNode<U>> for BlankNode<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &BlankNode<U>) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<TD> PartialEq<str> for BlankNode<TD>
where
    TD: TermData,
{
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<T, U> PartialEq<Term<U>> for BlankNode<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Term<U>) -> bool {
        if let Term::BNode(other) = other {
            self == other
        } else {
            false
        }
    }
}

impl<TD> TryFrom<Term<TD>> for BlankNode<TD>
where
    TD: TermData,
{
    type Error = TermError;

    fn try_from(term: Term<TD>) -> Result<Self, Self::Error> {
        match term {
            Term::BNode(bn) => Ok(bn),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "blank node".to_owned(),
            }),
        }
    }
}

impl<'a, T, U> TryFrom<&'a Term<U>> for BlankNode<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    type Error = TermError;

    fn try_from(term: &'a Term<U>) -> Result<Self, Self::Error> {
        match term {
            Term::BNode(bn) => Ok(bn.clone_into()),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "blank node".to_owned(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("" => false ; "empty")]
    #[test_case("example" => true ; "start alpha")]
    #[test_case("_" => true ; "underscore")]
    #[test_case("1" => true ; "number")]
    #[test_case("hans_the_1" => true ; "mixed")]
    #[test_case("hans.the?1" => false ; "unallowed char")]
    fn check_regex(to_check: &str) -> bool {
        BLANK_NODE_LABEL.is_match(to_check)
    }

    #[test_case("" => "invalid name" ; "empty")]
    #[test_case("hans" => "_:hans" ; "no questionmark")]
    #[test_case("ha?ns" => "invalid name" ; "include unallowed char")]
    #[test_case("_" => "_:_" ; "underscore")]
    #[test_case("1" => "_:1" ; "number")]
    #[test_case("hans_the_1" => "_:hans_the_1" ; "mixed")]
    fn check_write_fmt(to_check: &str) -> String {
        let var: BlankNode<&str> = if let Ok(var) = BlankNode::new(to_check) {
            var
        } else {
            return "invalid name".to_owned();
        };

        let mut buf = String::new();
        match var.write_fmt(&mut buf) {
            Ok(_) => buf,
            Err(_) => "failed write".to_owned(),
        }
    }

    #[test_case("" => b"invalid name".to_vec() ; "empty")]
    #[test_case("hans" => b"_:hans".to_vec() ; "no questionmark")]
    #[test_case("ha?ns" => b"invalid name".to_vec() ; "include unallowed char")]
    #[test_case("_" => b"_:_".to_vec() ; "underscore")]
    #[test_case("1" => b"_:1".to_vec() ; "number")]
    #[test_case("hans_the_1" => b"_:hans_the_1".to_vec() ; "mixed")]
    fn check_write_io(to_check: &str) -> Vec<u8> {
        let var: BlankNode<&str> = if let Ok(var) = BlankNode::new(to_check) {
            var
        } else {
            return b"invalid name".to_vec();
        };

        let mut buf = Vec::new();
        match var.write_io(&mut buf) {
            Ok(_) => buf,
            Err(_) => b"failed write".to_vec(),
        }
    }

    #[test]
    fn map() {
        let input: BlankNode<&str> = BlankNode::new("test").unwrap();
        let expect: BlankNode<&str> = BlankNode::new("TEST").unwrap();

        let mut cnt = 0;
        let mut invoked = 0;

        let cl = input.clone_map(|s: &str| {
            cnt += s.len();
            invoked += 1;
            s.to_ascii_uppercase()
        });
        assert_eq!(cl, expect);
        assert_eq!(cnt, "test".len());
        assert_eq!(invoked, 1);

        cnt = 0;
        invoked = 0;
        let mapped = input.map(|s: &str| {
            cnt += s.len();
            invoked += 1;
            s.to_ascii_uppercase()
        });
        assert_eq!(mapped, expect);
        assert_eq!(cnt, "test".len());
        assert_eq!(invoked, 1);

        assert_eq!(
            cl.map_into::<Box<str>>(),
            mapped.clone_into::<std::sync::Arc<str>>()
        );
    }

    // further tests are executed in `crate::test`
}
