//! I define the [`VarName`] wrapper type,
//! which guarantees that the underlying `str`
//! satisfies the `VARNAME` rule in [SPARQL](https://www.w3.org/TR/sparql11-query/#rVARNAME).

use super::*;
use lazy_static::lazy_static;
use regex::Regex;
use sophia_iri::wrap;
use std::borrow::Borrow;
use std::fmt::Debug;
use thiserror::Error;

lazy_static! {
    /// Production of SPARQL's VARNAME according to the
    /// [SPARQL spec](https://www.w3.org/TR/sparql11-query/#rVARNAME).
    ///
    /// # Captures
    ///
    /// This regular expression matches the whole input (`^...$`),
    /// therefore, it can not be used to capture `VARNAME`s in an arbitrary string.
    ///
    /// # Rule
    ///
    /// `VARNAME ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*`
    static ref VARNAME: Regex = Regex::new(r#"(?x)
      ^
      [_A-Za-z0-9\u{C0}-\u{D6}\u{D8}-\u{F6}\u{F8}-\u{2FF}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\U{10000}-\U{EFFFF}]
      [_A-Za-z0-9\u{B7}\u{C0}-\u{D6}\u{D8}-\u{F6}\u{F8}-\u{2FF}\u{300}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{203F}-\u{2040}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\U{10000}-\U{EFFFF}]*
      $
    "#).unwrap();
}

wrap! { VarName borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `VARNAME` rule in [SPARQL](https://www.w3.org/TR/sparql11-query/#rVARNAME).
    pub fn new(name: T) -> Result<Self, InvalidVarName> {
        if VARNAME.is_match(name.borrow()) {
            Ok(VarName(name))
        } else {
            Err(InvalidVarName(name.borrow().to_string()))
        }
    }

    /// Gets a reference to the underlying &str.
    pub fn as_str(&self) -> &str {
        self.0.borrow()
    }
}
/// This error is raised when trying to parse an invalid variable name.
#[derive(Debug, Error)]
#[error("The given variable name '{0}' does not comply with SPARQL's VARNAME")]
pub struct InvalidVarName(pub String);

impl<T> Term for VarName<T>
where
    T: Borrow<str> + Debug,
{
    type BorrowTerm<'x> = &'x Self where T: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Variable
    }
    fn variable(&self) -> Option<VarName<MownStr>> {
        Some(self.as_ref().map_unchecked(MownStr::from_str))
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
    #[test_case("foo_bar_baz")]
    #[test_case("hé_hé")]
    #[test_case("1")]
    #[test_case("abc42")]
    fn valid(tag: &str) {
        assert!(VarName::new(tag).is_ok());
    }

    #[test_case(""; "empty")]
    #[test_case(" "; "space")]
    #[test_case("."; "dot")]
    #[test_case("a.b"; "with dot")]
    #[test_case("a,b"; "with comma")]
    #[test_case("a b"; "with space")]
    fn invalid(tag: &str) {
        assert!(VarName::new(tag).is_err());
    }
}
