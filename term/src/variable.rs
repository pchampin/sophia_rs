//! Variables like used in SPARQL or universally quantified variables in
//! Notation3.
//!

use super::{Result, Term, TermData, TermError};
use crate::mown_str::MownStr;
use lazy_static::lazy_static;
use regex::Regex;
use std::convert::TryFrom;
use std::fmt;
use std::io;
use std::ops::Deref;

lazy_static! {
    /// Production of SPARQL's VARNAME according to the
    /// [SPARQL spec](https://www.w3.org/TR/sparql11-query/#rVARNAME).
    ///
    /// # Captures
    ///
    /// This regular expression matches the whole input (`^...$`),
    /// therefore, it can not be used to capture `VARNAME`s in an arbitrary
    /// string.
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

/// A variable as an RDF term.
///
/// Defined in SPARQL and Notation3. However, `sophia` allows them generally
/// everywhere. Serializers and parsers might deny them.
#[derive(Clone, Copy, Debug, Eq, Hash)]
pub struct Variable<TD: TermData>(TD);

impl<TD> Variable<TD>
where
    TD: TermData,
{
    /// Return a new variable term with the given name.
    ///
    /// May fail if `name` is not a valid variable name.
    pub fn new<U>(name: U) -> Result<Self>
    where
        U: AsRef<str>,
        TD: From<U>,
    {
        if VARNAME.is_match(name.as_ref()) {
            Ok(Variable(name.into()))
        } else {
            Err(TermError::InvalidVariableName(name.as_ref().to_string()))
        }
    }

    /// Return a new variable term.
    ///
    /// # Pre-condition
    ///
    /// This function requires that `name` is a valid variable name.
    pub fn new_unchecked<U>(name: U) -> Self
    where
        U: AsRef<str>,
        TD: From<U>,
    {
        debug_assert!(VARNAME.is_match(name.as_ref()));

        Variable(name.into())
    }

    /// Borrow the inner contents of the variable.
    pub fn as_ref(&self) -> Variable<&TD> {
        Variable(&self.0)
    }

    /// Borrow the inner contents of the variable as `&str`.
    pub fn as_ref_str(&self) -> Variable<&str> {
        Variable(self.0.as_ref())
    }

    /// Create a new variable by applying `f` to the `TermData` of `self`.
    pub fn map<F, TD2>(self, f: F) -> Variable<TD2>
    where
        F: FnMut(TD) -> TD2,
        TD2: TermData,
    {
        let mut f = f;
        Variable(f(self.0))
    }

    /// Maps the variable using the `Into` trait.
    pub fn map_into<TD2>(self) -> Variable<TD2>
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
    pub fn clone_map<'a, U, F>(&'a self, factory: F) -> Variable<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        let mut factory = factory;
        Variable(factory(self.0.as_ref()))
    }

    /// Apply `clone_map()` using the `Into` trait.
    pub fn clone_into<'src, U>(&'src self) -> Variable<U>
    where
        U: TermData + From<&'src str>,
    {
        self.clone_map(Into::into)
    }

    /// Borrow the variables ID.
    /// 
    /// _Note:_ The ID does not have a leading `_:`.
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    /// Writes the variable to the `fmt::Write` using the N3/SPARQL syntax.
    pub fn write_fmt<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        w.write_char('?')?;
        w.write_str(self.as_str())
    }

    /// Writes the variable to the `io::Write` using the N3/SPARQL syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        w.write_all(b"?")?;
        w.write_all(self.as_str().as_bytes())
    }

    /// Return this variables's name as text.
    pub fn value(&self) -> MownStr {
        self.as_str().into()
    }
}

impl<T, U> PartialEq<Variable<U>> for Variable<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Variable<U>) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<TD> PartialEq<str> for Variable<TD>
where
    TD: TermData,
{
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<T, U> PartialEq<Term<U>> for Variable<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Term<U>) -> bool {
        if let Term::Variable(other) = other {
            self == other
        } else {
            false
        }
    }
}

impl<TD> Deref for Variable<TD>
where
    TD: TermData,
{
    type Target = TD;

    fn deref(&self) -> &TD {
        &self.0
    }
}

impl<TD> fmt::Display for Variable<TD>
where
    TD: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_fmt(f)
    }
}

impl<TD> TryFrom<Term<TD>> for Variable<TD>
where
    TD: TermData,
{
    type Error = TermError;

    fn try_from(term: Term<TD>) -> Result<Self, Self::Error> {
        match term {
            Term::Variable(var) => Ok(var),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "variable".to_owned(),
            }),
        }
    }
}

impl<'a, T, U> TryFrom<&'a Term<U>> for Variable<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    type Error = TermError;

    fn try_from(term: &'a Term<U>) -> Result<Self, Self::Error> {
        match term {
            Term::Variable(var) => Ok(var.clone_into()),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "variable".to_owned(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("" => false ; "empty")]
    #[test_case("hans" => true ; "no questionmark")]
    #[test_case("ha?ns" => false ; "include unallowed char")]
    #[test_case("_" => true ; "underscore")]
    #[test_case("1" => true ; "number")]
    #[test_case("hans_the_1" => true ; "mixed")]
    fn check_regex(to_check: &str) -> bool {
        VARNAME.is_match(to_check)
    }

    #[test_case("" => "invalid name" ; "empty")]
    #[test_case("hans" => "?hans" ; "no questionmark")]
    #[test_case("ha?ns" => "invalid name" ; "include unallowed char")]
    #[test_case("_" => "?_" ; "underscore")]
    #[test_case("1" => "?1" ; "number")]
    #[test_case("hans_the_1" => "?hans_the_1" ; "mixed")]
    fn check_write_fmt(to_check: &str) -> String {
        let var: Variable<&str> = if let Ok(var) = Variable::new(to_check) {
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
    #[test_case("hans" => b"?hans".to_vec() ; "no questionmark")]
    #[test_case("ha?ns" => b"invalid name".to_vec() ; "include unallowed char")]
    #[test_case("_" => b"?_".to_vec() ; "underscore")]
    #[test_case("1" => b"?1".to_vec() ; "number")]
    #[test_case("hans_the_1" => b"?hans_the_1".to_vec() ; "mixed")]
    fn check_write_io(to_check: &str) -> Vec<u8> {
        let var: Variable<&str> = if let Ok(var) = Variable::new(to_check) {
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
}
