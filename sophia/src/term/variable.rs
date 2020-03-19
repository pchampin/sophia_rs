//! Variables like used in SPARQL or universally quantified variables in
//! Notation3.
//!

use super::{Result, Term, TermData, TermError};
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
    pub static ref VARNAME: Regex = Regex::new(r#"(?x)
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

    /// Clone self while transforming the inner `TermData` with the given
    /// factory.
    ///
    /// Clone as this might allocate a new `TermData`. However there is also
    /// `TermData` that is cheap to clone, i.e. `Copy`.
    pub fn clone_with<'a, U, F>(&'a self, mut factory: F) -> Variable<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        Variable(factory(self.as_ref()))
    }

    /// Writes the variable to the `fmt::Write` using the N3/SPARQL syntax.
    pub fn write_fmt<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        w.write_char('?')?;
        w.write_str(self.as_ref())
    }

    /// Writes the variable to the `io::Write` using the N3/SPARQL syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        w.write_all(b"?")?;
        w.write_all(self.as_ref().as_bytes())
    }

    /// Return a copy of this variables's underlying identifier.
    pub fn value(&self) -> String {
        self.as_ref().to_owned()
    }
}

impl<T, U> PartialEq<Variable<U>> for Variable<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Variable<U>) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<TD> PartialEq<str> for Variable<TD>
where
    TD: TermData,
{
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
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

impl<TD> AsRef<str> for Variable<TD>
where
    TD: TermData,
{
    /// As variable is merely a wrapper around `TermData`.
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<'a, T, U> From<&'a Variable<U>> for Variable<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    fn from(other: &'a Variable<U>) -> Self {
        other.clone_with(T::from)
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
            Term::Variable(var) => Ok(var.clone_with(T::from)),
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
