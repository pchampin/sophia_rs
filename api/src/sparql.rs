//! Common traits for working with [SPARQL](https://www.w3.org/TR/sparql11-query/).
//!
//! # Design rationale
//!
//! These traits are deliberately very generic.
//! Specific implementations may have additional features, such as:
//!
//! - preparing a query for multiple use;
//! - setting default values for `BASE`, `PREFIX`, `FROM`, `FROM NAMED` directives,
//!   before parsing query string;
//! - pre-binding variables before evaluating query;
//! - etc...
//!
//! However, we do not want to impose these feature, or any subset thereof,
//! to all implementation of Sophia.

use crate::term::TTerm;
use crate::triple::stream::TripleSource;
use std::error::Error;

/// A dataset that can be queried with SPARQL.
pub trait SparqlDataset {
    type BindingsTerm: TTerm;
    type BindingsResult: SparqlBindings<Self>;
    type TriplesResult: TripleSource;
    type SparqlError: Error + 'static;

    /// Parse and immediately execute `query`
    fn query(&self, query: &str) -> Result<SparqlResult<Self>, Self::SparqlError>;
}

/// The result of executing a SPARQL query.
pub enum SparqlResult<T>
where
    T: SparqlDataset + ?Sized,
{
    /// The result of a SELECT query
    Bindings(T::BindingsResult),
    /// The result of an ASK query
    Boolean(bool),
    /// The result of a CONSTRUCT or DESCRIBE query
    Triples(T::TriplesResult),
}

impl<T> SparqlResult<T>
where
    T: SparqlDataset + ?Sized,
{
    /// Get this result as a `Bindings`.
    ///
    /// # Panics
    /// This will panic if `self` is actually of another kind.
    pub fn into_bindings(self) -> T::BindingsResult {
        match self {
            SparqlResult::Bindings(b) => b,
            _ => panic!("This SparqlResult is not a Bindings"),
        }
    }
    /// Get this result as a `Boolean`.
    ///
    /// # Panics
    /// This will panic if `self` is actually of another kind.
    pub fn into_boolean(self) -> bool {
        match self {
            SparqlResult::Boolean(b) => b,
            _ => panic!("This SparqlResult is not a Boolean"),
        }
    }
    /// Get this result as a `Triples`.
    ///
    /// # Panics
    /// This will panic if `self` is actually of another kind.
    pub fn into_triples(self) -> T::TriplesResult {
        match self {
            SparqlResult::Triples(t) => t,
            _ => panic!("This SparqlResult is not a Triples"),
        }
    }
}

/// The result of executing a SPARQL SELECT query
pub trait SparqlBindings<D>:
    IntoIterator<Item = Result<Vec<Option<D::BindingsTerm>>, D::SparqlError>>
where
    D: SparqlDataset + ?Sized,
{
    /// Return the list of SELECTed variable names
    fn variables(&self) -> Vec<&str>;
}

/// A dummy module to check that implementing these traits is actually possible
#[cfg(test)]
mod dummy {
    use super::*;
    use crate::{dataset::Dataset, term::test::TestTerm};
    use std::convert::Infallible;

    pub type MyTerm = TestTerm<String>;
    pub type MyQuad = ([MyTerm; 3], Option<MyTerm>);
    pub type MyDataset = Vec<MyQuad>;

    pub struct MyBindings(Box<dyn Iterator<Item = Result<Vec<Option<MyTerm>>, Infallible>>>);

    impl IntoIterator for MyBindings {
        type Item = Result<Vec<Option<MyTerm>>, Infallible>;
        type IntoIter = Box<dyn Iterator<Item = Result<Vec<Option<MyTerm>>, Infallible>>>;
        fn into_iter(self) -> Self::IntoIter {
            self.0
        }
    }
    impl SparqlBindings<MyDataset> for MyBindings {
        fn variables(&self) -> Vec<&str> {
            vec!["s"]
        }
    }

    impl SparqlDataset for MyDataset {
        type BindingsTerm = MyTerm;
        type BindingsResult = MyBindings;
        type TriplesResult = Box<dyn Iterator<Item = Result<[MyTerm; 3], Infallible>>>;
        type SparqlError = Infallible;

        fn query(&self, query: &str) -> Result<SparqlResult<Self>, Self::SparqlError> {
            match query {
                "ASK" => Ok(SparqlResult::Boolean(true)),
                "GRAPH" => Ok(SparqlResult::Triples(Box::new(
                    self.clone().into_iter().map(|q| Ok(q.0)),
                )
                    as Self::TriplesResult)),
                _ => Ok(SparqlResult::Bindings(MyBindings(Box::new(
                    self.subjects()
                        .unwrap()
                        .into_iter()
                        .map(|t| Ok(vec![Some(t)])),
                )))),
            }
        }
    }

    fn test_triple() -> [MyTerm; 3] {
        use crate::ns::xsd;
        [
            TestTerm::iri("http://example.org/person#Alice"),
            TestTerm::iri("http://www.w3.org/2001/vcard-rdf/3.0#FN"),
            TestTerm::lit_dt("Alice", xsd::string),
        ]
    }

    fn test_dataset() -> MyDataset {
        vec![(test_triple(), None)]
    }

    #[test]
    fn get_boolean_result_by_destructuring() {
        let my_dataset = test_dataset();

        assert_eq!(
            match my_dataset.query("ASK").unwrap() {
                SparqlResult::Boolean(b) => Some(b),
                _ => None,
            },
            Some(true)
        );
    }

    #[test]
    fn get_boolean_result_by_coercing() {
        let my_dataset = test_dataset();

        assert_eq!(my_dataset.query("ASK").unwrap().into_boolean(), true);
    }

    #[test]
    fn get_triples_result_by_destructuring() {
        let my_dataset = test_dataset();
        let mut triples_result = match my_dataset.query("GRAPH").unwrap() {
            SparqlResult::Triples(t) => Some(t),
            _ => None,
        }
        .unwrap();

        assert_eq!(triples_result.next(), Some(Ok(test_triple())));
        assert_eq!(triples_result.next(), None);
    }

    #[test]
    fn get_triples_result_by_coercing() {
        let my_dataset = test_dataset();
        let mut triples_result = my_dataset.query("GRAPH").unwrap().into_triples();

        assert_eq!(triples_result.next(), Some(Ok(test_triple())));
        assert_eq!(triples_result.next(), None);
    }

    #[test]
    fn get_bindings_result_by_destructuring() {
        let my_dataset = test_dataset();
        let bindings_result = match my_dataset.query("SELECT").unwrap() {
            SparqlResult::Bindings(b) => Some(b),
            _ => None,
        }
        .unwrap();

        assert_eq!(bindings_result.variables(), vec!["s"]);

        let mut bindings_iter = bindings_result.into_iter();
        assert_eq!(
            bindings_iter.next(),
            Some(Ok(vec![Some(TestTerm::iri(
                "http://example.org/person#Alice"
            ),)]))
        );
        assert_eq!(bindings_iter.next(), None);
    }

    #[test]
    fn get_bindings_result_by_coercing() {
        let my_dataset = test_dataset();
        let bindings_result = my_dataset.query("SELECT").unwrap().into_bindings();

        assert_eq!(bindings_result.variables(), vec!["s"]);

        let mut bindings_iter = bindings_result.into_iter();
        assert_eq!(
            bindings_iter.next(),
            Some(Ok(vec![Some(TestTerm::iri(
                "http://example.org/person#Alice"
            ),)]))
        );
        assert_eq!(bindings_iter.next(), None);
    }
}
