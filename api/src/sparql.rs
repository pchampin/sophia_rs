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

use crate::dataset::Dataset;
use crate::graph::Graph;
use crate::term::TTerm;
use std::error::Error;

/// A dataset that can be queried with SPARQL.
pub trait SparqlDataset: Dataset {
    type BindingsTerm: TTerm;
    type BindingsResult: SparqlBindings<Self::BindingsTerm>;
    type GraphResult: Graph;
    type SparqlError: Error + 'static;

    /// Parse and immediately execute `query`
    fn query(&self, query: &str) -> Result<SparqlResult<Self>, Self::SparqlError>;
}

/// The result of executing a SPARQL query.
pub enum SparqlResult<T>
where
    T: SparqlDataset + ?Sized,
{
    Bindings(T::BindingsResult),
    Boolean(bool),
    Graph(T::GraphResult),
}

/// The result of executing a SPARQL SELECT query
pub trait SparqlBindings<T: TTerm>: IntoIterator<Item = Vec<Option<T>>> {
    /// Borrow the list of SELECTed variable names
    fn variables(&self) -> &[&str];

    /// Return an owned copy of the list of SELECTed variable names
    fn variables_owned(&self) -> Vec<String> {
        self.variables()
            .iter()
            .cloned()
            .map(str::to_string)
            .collect()
    }
}

/// A dummy module to check that implementing these traits is actually possible
#[cfg(test)]
mod dummy {
    use super::*;
    use crate::quad::Quad;
    use std::convert::Infallible;

    pub type MyTerm = crate::term::test::TestTerm<String>;
    pub type MyQuad = ([MyTerm; 3], Option<MyTerm>);
    pub type MyDataset = Vec<MyQuad>;

    pub struct MyBindings(Box<dyn Iterator<Item = Vec<Option<MyTerm>>>>);

    impl IntoIterator for MyBindings {
        type Item = Vec<Option<MyTerm>>;
        type IntoIter = Box<dyn Iterator<Item = Vec<Option<MyTerm>>>>;
        fn into_iter(self) -> Self::IntoIter {
            self.0
        }
    }
    impl SparqlBindings<MyTerm> for MyBindings {
        fn variables(&self) -> &[&str] {
            &["s"]
        }
    }

    impl SparqlDataset for MyDataset {
        type BindingsTerm = MyTerm;
        type BindingsResult = MyBindings;
        type GraphResult = Vec<[MyTerm; 3]>;
        type SparqlError = Infallible;

        fn query(&self, query: &str) -> Result<SparqlResult<Self>, Self::SparqlError> {
            match query {
                "ASK" => Ok(SparqlResult::Boolean(true)),
                "GRAPH" => Ok(SparqlResult::Graph(
                    self.iter()
                        .map(|q| [q.s().clone(), q.p().clone(), q.o().clone()])
                        .collect(),
                )),
                _ => Ok(SparqlResult::Bindings(MyBindings(Box::new(
                    self.subjects().unwrap().into_iter().map(|t| vec![Some(t)]),
                )))),
            }
        }
    }
}
