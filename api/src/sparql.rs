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
    type Result: SparqlResult;
    type SparqlError: Error + 'static;

    /// Parse and immediately execute `query`
    fn query(&self, query: &str) -> Result<Self::Result, Self::SparqlError>;
}

/// The result of executing a SPARQL query.
pub trait SparqlResult {
    type Term: TTerm;
    /// The type of iterator returned if the query was a SELECT.
    type Bindings: Iterator<Item = Vec<Option<Self::Term>>>;
    /// The type of graph returned if the query was a CONSTRUCT or DESCRIBE.
    type Graph: Graph;

    /// The kind of this result (depends on the type of query)
    fn kind(&self) -> SparqlResultKind;
    /// If the query was a SELECT, the list of selected variable names
    fn variables(&self) -> Option<Vec<String>>;
    /// If the query was a SELECT, an iterator over all the solutions
    fn bindings(self) -> Option<Self::Bindings>;
    /// If the query was an ASK, the boolean result
    fn boolean(&self) -> Option<bool>;
    /// If the query was a CONSTRUCT or DESCRIBE, the resulting graph
    fn graph(&self) -> Option<Self::Graph>;
}

/// Different kinds of SPARQL results
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SparqlResultKind {
    /// Result of a SELECT query
    Bindings,
    /// Result of an ASK query
    Boolean,
    /// Result of a CONSTRUCT or DESCRIBE query
    Graph,
}
