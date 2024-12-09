//! Common traits for working with [SPARQL](https://www.w3.org/TR/sparql11-query/).
//!
//! # Design rationale
//!
//! These traits are deliberately very generic.
//! Specific implementations may have additional features, such as:
//!
//! - setting default values for `BASE`, `PREFIX`, `FROM`, `FROM NAMED` directives,
//!   before parsing query string, or
//! - pre-binding variables before evaluating query;
//! - etc...
//!
//! However, we do not want to impose these feature, or any subset thereof,
//! to all implementations of Sophia.
//!
//! # Extension point
//!
//! A possible way to extend these traits with additional functionalities
//! (such as the ones described above)
//! would be to define subtraits of `Query` with additional methods
//! (*e.g.*`set_base`, `bind_variables`...).
//! Implementation could then express requirements as trait bound, e.g.:
//! ```text
//!     D: SparqlDataset,
//!     D::Query: Clone + BindVariable,
//! ```
//!
//! Sophia may define such traits in the future.

use sophia_iri::Iri;

use crate::source::TripleSource;
use crate::term::Term;

use std::borrow::Borrow;
use std::error::Error;

/// A dataset that can be queried with SPARQL.
pub trait SparqlDataset {
    /// The type of terms that SELECT queries will return.
    type BindingsTerm: Term;
    /// The type of bindings that SELECT queries will return.
    type BindingsResult: SparqlBindings<Self>;
    /// The type of triples that GRAPH and DESCRIBE queries will return.
    type TriplesResult: TripleSource;
    /// The type of errors that processing SPARQL queries may raise.
    type SparqlError: Error + Send + Sync + 'static;
    /// The type representing pre-processed queries.
    ///
    /// See [`prepare_query`](#tymethod.prepare_query) for more detail.
    type Query: Query<Error = Self::SparqlError>;

    /// Parse and immediately execute `query`.
    ///
    /// `query` is usually either a `&str` that will be parsed on the fly,
    /// or a `Self::Query` that was earlier prepared by the [`prepare_query`] method.
    ///
    /// [`prepare_query`]: #method.prepared
    fn query<Q>(&self, query: Q) -> Result<SparqlResult<Self>, Self::SparqlError>
    where
        Q: IntoQuery<Self::Query>;

    /// Prepare a query for multiple future executions.
    ///
    /// This allows some implementation to separate parsing,
    /// (or any other pre-processing step)
    /// of the query string from the actual execution of the query.
    /// There is however no guarantee on how much pre-processing is actually done by this method
    /// (see below).
    ///
    /// # Note to implementers
    ///
    /// If it is impossible or inconvenient to provide a type for pre-parsed queries,
    /// you can still use `String`, which implements the [`Query`] trait.
    ///
    /// See also [`SparqlDataset::prepare_query_with`]
    fn prepare_query(&self, query_string: &str) -> Result<Self::Query, Self::SparqlError> {
        Self::Query::parse(query_string)
    }

    /// Prepare a query for multiple future executions,
    /// using the given IRI to resolve relative IRIs.
    ///
    /// See also [`SparqlDataset::prepare_query`].
    fn prepare_query_with(
        &self,
        query_string: &str,
        base: Iri<&str>,
    ) -> Result<Self::Query, Self::SparqlError> {
        Self::Query::parse_with(query_string, base)
    }
}

/// Preprocessed query, ready for execution.
///
/// This trait exist to allow *some* implementations of [`SparqlDataset`]
/// to mutualize the parsing of queries in the
/// [`prepare_query`](SparqlDataset::prepare_query) method.
pub trait Query: Sized {
    /// The error type that might be raised when parsing a query.
    type Error: Error + Send + Sync + 'static;
    /// Parse the given text into a [`Query`].
    fn parse(query_source: &str) -> Result<Self, Self::Error>;
    /// Parse the given text into a [`Query`], using the given base IRI
    fn parse_with(query_source: &str, base: Iri<&str>) -> Result<Self, Self::Error>;
}

impl Query for String {
    type Error = std::convert::Infallible;
    fn parse(query_source: &str) -> Result<Self, Self::Error> {
        Ok(query_source.into())
    }
    fn parse_with(query_source: &str, base: Iri<&str>) -> Result<Self, Self::Error> {
        Ok(format!("BASE <{base}>\n{query_source}"))
    }
}

/// A utility trait to allow [`SparqlDataset::query`]
/// to accept either `&str` or [`Self::Query`](SparqlDataset::Query).
pub trait IntoQuery<Q: Query> {
    /// The output type of [`into_query`](IntoQuery::into_query).
    type Out: Borrow<Q>;
    /// Convert `self` to a [`Query`].
    fn into_query(self) -> Result<Self::Out, Q::Error>;
}

impl<'a, Q> IntoQuery<Q> for &'a Q
where
    Q: Query,
{
    type Out = &'a Q;
    fn into_query(self) -> Result<Self::Out, Q::Error> {
        Ok(self)
    }
}

impl<Q> IntoQuery<Q> for &str
where
    Q: Query,
{
    type Out = Q;
    fn into_query(self) -> Result<Self::Out, Q::Error> {
        Q::parse(self)
    }
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
