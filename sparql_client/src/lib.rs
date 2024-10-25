//! A client implementation of the [SPARQL1.1 protocol] based on [Sophia].
//! It implements [`sophia_api::sparql::SparqlDataset`].
//!
//! Example:
//! ```
//! use sophia_api::sparql::{SparqlDataset, SparqlResult};
//! use sophia_api::term::Term;
//! use sophia_sparql_client::SparqlClient;
//!
//! # fn bla() -> Result<(), Box<dyn std::error::Error>> {
//! let cli = SparqlClient::new("https://query.wikidata.org/bigdata/namespace/wdq/sparql");
//! let query = r#"
//!     #All Dr. Who performers
//!     SELECT ?doctor ?doctorLabel ?ordinal ?performer ?performerLabel
//!     WHERE {
//!       ?doctor p:P31 ?type. ?type ps:P31 wd:Q47543030 .
//!       OPTIONAL { ?type pq:P1545 ?ordinal } OPTIONAL { ?doctor wdt:P1545 ?ordinal }
//!       OPTIONAL { ?doctor p:P175 / ps:P175 ?performer }
//!       SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en" }
//!     }
//!     ORDER BY ASC(xsd:integer(?ordinal) )
//! "#;
//! if let SparqlResult::Bindings(bindings) = cli.query(query)? {
//!     for b in bindings {
//!         let b = b?;
//!         let doctor_label = b[1].as_ref().and_then(|t| t.lexical_form()).unwrap();
//!         let performer_label = b[4].as_ref().and_then(|t| t.lexical_form()).unwrap_or("NULL".into());
//!         println!("{}\t{}", doctor_label, performer_label);
//!     }
//! }
//! # Ok(()) }
//! ```
//!
//! [SPARQL1.1 protocol]: https://www.w3.org/TR/sparql11-protocol/
//! [Sophia]: https://docs.rs/sophia/
#![deny(missing_docs)]

use reqwest::{blocking::Client, Error as ReqwestError};
use sophia_api::prelude::*;
use sophia_api::sparql::{
    IntoQuery, Query as SparqlQuery, SparqlBindings, SparqlDataset, SparqlResult,
};
use sophia_api::term::SimpleTerm;
use sophia_turtle::parser::{nt, turtle};
use sophia_xml::parser as rdfxml;
use std::borrow::Borrow;
use std::io::Cursor;

mod results;
pub use results::BindingsDocument as Bindings;
use results::ResultsDocument;

type StaticTerm = SimpleTerm<'static>;

/// A [SPARQL 1.1] client implementing [`SparqlDataset`].
///
/// [SPARQL 1.1]: https://www.w3.org/TR/sparql11-protocol/
pub struct SparqlClient {
    endpoint: Box<str>,
    client: Client,
    accept: Option<String>,
}

impl SparqlClient {
    /// The default [Accept HTTP header](https://tools.ietf.org/html/rfc7231.html#section-5.3.2) used by clients.
    pub const DEFAULT_ACCEPT: &'static str = "application/sparql-results+json,application/sparql-results+xml;q=0.8,text/turtle,application/n-triples;q=0.9,application/rdf+xml;q=0.8";

    /// Create a [`SparqlClient`] on the given SPARQL-endpoint URL.
    #[must_use]
    pub fn new(endpoint: &str) -> Self {
        Self {
            endpoint: Box::from(endpoint),
            client: Client::new(),
            accept: None,
        }
    }

    /// Replace the underlying [`reqwest::Client`] of this client.
    #[must_use]
    pub fn with_client(mut self, client: Client) -> Self {
        self.client = client;
        self
    }

    /// Replace the [Accept HTTP header](https://tools.ietf.org/html/rfc7231.html#section-5.3.2) used by this client.
    ///
    /// This might be useful if the endpoint implements content-negotiation incorrectly.
    ///
    /// See also [`DEFAULT_ACCEPT`](Self::DEFAULT_ACCEPT)
    #[must_use]
    pub fn with_accept<T: Into<String>>(mut self, accept: T) -> Self {
        self.accept = Some(accept.into());
        self
    }

    /// The [Accept HTTP header](https://tools.ietf.org/html/rfc7231.html#section-5.3.2) used by this client.
    #[must_use]
    pub fn accept(&self) -> &str {
        self.accept.as_deref().unwrap_or(Self::DEFAULT_ACCEPT)
    }

    #[allow(clippy::unnecessary_wraps)]
    fn wrap_triple_source<T: TripleSource + 'static>(
        triples: T,
    ) -> Result<SparqlResult<Self>, Error>
    where
        Error: From<T::Error>,
    {
        let it: Box<dyn Iterator<Item = Result<[StaticTerm; 3], Error>>> = Box::new(
            triples
                .map_triples(|t| [t.s().into_term(), t.p().into_term(), t.o().into_term()])
                .into_iter()
                .map(|r| r.map_err(Error::from)),
        );
        Ok(SparqlResult::Triples(it))
    }
}

impl SparqlDataset for SparqlClient {
    type BindingsTerm = StaticTerm;
    type BindingsResult = Bindings;
    type TriplesResult = Box<dyn Iterator<Item = Result<[StaticTerm; 3], Error>>>;
    type SparqlError = Error;
    type Query = Query;

    fn query<Q>(&self, query: Q) -> Result<SparqlResult<Self>, Error>
    where
        Q: IntoQuery<Query>,
    {
        let query = query.into_query()?;
        let resp = self
            .client
            .post(&self.endpoint[..])
            .header("Accept", self.accept())
            .header("Content-type", "application/sparql-query")
            .header("User-Agent", "Sophia SPARQL Client")
            .body(query.borrow().0.to_string())
            .send()?;
        let ctype = resp
            .headers()
            .get("content-type")
            .map_or("application/octet-stream", |h| h.to_str().unwrap())
            .split(';')
            .next()
            .unwrap();
        match ctype {
            "application/sparql-results+json" => match resp.json::<ResultsDocument>()? {
                ResultsDocument::Boolean { boolean, .. } => Ok(SparqlResult::Boolean(boolean)),
                ResultsDocument::Bindings { doc } => Ok(SparqlResult::Bindings(doc)),
            },
            "application/sparql-results+xml" => {
                let body: Cursor<Vec<u8>> = Cursor::new(resp.bytes()?.into());
                match ResultsDocument::from_xml(body)? {
                    ResultsDocument::Boolean { boolean, .. } => Ok(SparqlResult::Boolean(boolean)),
                    ResultsDocument::Bindings { doc } => Ok(SparqlResult::Bindings(doc)),
                }
            }
            "text/turtle" => {
                let body: Cursor<Vec<u8>> = Cursor::new(resp.bytes()?.into());
                Self::wrap_triple_source(turtle::parse_bufread(body))
            }
            "application/n-triples" => {
                let body: Cursor<Vec<u8>> = Cursor::new(resp.bytes()?.into());
                Self::wrap_triple_source(nt::parse_bufread(body))
            }
            "application/rdf+xml" => {
                let body: Cursor<Vec<u8>> = Cursor::new(resp.bytes()?.into());
                Self::wrap_triple_source(rdfxml::parse_bufread(body))
            }
            _ => Err(Error::Unsupported(format!(
                "unsupported content-type: {0}",
                &ctype,
            ))),
        }
    }
}

impl SparqlBindings<SparqlClient> for Bindings {
    fn variables(&self) -> Vec<&str> {
        self.head
            .vars
            .iter()
            .map(AsRef::as_ref)
            .collect::<Vec<&str>>()
    }
}

impl Iterator for Bindings {
    type Item = Result<Vec<Option<StaticTerm>>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.results.bindings.is_empty() {
            None
        } else {
            Some(self.pop_binding())
        }
    }
}

#[derive(Debug, thiserror::Error)]
/// Error type produced by [`SparqlClient`].
pub enum Error {
    #[error("i/o error: {0}")]
    /// An [`std::io::Error`] occurred while communicating with the SPARQL endpoint.
    Io(
        #[source]
        #[from]
        std::io::Error,
    ),

    #[error("http error: {0}")]
    /// A [`ReqwestError`] occurred while communicating with the SPARQL endpoint.
    Http(#[source] Box<ReqwestError>),

    #[error("{0}")]
    /// An unsupported media-type was returned by the SPARQL endpoint.
    Unsupported(String),

    #[error("invalid iri: {0}")]
    /// An invalid IRI was returned by the SPARQL endpoint.
    Iri(
        #[source]
        #[from]
        sophia_iri::InvalidIri,
    ),

    #[error("invalid bnode identifier: {0}")]
    /// An invalid blank node identifier was returned by the SPARQL endpoint.
    BNode(
        #[source]
        #[from]
        sophia_api::term::bnode_id::InvalidBnodeId,
    ),

    #[error("invalid language tag: {0}")]
    /// An invalid language tag was returned by the SPARQL endpoint.
    LanguageTag(
        #[source]
        #[from]
        sophia_api::term::language_tag::InvalidLanguageTag,
    ),

    #[error("turtle parsing error: {0}")]
    /// Invalid Turtle syntax was returned by the SPARQL endpoint.
    RioTurtle(
        #[source]
        #[from]
        rio_turtle::TurtleError,
    ),

    #[error("RDF/XML parsing error: {0}")]
    /// Invalid RDF/XML syntax was returned by the SPARQL endpoint.
    RioXml(
        #[source]
        #[from]
        rio_xml::RdfXmlError,
    ),

    #[error("XML results parsing error: {0}")]
    /// Invalid XML syntax was returned by the SPARQL endpoint.
    Xml(
        #[source]
        #[from]
        quick_xml::Error,
    ),

    #[error("XML results structural error: {0}")]
    /// Invalid XML Sparql results were returned by the SPARQL endpoint.
    SparqlXml(String),
}

impl From<ReqwestError> for Error {
    fn from(other: ReqwestError) -> Error {
        Error::Http(Box::new(other))
    }
}

/// A SPARQL query prepared for a [`SparqlClient`].
///
/// NB: Actually, this type simply wraps the query as a `Box<str>`,
/// so [preparing](sophia_api::sparql::SparqlDataset::prepare_query)
/// it in advance has no benefit for this implementation.
pub struct Query(Box<str>);

impl SparqlQuery for Query {
    type Error = Error;

    fn parse(query_source: &str) -> Result<Self, Self::Error> {
        Ok(Query(Box::from(query_source)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sophia_api::term::{LanguageTag, TermKind};
    use sophia_isomorphism::isomorphic_graphs;
    use SparqlResult::*;

    type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;

    fn client() -> TestResult<SparqlClient> {
        let endpoint = std::env::var("SOPHIA_SPARQL_ENDPOINT")
            .map_err(|_| "Please set SOPHIA_SPARQL_ENDPOINT with an endpoint URL".to_string())?;
        Ok(SparqlClient::new(&endpoint))
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn select_simple() -> TestResult {
        match client()?.query("SELECT (42 as ?answer) {}")? {
            Bindings(b) => {
                assert_eq!(b.variables(), vec!["answer".to_string()]);
                let bindings = b.into_iter().collect::<Vec<_>>();
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].as_ref().unwrap()[0], Some(42.into_term()));
            }
            _ => panic!(),
        };
        Ok(())
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn select_complex() -> TestResult {
        match client()?.query(
            r#"
            PREFIX : <tag:>
            SELECT ?x ?y ?z {}
            VALUES (?x ?y ?z) {
                (:a "simple" 42)
                (UNDEF "lang"@en UNDEF)
                (UNDEF UNDEF UNDEF)
            }
        "#,
        )? {
            Bindings(b) => {
                assert_eq!(
                    b.variables(),
                    vec!["x".to_string(), "y".to_string(), "z".to_string()]
                );
                let bindings = b.into_iter().collect::<Vec<_>>();
                assert_eq!(bindings.len(), 3);
                assert_eq!(
                    bindings[0].as_ref().unwrap()[0],
                    Some(Iri::new_unchecked("tag:a").into_term())
                );
                assert_eq!(bindings[0].as_ref().unwrap()[1], Some("simple".into_term()));
                assert_eq!(bindings[0].as_ref().unwrap()[2], Some(42.into_term()));
                assert_eq!(bindings[1].as_ref().unwrap()[0], None);
                assert_eq!(
                    bindings[1].as_ref().unwrap()[1],
                    Some("lang" * LanguageTag::new_unchecked("en"))
                );
                assert_eq!(bindings[1].as_ref().unwrap()[2], None);
                assert_eq!(bindings[2].as_ref().unwrap()[0], None);
                assert_eq!(bindings[2].as_ref().unwrap()[1], None);
                assert_eq!(bindings[2].as_ref().unwrap()[2], None);
            }
            _ => panic!(),
        };
        Ok(())
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn select_bnode() -> TestResult {
        match client()?.query(
            r#"
            PREFIX : <tag:>
            SELECT ?x {
                BIND(BNODE() as ?x)
            }
        "#,
        )? {
            Bindings(b) => {
                assert_eq!(b.variables(), vec!["x".to_string(),]);
                let bindings = b.into_iter().collect::<Vec<_>>();
                assert_eq!(bindings.len(), 1);
                assert_eq!(
                    bindings[0].as_ref().unwrap()[0].as_ref().unwrap().kind(),
                    TermKind::BlankNode
                );
            }
            _ => panic!(),
        };
        Ok(())
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn ask_true() -> TestResult {
        match client()?.query("ASK {}")? {
            Boolean(true) => (),
            _ => panic!(),
        };
        Ok(())
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn ask_false() -> TestResult {
        match client()?.query("PREFIX : <tag:> ASK {:abcdef :ghijkl :mnopqr}")? {
            Boolean(false) => (),
            _ => panic!(),
        };
        Ok(())
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn construct_empty() -> TestResult {
        test_construct(client()?, "")
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn construct_one_triple() -> TestResult {
        test_construct(client()?, "[] a 42.")
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn construct_complex() -> TestResult {
        test_construct(client()?, COMPLEX_TTL)
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn construct_ntriples() -> TestResult {
        test_construct(client()?.with_accept("application/n-triples"), COMPLEX_TTL)
    }

    #[test]
    #[ignore = "requires a running SPARQL endpoint"]
    fn construct_rdfxml() -> TestResult {
        test_construct(client()?.with_accept("application/rdf+xml"), COMPLEX_TTL)
    }

    const COMPLEX_TTL: &str = r#"
        :s :p1 :o1, :o2;
        :p2 :o1, :o3;
        :label "S".
    "#;

    fn test_construct(client: SparqlClient, ttl: &str) -> TestResult {
        let src = format!("@prefix : <tag:>. {}", ttl);
        let exp: Vec<[StaticTerm; 3]> = turtle::parse_str(&src).collect_triples()?;
        let q = format!("PREFIX : <tag:> CONSTRUCT {{ {} }} {{}}", ttl);

        match client.query(q.as_str())? {
            Triples(triples) => {
                let got: Vec<[StaticTerm; 3]> = triples.collect_triples()?;
                assert!(isomorphic_graphs(&got, &exp)?);
            }
            _ => panic!(),
        };
        Ok(())
    }
}
