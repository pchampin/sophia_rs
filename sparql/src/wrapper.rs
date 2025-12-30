#![allow(clippy::module_name_repetitions)]

use std::borrow::Borrow;
use std::marker::PhantomData;
use std::sync::Arc;

use sophia_api::prelude::*;
use sophia_api::sparql::{IntoQuery, SparqlResult};
use spargebra::{Query as QueryAST, SparqlParser};
use thiserror::Error;

use crate::{binding::Bindings, exec::ExecState, term::ResultTerm};

#[derive(Debug)]
/// Wrap any Sophia [`Dataset`] into a [`SparqlDataset`],
/// which can therefore be queried with SPARQL.
pub struct SparqlWrapper<'a, D: Dataset + ?Sized>(pub &'a D);

impl<'a, D: Dataset + ?Sized> SparqlDataset for SparqlWrapper<'a, D> {
    type BindingsTerm = ResultTerm;

    type BindingsResult = Bindings<'a, D>;

    type TriplesResult = Box<dyn Iterator<Item = Result<[Self::BindingsTerm; 3], D::Error>>>;

    type SparqlError = SparqlWrapperError<D::Error>;

    type Query = SparqlQuery<D>;

    fn query<Q>(&self, query: Q) -> Result<SparqlResult<Self>, Self::SparqlError>
    where
        Q: IntoQuery<Self::Query>,
    {
        let query = query.into_query()?;
        log::trace!("{:#?}", query.borrow().algebra);
        #[expect(unused_variables)]
        match &(query.borrow().algebra) {
            QueryAST::Select {
                dataset,
                pattern,
                base_iri,
            } => {
                let exec = ExecState::new(self.0, dataset, base_iri)?;
                let cfg = exec.config_cloned();
                Ok(SparqlResult::Bindings(exec.select(
                    pattern,
                    &cfg.default_matcher,
                    None,
                )))
            }
            QueryAST::Construct {
                template,
                dataset,
                pattern,
                base_iri,
            } => Err(SparqlWrapperError::NotImplemented("CONSTRUCT query")),
            QueryAST::Describe {
                dataset,
                pattern,
                base_iri,
            } => Err(SparqlWrapperError::NotImplemented("DESCRIBE query")),
            QueryAST::Ask {
                dataset,
                pattern,
                base_iri,
            } => {
                let exec = ExecState::new(self.0, dataset, base_iri)?;
                let cfg = exec.config_cloned();
                exec.ask(pattern, &cfg.default_matcher)
                    .map(SparqlResult::Boolean)
            }
        }
    }
}

//

#[derive(Error)]
pub enum SparqlWrapperError<E> {
    #[error("Query parse error: {0}")]
    Parse(#[from] spargebra::SparqlSyntaxError),
    #[error("Override variable: {0}")]
    Override(Arc<str>),
    #[error("Dataset error: {0}")]
    Dataset(E),
    #[error("Not implemented: {0}")]
    NotImplemented(&'static str),
}

impl<E: std::error::Error> std::fmt::Debug for SparqlWrapperError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

//

#[derive(Clone, Debug)]
pub struct SparqlQuery<D: ?Sized> {
    algebra: spargebra::Query,
    _phantom: PhantomData<D>,
}

impl<D: Dataset + ?Sized> From<spargebra::Query> for SparqlQuery<D> {
    fn from(algebra: spargebra::Query) -> Self {
        SparqlQuery {
            algebra,
            _phantom: PhantomData,
        }
    }
}

impl<D: Dataset + ?Sized> sophia_api::sparql::Query for SparqlQuery<D> {
    type Error = SparqlWrapperError<D::Error>;

    fn parse(query_source: &str) -> Result<Self, Self::Error> {
        let p = SparqlParser::new();
        Ok(SparqlQuery::from(p.parse_query(query_source)?))
    }

    fn parse_with(query_source: &str, base: Iri<&str>) -> Result<Self, Self::Error> {
        let p = SparqlParser::new().with_base_iri(base.as_str()).unwrap();
        Ok(SparqlQuery::from(p.parse_query(query_source)?))
    }
}
