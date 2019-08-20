//! Common implementations for adapting [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs) parsers.

use std::result::Result as StdResult;

use pest::error::{InputLocation, LineColLocation};
use rio_api::model::*;
use rio_api::parser::*;

use crate::error::*;
use crate::ns::xsd;
use crate::quad::stream::*;
use crate::term::{BoxTerm, RefTerm};
use crate::triple::stream::*;

/// TripleSource / QuadSource adapter for RIO TripleParser
pub enum RioSource<T, E> {
    Parser(T),
    Error(Option<E>),
}

impl<T, E> From<StdResult<T, E>> for RioSource<T, E> {
    fn from(res: StdResult<T, E>) -> Self {
        match res {
            Ok(parser) => RioSource::Parser(parser),
            Err(error) => RioSource::Error(Some(error)),
        }
    }
}

impl<T, E> TripleSource for RioSource<T, E>
where
    T: TripleParser,
    Error: From<T::Error>,
    Error: From<E>,
{
    type Error = Error;

    fn in_sink<TS: TripleSink>(
        &mut self,
        sink: &mut TS,
    ) -> CoercedResult<TS::Outcome, Error, TS::Error>
    where
        Error: CoercibleWith<TS::Error>,
    {
        match self {
            RioSource::Error(opt) => opt
                .take()
                .map(|e| Err(Error::from(e).into()))
                .unwrap_or_else(|| {
                    let message = "This parser has already failed".to_string();
                    let location = InputLocation::Pos(0);
                    let line_col = LineColLocation::Pos((0, 0));
                    Err(Error::from(ErrorKind::ParserError(message, location, line_col)).into())
                }),
            RioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |t| {
                        sink.feed(&[
                            rio2refterm(t.subject.into()).unwrap(),   // TODO
                            rio2refterm(t.predicate.into()).unwrap(), // TODO
                            rio2refterm(t.object).unwrap(),           // TODO
                        ])
                        .unwrap(); // TODO
                    })
                    .map_err(Error::from)?;
                Ok(sink.finish()?)
            }
        }
    }
}

impl<T, E> QuadSource for RioSource<T, E>
where
    T: QuadParser,
    Error: From<T::Error>,
    Error: From<E>,
{
    type Error = Error;

    fn in_sink<TS: QuadSink>(
        &mut self,
        sink: &mut TS,
    ) -> CoercedResult<TS::Outcome, Error, TS::Error>
    where
        Error: CoercibleWith<TS::Error>,
    {
        match self {
            RioSource::Error(opt) => opt
                .take()
                .map(|e| Err(Error::from(e).into()))
                .unwrap_or_else(|| {
                    let message = "This parser has already failed".to_string();
                    let location = InputLocation::Pos(0);
                    let line_col = LineColLocation::Pos((0, 0));
                    Err(Error::from(ErrorKind::ParserError(message, location, line_col)).into())
                }),
            RioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |q| {
                        sink.feed(&(
                            [
                                rio2refterm(q.subject.into()).unwrap(),   // TODO
                                rio2refterm(q.predicate.into()).unwrap(), // TODO
                                rio2refterm(q.object).unwrap(),           // TODO
                            ],
                            if let Some(n) = q.graph_name {
                                Some(rio2refterm(n.into()).unwrap()) // TODO
                            } else {
                                None
                            },
                        ))
                        .unwrap(); // TODO
                    })
                    .map_err(Error::from)?;
                Ok(sink.finish()?)
            }
        }
    }
}

/// Convert RIO term to Sophia term
pub fn rio2refterm(t: Term) -> Result<RefTerm> {
    use Literal::*;
    match t {
        Term::BlankNode(b) => RefTerm::new_bnode(b.id),
        Term::NamedNode(n) => RefTerm::new_iri(n.iri),
        Term::Literal(Simple { value }) => RefTerm::new_literal_dt(value, xsd::string),
        Term::Literal(LanguageTaggedString { value, language }) => {
            RefTerm::new_literal_lang(value, language)
        }
        Term::Literal(Typed { value, datatype }) => {
            RefTerm::new_literal_dt(value, RefTerm::new_iri(datatype.iri)?)
        }
    }
}

/// Convert RIO term to Sophia term
pub fn rio2boxterm(t: Term) -> Result<BoxTerm> {
    Ok(BoxTerm::from_with(&rio2refterm(t)?, Box::from))
}
