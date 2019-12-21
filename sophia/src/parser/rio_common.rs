//! Common implementations for adapting [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs) parsers.

use std::result::Result as StdResult;

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

enum MyError<E1, E2> {
    Rio(E1),
    Sophia(Error),
    Sink(E2),
}
impl<E1, E2> MyError<E1, E2>
where
    E1: std::error::Error + 'static,
    E2: std::error::Error + 'static,
{
    fn from_sophia_error(err: Error) -> Self {
        MyError::Sophia(err)
    }
    fn from_sink_error(err: E2) -> Self {
        MyError::Sink(err)
    }
    fn into_stream_error(self) -> StreamError<Error, E2>
    where
        Error: From<E1>,
    {
        match self {
            MyError::Rio(err) => SourceError(err.into()),
            MyError::Sophia(err) => SourceError(err),
            MyError::Sink(err) => SinkError(err),
        }
    }
}
impl<E1, E2> From<E1> for MyError<E1, E2>
where
    E1: std::error::Error + 'static,
    E2: std::error::Error + 'static,
{
    fn from(other: E1) -> Self {
        MyError::Rio(other)
    }
}

impl<T, E> TripleSource for RioSource<T, E>
where
    T: TriplesParser,
    T::Error: 'static,
    Error: From<T::Error>,
    Error: From<E>,
{
    type Error = Error;

    fn in_sink<TS: TripleSink>(
        &mut self,
        sink: &mut TS,
    ) -> StdResult<TS::Outcome, StreamError<Error, TS::Error>> {
        match self {
            RioSource::Error(opt) => opt
                .take()
                .map(|e| Err(SourceError(e.into())))
                .unwrap_or_else(|| {
                    let message = "This parser has already failed".to_string();
                    let location = Location::Unknown;
                    Err(SourceError(Error::from(ErrorKind::ParserError(
                        message, location,
                    ))))
                }),
            RioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |t| -> StdResult<(), MyError<T::Error, TS::Error>> {
                        sink.feed(&[
                            rio2refterm(t.subject.into()).map_err(MyError::from_sophia_error)?,
                            rio2refterm(t.predicate.into()).map_err(MyError::from_sophia_error)?,
                            rio2refterm(t.object).map_err(MyError::from_sophia_error)?,
                        ])
                        .map_err(MyError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())?;
                Ok(sink.finish().map_err(SinkError)?)
            }
        }
    }
}

impl<T, E> QuadSource for RioSource<T, E>
where
    T: QuadsParser,
    T::Error: 'static,
    Error: From<T::Error>,
    Error: From<E>,
{
    type Error = Error;

    fn in_sink<TS: QuadSink>(
        &mut self,
        sink: &mut TS,
    ) -> StdResult<TS::Outcome, StreamError<Error, TS::Error>> {
        match self {
            RioSource::Error(opt) => opt
                .take()
                .map(|e| Err(SourceError(e.into())))
                .unwrap_or_else(|| {
                    let message = "This parser has already failed".to_string();
                    let location = Location::Unknown;
                    Err(SourceError(Error::from(ErrorKind::ParserError(
                        message, location,
                    ))))
                }),
            RioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |q| -> StdResult<(), MyError<T::Error, TS::Error>> {
                        sink.feed(&(
                            [
                                rio2refterm(q.subject.into())
                                    .map_err(MyError::from_sophia_error)?,
                                rio2refterm(q.predicate.into())
                                    .map_err(MyError::from_sophia_error)?,
                                rio2refterm(q.object).map_err(MyError::from_sophia_error)?,
                            ],
                            if let Some(n) = q.graph_name {
                                Some(rio2refterm(n.into()).map_err(MyError::from_sophia_error)?)
                            } else {
                                None
                            },
                        ))
                        .map_err(MyError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())?;
                Ok(sink.finish().map_err(SinkError)?)
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
