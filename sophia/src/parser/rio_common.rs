//! Common implementations for adapting [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs) parsers.

use std::result::Result as StdResult;

use rio_api::model::*;
use rio_api::parser::*;

use crate::error::*;
use crate::ns::xsd;
use crate::quad::stream::*;
use crate::term::{BoxTerm, RefTerm};
use crate::triple::stream::*;

/// TripleSource / QuadSource adapter for RIO TripleParser / QuadParser
pub enum StrictRioSource<T, E> {
    Parser(T),
    Error(Option<E>),
}

impl<T, E> From<StdResult<T, E>> for StrictRioSource<T, E> {
    fn from(res: StdResult<T, E>) -> Self {
        match res {
            Ok(parser) => StrictRioSource::Parser(parser),
            Err(error) => StrictRioSource::Error(Some(error)),
        }
    }
}

// This intermediate type is required,
// because Rio requires that the error type of triple_handler/quad_handler
// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
//
// This is costless, though,
// because MyStreamError's internal representation is identical to StreamError,
// so the final type conversion performed by into_stream_error is actually
// just for pleasing the compiler.
enum MyStreamError<E> {
    Source(Error),
    Sink(E),
}
impl<E> MyStreamError<E>
where
    E: std::error::Error + 'static,
{
    fn from_sink_error(err: E) -> Self {
        MyStreamError::Sink(err)
    }
    fn into_stream_error(self) -> StreamError<Error, E> {
        match self {
            MyStreamError::Source(err) => SourceError(err),
            MyStreamError::Sink(err) => SinkError(err),
        }
    }
}
impl<E1, E2> From<E1> for MyStreamError<E2>
where
    Error: From<E1>,
    E2: std::error::Error + 'static,
{
    fn from(other: E1) -> Self {
        MyStreamError::Source(other.into())
    }
}

impl<T, E> TripleSource for StrictRioSource<T, E>
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
            StrictRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            StrictRioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |t| -> StdResult<(), MyStreamError<TS::Error>> {
                        sink.feed(&[
                            rio2refterm(t.subject.into()),
                            rio2refterm(t.predicate.into()),
                            rio2refterm(t.object.into()),
                        ])
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())?;
                Ok(sink.finish().map_err(SinkError)?)
            }
        }
    }
}

impl<T, E> QuadSource for StrictRioSource<T, E>
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
            StrictRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            StrictRioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |q| -> StdResult<(), MyStreamError<TS::Error>> {
                        sink.feed(&(
                            [
                                rio2refterm(q.subject.into()),
                                rio2refterm(q.predicate.into()),
                                rio2refterm(q.object.into()),
                            ],
                            if let Some(n) = q.graph_name {
                                Some(rio2refterm(n.into()))
                            } else {
                                None
                            },
                        ))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())?;
                Ok(sink.finish().map_err(SinkError)?)
            }
        }
    }
}

/// QuadSource adapter for RIO GeneralizedQuadParser
pub enum GeneralizedRioSource<T, E> {
    Parser(T),
    Error(Option<E>),
}

impl<T, E> From<StdResult<T, E>> for GeneralizedRioSource<T, E> {
    fn from(res: StdResult<T, E>) -> Self {
        match res {
            Ok(parser) => GeneralizedRioSource::Parser(parser),
            Err(error) => GeneralizedRioSource::Error(Some(error)),
        }
    }
}

impl<T, E> QuadSource for GeneralizedRioSource<T, E>
where
    T: GeneralizedQuadsParser,
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
            GeneralizedRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            GeneralizedRioSource::Parser(parser) => {
                parser
                    .parse_all(&mut |q| -> StdResult<(), MyStreamError<TS::Error>> {
                        sink.feed(&(
                            [
                                rio2refterm(q.subject),
                                rio2refterm(q.predicate),
                                rio2refterm(q.object),
                            ],
                            if let Some(n) = q.graph_name {
                                Some(rio2refterm(n))
                            } else {
                                None
                            },
                        ))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())?;
                Ok(sink.finish().map_err(SinkError)?)
            }
        }
    }
}

/// Consume inner error and convert it to Error
fn consume_err<E: Into<Error>>(opt: &mut Option<E>) -> Error {
    opt.take().map(|e| e.into()).unwrap_or_else(|| {
        let message = "This parser has already failed".to_string();
        let location = Location::Unknown;
        Error::from(ErrorKind::ParserError(message, location))
    })
}

/// Convert RIO term to Sophia term
pub fn rio2refterm(t: GeneralizedTerm) -> RefTerm {
    use Literal::*;
    unsafe {
        match t {
            GeneralizedTerm::BlankNode(b) => RefTerm::new_bnode(b.id).unwrap(),
            GeneralizedTerm::NamedNode(n) => RefTerm::new_iri_unchecked(n.iri, None),
            GeneralizedTerm::Literal(Simple { value }) => {
                RefTerm::new_literal_dt_unchecked(value, xsd::string)
            }
            GeneralizedTerm::Literal(LanguageTaggedString { value, language }) => {
                RefTerm::new_literal_lang_unchecked(value, language)
            }
            GeneralizedTerm::Literal(Typed { value, datatype }) => {
                RefTerm::new_literal_dt_unchecked(
                    value,
                    RefTerm::new_iri_unchecked(datatype.iri, None),
                )
            }
            GeneralizedTerm::Variable(v) => RefTerm::new_variable_unchecked(v.name),
        }
    }
}

/// Convert RIO term to Sophia term
pub fn rio2boxterm(t: GeneralizedTerm) -> BoxTerm {
    BoxTerm::from_with(&rio2refterm(t), Box::from)
}
