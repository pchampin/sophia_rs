//! I define two traits, TripleSource and TripleSink,
//! which are pervasive for streaming triples from one object to another.

use std::error::Error as StdError;
use std::fmt;
use std::iter::Map;

use ::error::Never;
use ::graph::*;
use ::triple::*;

pub trait TripleSource: Sized {
    type Error: StdError;

    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome, WhereFrom<Self::Error, TS::Error>>;
    fn into_graph<G: MutableGraph>(self, graph: &mut G) -> Result<usize, WhereFrom<Self::Error, G::Error>> {
        self.into_sink(&mut graph.inserter())
    }
}

impl<I, T, E> TripleSource for I
where
    I: Iterator<Item=Result<T, E>>,
    T: Triple,
    E: StdError,
{
    type Error = E;

    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome, WhereFrom<Self::Error, TS::Error>> {
        for tr in self {
            let t = tr.as_upstream()?;
            sink.feed(&t).as_downstream()?;
        }
        return sink.finish().as_downstream()
    }
}


/// A utility extension trait for converting standard iterators
/// into result iterators.
/// Useful for converting Triple iterators into a valid TripleSource.
pub trait WrapAsOks<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn wrap_as_oks(self) -> Map<Self, fn(T) -> Result<T, Never>>;
}

impl<T, I> WrapAsOks<T> for I
    where I: Iterator<Item=T> + Sized,
{
    fn wrap_as_oks(self) -> Map<Self, fn(T) -> Result<T, Never>> {
        self.map(Result::Ok)
    }
}



pub trait TripleSink {
    type Error: StdError;
    type Outcome;

    fn feed<T: Triple>(&mut self, t: &T) -> Result<(), Self::Error>;
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;
}

impl TripleSink for () {
    type Error = Never;
    type Outcome = ();

    fn feed<T: Triple>(&mut self, _: &T) -> Result<(), Self::Error> { Ok(()) }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> { Ok(()) }
}



#[derive(Debug, PartialEq)]
pub enum WhereFrom<U, D> {
    Upstream(U),
    Downstream(D),
}
pub use self::WhereFrom::*;

impl<U, D> fmt::Display for WhereFrom<U, D>
    where U: StdError, D: StdError,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Upstream(err) => write!(f, "Upstream:\n{}", err),
            Downstream(err) => write!(f, "Downstream:\n{}", err),
        }
    }
}

impl<U, D> StdError for WhereFrom<U, D>
    where U: StdError, D: StdError,
{}



pub trait UnwrapWhereFrom<T, U, D> {
    fn unwrap_upstream(self) -> Result<T, D>;
    fn unwrap_downstream(self) -> Result<T, U>;
}

impl<T, U, D> UnwrapWhereFrom<T, U, D> for Result<T, WhereFrom<U, D>>
    where U: StdError, D: StdError,
{
    fn unwrap_upstream(self) -> Result<T, D> {
        match self {
            Ok(ok) => Ok(ok),
            Err(Upstream(err)) => panic!("{:?}", err),
            Err(Downstream(err)) => Err(err),
        }
    }
    fn unwrap_downstream(self) -> Result<T, U> {
        match self {
            Ok(ok) => Ok(ok),
            Err(Upstream(err)) => Err(err),
            Err(Downstream(err)) => panic!("{:?}", err),
        }
    }
}


pub trait AsUpstream<T, E> {
    fn as_upstream<F>(self) -> Result<T, WhereFrom<E, F>>;
}

impl<T, E> AsUpstream<T, E> for Result<T, E> {
    fn as_upstream<F>(self) -> Result<T, WhereFrom<E, F>> {
        match self {
            Ok(ok) => Ok(ok),
            Err(err) => Err(Upstream(err)),
        }
    }
}


pub trait AsDownstream<T, E> {
    fn as_downstream<F>(self) -> Result<T, WhereFrom<F, E>>;
}

impl<T, E> AsDownstream<T, E> for Result<T, E> {
    fn as_downstream<F>(self) -> Result<T, WhereFrom<F, E>> {
        match self {
            Ok(ok) => Ok(ok),
            Err(err) => Err(Downstream(err)),
        }
    }
}

