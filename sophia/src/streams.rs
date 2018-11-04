//! `TripleSource` and `TripleSink`,
//! are pervasive traits for streaming triples from one object to another.
//! 
//! See [`TripleSource`]'s and [`TripleSink`]'s documentation for more detail.
//! 
//! [`TripleSource`]: trait.TripleSource.html
//! [`TripleSink`]: trait.TripleSink.html
//! [`Triple`]: ../triple/trait.Triple.html

use std::error::Error as StdError;
use std::fmt;
use std::iter::Map;

use ::error::Never;
use ::graph::*;
use ::triple::*;

/// A triple source is anything that yields or produces [triples](../triple/trait.Triple.html),
/// and may also fail in the process.
/// 
/// Typical triple sources are
/// [parsers](../parsers/index.html),
/// or *fallible triple iterators*
/// (i.e. iterators yielding [results] whose `Ok` values are [triples]).
/// 
/// See also [`TripleSink`].
/// 
/// [results]: https://doc.rust-lang.org/std/result/enum.Result.html
/// [triples]: ../triple/trait.Triple.html
/// [`TripleSink`]: trait.TripleSink.html
/// 
pub trait TripleSource: Sized {
    /// The error type that this triple source may raise.
    /// 
    /// Must be [`Never`](../error/enum.Never.html) for infallible sources.
    type Error: StdError;

    /// Feed all triples from this source into the given [sink](trait.TripleSink.html).
    /// 
    /// Stop on the first error (in the source or the sink).
    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome, WhereFrom<Self::Error, TS::Error>>;

    /// Insert all triples from this source into the given [graph](../graph/trait.MutableGraph.html).
    /// 
    /// Stop on the first error (in the source or in the graph).
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
/// Useful for converting any [`Triple`] iterator into a valid [`TripleSource`].
/// 
/// [`TripleSource`]: trait.TripleSource.html
/// [`Triple`]: ../triple/trait.Triple.html
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



/// A triple sink is anything that consumes [triples](../triple/trait.Triple.html),
/// produces a result,
/// and may also fail in the process.
/// 
/// Typical triple sinks are [serializers]
/// or graphs' [inserters] and [removers].
/// 
/// See also [`TripleSource`].
/// 
/// [serializers]: ../serializers/index.html
/// [inserters]: ../graph/trait.MutableGraph.html#method.inserter
/// [removers]: ../graph/trait.MutableGraph.html#method.remover
/// [`TripleSource`]: trait.TripleSource.html
/// 
pub trait TripleSink {
    /// The error type that this triple sink may raise.
    /// 
    /// Must be [`Never`](../error/enum.Never.html) for infallible sources.
    type Error: StdError;

    /// The type of the result produced by this triple sink.
    /// 
    /// See [`finish`](#tymethod.finish).
    type Outcome;

    /// Feed one triple in this sink.
    fn feed<T: Triple>(&mut self, t: &T) -> Result<(), Self::Error>;

    /// Produce the result once all triples were fed.
    /// 
    /// NB: the behaviour of a triple sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error>;
}

/// [`()`](https://doc.rust-lang.org/std/primitive.unit.html) acts as a "black hole",
/// consuming all triples without erring, and producing no result.
/// 
/// Useful for benchmarking triple sources.
impl TripleSink for () {
    type Error = Never;
    type Outcome = ();

    fn feed<T: Triple>(&mut self, _: &T) -> Result<(), Self::Error> { Ok(()) }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> { Ok(()) }
}



/// A generic error type used to identify
/// the origin of an error in a triple stream.
#[derive(Debug, PartialEq)]
pub enum WhereFrom<U, D> {
    /// Wraps an error originating in the [triple source](trait.TripleSource.html).
    Upstream(U),
    /// Wraps an error originating in the [triple sink](trait.TripleSink.html).
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



/// A utility extension trait for unwrapping
/// [`WhereFrom`](enum.WhereFrom.html) results.
/// 
/// Useful when the source or the sink is known to be infallible.
/// 
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


/// A utility extension trait for wrapping any error as
/// [`Upstream`](enum.WhereFrom.html#variant.Upstream).
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


/// A utility extension trait for wrapping any error as
/// [`Downstream`](enum.WhereFrom.html#variant.Downstream).
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

