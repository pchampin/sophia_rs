use std::convert::Infallible;
use std::error::Error;
use std::fmt;

/// A error that is raised by functions that move fallible `Source`s into
/// fallible `Sinks`.
///
/// In case this error is raised it can be matched to investigate if `Source`
/// or `Sink` failed.
///
/// The `std::error::Error::source()` method is implemented accordingly.
///
/// # ToDo
///
/// For a proper use in multi-threaded environments the trait bounds should be
/// supplied with `+ Send + Sync`. However, such bounds wound make `StreamError`
/// incompatible with `sophia`'s whole error-handling. This should be resolved
/// when the error-handling is completly refactored
/// ([tracking issue](https://github.com/pchampin/sophia_rs/issues/8)).
#[derive(Debug)]
pub enum StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
    Source(SourceErr),
    Sink(SinkErr),
}

use self::StreamError::*;

impl<SourceErr, SinkErr> StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
	/// Build a `StreamError` from an error of the `Source`.
    pub fn from_source_err(se: SourceErr) -> Self {
        Source(se)
	}
	/// Build a `StreamError` from an error of the `Sink`.
    pub fn from_sink_err(se: SinkErr) -> Self {
        Sink(se)
    }
    /// Checks if `StreamError` was raised by the `Source`.
    pub fn is_source_err(&self) -> bool {
        if let Source(_) = self {
            true
        } else {
            false
        }
    }
    /// Checks if `StreamError` was raised by the `Sink`.
    pub fn is_sink_err(&self) -> bool {
        if let Sink(_) = self {
            true
        } else {
            false
        }
    }
}

impl<SourceErr, SinkErr> StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error + Into<SourceErr>,
{
	/// Converts the ``StreamError` into its `SourceError`.
	/// 
	/// _Note_ this also works if `SinkError` is `Infallible`.
    pub fn into_source_err(self) -> SourceErr {
        match self {
            Source(err) => err,
            Sink(err) => err.into(),
        }
    }
}

impl<SourceErr, SinkErr> StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error + Into<SinkErr>,
    SinkErr: 'static + Error,
{
	/// Converts the ``StreamError` into its `SinkError`.
	/// 
	/// _Note_ this also works if `SourceError` is `Infallible`.
    pub fn into_sink_err(self) -> SinkErr {
        match self {
            Source(err) => err.into(),
            Sink(err) => err,
        }
    }
}

impl<SinkErr> StreamError<Infallible, SinkErr>
where
    SinkErr: 'static + Error,
{
    /// Converts a `StreamError` from an infallible `Source`.
    pub fn with_fallible_source<SourceErr>(self) -> StreamError<SourceErr, SinkErr>
    where
        SourceErr: 'static + Error,
    {
        match self {
            Source(_) => unreachable!(),
            Sink(err) => Sink(err),
        }
    }
}

impl<SourceErr> StreamError<SourceErr, Infallible>
where
    SourceErr: 'static + Error,
{
    /// Converts a `StreamError` from an infallible `Sink`.
    pub fn with_fallible_sink<SinkErr>(self) -> StreamError<SourceErr, SinkErr>
    where
        SinkErr: 'static + Error,
    {
        match self {
            Source(err) => Source(err),
            Sink(_) => unreachable!(),
        }
    }
}

impl<Err> StreamError<Err, Err>
where
    Err: 'static + Error,
{
    /// Converts the `StreamError` into its underlying error if `Source` and
    /// `Sink` errors are of the same type.
    pub fn into_inner(self) -> Err {
        match self {
            Source(err) | Sink(err) => err,
        }
    }
}

impl<SourceErr, SinkErr> fmt::Display for StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::StreamError::*;

        match self {
            Source(err) => write!(f, "Source failed: {}", err),
            Sink(err) => write!(f, "Sink failed: {}", err),
        }
    }
}

impl<SourceErr, SinkErr> Error for StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Source(err) => Some(err),
            Sink(err) => Some(err),
        }
    }
}

/// Extension trait for results to convienently convert to `StreamError`.
///
/// The counter part is [`SinkResultExt`](trait.SinkResultExt.html)
pub trait SourceResultExt<T, SourceErr>
where
    SourceErr: 'static + Error,
{
    fn map_source_err<SinkErr>(self) -> Result<T, StreamError<SourceErr, SinkErr>>
    where
        SinkErr: 'static + Error;
}

impl<T, SourceErr> SourceResultExt<T, SourceErr> for Result<T, SourceErr>
where
    SourceErr: 'static + Error,
{
    fn map_source_err<SinkErr>(self) -> Result<T, StreamError<SourceErr, SinkErr>>
    where
        SinkErr: 'static + Error,
    {
        self.map_err(Source)
    }
}

/// Extension trait for results to convienently convert to `StreamError`.
///
/// The counter part is [`SourceResultExt`](trait.SourceResultExt.html)
pub trait SinkResultExt<T, SinkErr>
where
    SinkErr: 'static + Error,
{
    fn map_sink_err<SourceErr>(self) -> Result<T, StreamError<SourceErr, SinkErr>>
    where
        SourceErr: 'static + Error;
}

impl<T, SinkErr> SinkResultExt<T, SinkErr> for Result<T, SinkErr>
where
    SinkErr: 'static + Error,
{
    fn map_sink_err<SourceErr>(self) -> Result<T, StreamError<SourceErr, SinkErr>>
    where
        SourceErr: 'static + Error,
    {
        self.map_err(Sink)
    }
}
