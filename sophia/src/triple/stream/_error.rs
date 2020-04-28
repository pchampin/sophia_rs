// this module is transparently re-exported by its parent `stream`

use std::error::Error;

/// A error that is raised by functions that move fallible `Source`s into
/// fallible `Sinks`.
///
/// In case this error is raised it can be matched to investigate if `Source`
/// or `Sink` failed.
///
/// # Conversion
///
/// Both variants `SourceError` and `SinkError` are public exported.
/// Consequently, `StreamError` can be constructed with `.map_err(SourceError)`
/// and `.map_err(SinkError)`.
///
/// # ToDo
///
/// For a proper use in multi-threaded environments the trait bounds should be
/// supplied with `+ Send + Sync`. However, such bounds wound make `StreamError`
/// incompatible with `sophia`'s whole error-handling. This should be resolved
/// when the error-handling is completely refactored
/// ([tracking issue](https://github.com/pchampin/sophia_rs/issues/8)).
#[derive(Debug, thiserror::Error)]
pub enum StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
    #[error("Source failed: {0}")]
    SourceError(#[source] SourceErr),
    #[error("Sink failed: {0}")]
    SinkError(#[source] SinkErr),
}

pub use self::StreamError::*;

impl<SourceErr, SinkErr> StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
    /// Checks if `StreamError` was raised by the `Source`.
    pub fn is_source_error(&self) -> bool {
        if let SourceError(_) = self {
            true
        } else {
            false
        }
    }
    /// Checks if `StreamError` was raised by the `Sink`.
    pub fn is_sink_error(&self) -> bool {
        if let SinkError(_) = self {
            true
        } else {
            false
        }
    }
    /// Converts `StreamError` into an inner error.
    pub fn inner_into<Err>(self) -> Err
    where
        SourceErr: 'static + Error + Into<Err>,
        SinkErr: 'static + Error + Into<Err>,
    {
        match self {
            SourceError(err) => err.into(),
            SinkError(err) => err.into(),
        }
    }
    /// Unwrap as the inner SourceError.
    ///
    /// # Panic
    /// Panic if self is actually a SinkError.
    pub fn unwrap_source_error(self) -> SourceErr {
        match self {
            SourceError(err) => err,
            SinkError(_) => panic!("this is a SinkError"),
        }
    }
    /// Unwrap as the inner SinkError.
    ///
    /// # Panic
    /// Panic if self is actually a SourceError.
    pub fn unwrap_sink_error(self) -> SinkErr {
        match self {
            SourceError(_) => panic!("this is a SourceError"),
            SinkError(err) => err,
        }
    }
}

/// Convenient type alias
pub type StreamResult<T, E1, E2> = Result<T, StreamError<E1, E2>>;

/// Extension trait for `SourceError`s.
pub trait SourceResult<T, E1, E2>
where
    E1: 'static + Error,
    E2: 'static + Error,
{
    /// Map an error to a `SourceError`.
    fn source_err(self) -> StreamResult<T, E1, E2>;
}

impl<T, E1, E2> SourceResult<T, E1, E2> for Result<T, E1>
where
    E1: 'static + Error,
    E2: 'static + Error,
{
    fn source_err(self) -> StreamResult<T, E1, E2> {
        self.map_err(SourceError)
    }
}

/// Extension trait for `SinkError`s.
pub trait SinkResult<T, E1, E2>
where
    E1: 'static + Error,
    E2: 'static + Error,
{
    /// Map an error to a `SinkError`.
    fn sink_err(self) -> StreamResult<T, E1, E2>;
}

impl<T, E1, E2> SinkResult<T, E1, E2> for Result<T, E2>
where
    E1: 'static + Error,
    E2: 'static + Error,
{
    fn sink_err(self) -> StreamResult<T, E1, E2> {
        self.map_err(SinkError)
    }
}
