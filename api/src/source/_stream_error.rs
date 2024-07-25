use crate::Error;

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
#[derive(Debug, thiserror::Error)]
pub enum StreamError<SourceErr, SinkErr>
where
    SourceErr: Error,
    SinkErr: Error,
{
    /// Error caused by the source
    #[error("Source failed: {0}")]
    SourceError(#[source] SourceErr),
    /// Error caused by the sink
    #[error("Sink failed: {0}")]
    SinkError(#[source] SinkErr),
}
pub use StreamError::*;

impl<SourceErr, SinkErr> StreamError<SourceErr, SinkErr>
where
    SourceErr: Error,
    SinkErr: Error,
{
    /// Checks if `StreamError` was raised by the `Source`.
    pub fn is_source_error(&self) -> bool {
        matches!(self, SourceError(_))
    }
    /// Checks if `StreamError` was raised by the `Sink`.
    pub fn is_sink_error(&self) -> bool {
        matches!(self, SinkError(_))
    }
    /// Converts `StreamError` into an inner error.
    pub fn inner_into<Err>(self) -> Err
    where
        SourceErr: Error + Into<Err>,
        SinkErr: Error + Into<Err>,
    {
        match self {
            SourceError(err) => err.into(),
            SinkError(err) => err.into(),
        }
    }
    /// Convert using `f` if a `SourceError`
    pub fn map_source<E, F>(self, f: F) -> StreamError<E, SinkErr>
    where
        E: Error,
        F: FnOnce(SourceErr) -> E,
    {
        match self {
            SourceError(e) => SourceError(f(e)),
            SinkError(e) => SinkError(e),
        }
    }
    /// Convert using `f` if a `SinkError`
    pub fn map_sink<E, F>(self, f: F) -> StreamError<SourceErr, E>
    where
        E: Error,
        F: FnOnce(SinkErr) -> E,
    {
        match self {
            SourceError(e) => SourceError(e),
            SinkError(e) => SinkError(f(e)),
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
    /// Switch source and sink error.
    pub fn reverse(self) -> StreamError<SinkErr, SourceErr> {
        match self {
            SourceError(e) => SinkError(e),
            SinkError(e) => SourceError(e),
        }
    }
}

/// Convenient type alias for [`Result`] whose error is [`StreamError`].
pub type StreamResult<T, E1, E2> = Result<T, StreamError<E1, E2>>;

/// Additional methods for [`StreamResult`]
pub trait StreamResultExt<T, E1, E2>
where
    E1: Error,
    E2: Error,
{
    /// Map the error if it is a [`SourceError`]
    fn map_source_err<E, F>(self, f: F) -> StreamResult<T, E, E2>
    where
        E: Error,
        F: FnOnce(E1) -> E;
    /// Map the error if it is a [`SinkError`]
    fn map_sink_err<E, F>(self, f: F) -> StreamResult<T, E1, E>
    where
        E: Error,
        F: FnOnce(E2) -> E;
}

impl<T, E1, E2> StreamResultExt<T, E1, E2> for StreamResult<T, E1, E2>
where
    E1: Error,
    E2: Error,
{
    fn map_source_err<E, F>(self, f: F) -> StreamResult<T, E, E2>
    where
        E: Error,
        F: FnOnce(E1) -> E,
    {
        self.map_err(|err| err.map_source(f))
    }

    fn map_sink_err<E, F>(self, f: F) -> StreamResult<T, E1, E>
    where
        E: Error,
        F: FnOnce(E2) -> E,
    {
        self.map_err(|err| err.map_sink(f))
    }
}
