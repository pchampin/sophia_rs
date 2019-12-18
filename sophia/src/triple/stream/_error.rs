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
/// when the error-handling is completly refactored
/// ([tracking issue](https://github.com/pchampin/sophia_rs/issues/8)).
#[derive(Debug, thiserror::Error)]
pub enum StreamError<SourceErr, SinkErr>
where
    SourceErr: 'static + Error,
    SinkErr: 'static + Error,
{
    #[error("Source failed: {0}")]
    SourceError( #[source] SourceErr),
    #[error("Sink failed: {0}")]
    SinkError( #[source] SinkErr),
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

}


