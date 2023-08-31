use sophia_api::{
    quad::Spog,
    source::{
        QuadSource,
        StreamError::{SinkError, SourceError},
        StreamResult,
    },
};

use crate::JsonLdError;

use super::adapter::RdfTerm;

/// The type of [`QuadSource`] returned by [`JsonLdParser`].
pub enum JsonLdQuadSource {
    /// Some quads were parsed
    Quads(std::vec::IntoIter<Spog<RdfTerm>>),
    /// An error was raised while parsing
    Err(Option<JsonLdError>),
}

impl JsonLdQuadSource {
    pub(crate) fn from_err<E: Into<JsonLdError>>(err: E) -> Self {
        JsonLdQuadSource::Err(Some(err.into()))
    }
}

impl QuadSource for JsonLdQuadSource {
    type Quad<'x> = Spog<RdfTerm>;

    type Error = JsonLdError;

    fn try_for_some_quad<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), E>,
    {
        match self {
            JsonLdQuadSource::Quads(quads) => {
                if let Some(quad) = quads.next() {
                    f(quad).map(|_| true).map_err(SinkError)
                } else {
                    Ok(false)
                }
            }
            JsonLdQuadSource::Err(opt) => {
                if let Some(err) = opt.take() {
                    Err(SourceError(err))
                } else {
                    Ok(false)
                }
            }
        }
    }
}
