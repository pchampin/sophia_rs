//! A JSON-LD serializer implementing the
//! [`Serialize RDF as JSON-LD Algorithm`].
//!
//! [`Serialize RDF as JSON-LD Algorithm`]: https://www.w3.org/TR/json-ld11-api/#serialize-rdf-as-json-ld-algorithm

use crate::config::*;
use crate::error::*;
use json::JsonValue;
use sophia::quad::stream::*;
use sophia::triple::stream::{SinkError, StreamResult};
use sophia_api::serializer::*;

mod engine;
mod rdf_object;
#[cfg(test)]
mod test;

/// A JSON-LD serializer.
pub struct JsonLdSerializer<W> {
    config: JsonLdConfig,
    target: W,
}

impl<W> JsonLdSerializer<W> {
    /// Build a new JSON-LD serializer with the default config.
    #[inline]
    pub fn new(target: W) -> JsonLdSerializer<W> {
        Self::new_with_config(target, JsonLdConfig::default())
    }

    /// Build a new JSON-LD serializer writing to `write`, with the given config.
    pub fn new_with_config(target: W, config: JsonLdConfig) -> JsonLdSerializer<W> {
        JsonLdSerializer { config, target }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &JsonLdConfig {
        &self.config
    }

    /// Convert a quad stream into a Json object
    fn convert_quads<QS>(&mut self, source: QS) -> StreamResult<JsonValue, QS::Error, JsonLdError>
    where
        QS: QuadSource,
    {
        let mut engine = engine::Engine::new_with_config(self.config.clone());
        engine.process_quads(source)?;
        engine.into_json().map_err(SinkError)
    }
}

impl<W> QuadSerializer for JsonLdSerializer<W>
where
    W: std::io::Write,
{
    type Error = JsonLdError;

    fn serialize_quads<QS>(&mut self, source: QS) -> StreamResult<&mut Self, QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        let result = self.convert_quads(source)?;
        let json_txt = match self.config.spaces {
            0 => json::stringify(result),
            x => json::stringify_pretty(result, x),
        };
        self.target
            .write(json_txt.as_bytes())
            .map_err(|e| SinkError(e.into()))?;
        Ok(self)
    }
}

/// A utility type alias of [`JsonLdSerializer`] with `[JsonTarget]` as its target.
///
/// [`JsonLdSerializer`]: struct.JsonLdSerializer.html
/// [`JsonTarget`]: struct.JsonTarget.html
pub type Jsonifier = JsonLdSerializer<JsonTarget>;

/// This type is just a placeholder [`JsonLdSerializer`]
/// targetting a `JsonValue`.
/// See [`new_jsonifier`] and [`new_jsonifier_with_config`].
///
/// [`JsonLdSerializer`]: struct.JsonLdSerializer.html
/// [`new_jsonifier`]: struct.JsonLdSerializer.html#method.new_jsonifier
/// [`new_jsonifier_with_config`]: struct.JsonLdSerializer.html#method.new_jsonifier_with_config
#[derive(Clone, Debug)]
pub struct JsonTarget(JsonValue);

impl Jsonifier {
    /// Create a new serializer which targets a `JsonValue`.
    #[inline]
    pub fn new_jsonifier() -> Self {
        JsonLdSerializer::new(JsonTarget(JsonValue::Null))
    }
    /// Create a new serializer which targets a `JsonValue` with a custom config.
    #[inline]
    pub fn new_jsonifier_with_config(config: JsonLdConfig) -> Self {
        JsonLdSerializer::new_with_config(JsonTarget(JsonValue::Null), config)
    }

    /// Get a reference to the converted JsonValue
    #[inline]
    pub fn as_json(&self) -> &JsonValue {
        &self.target.0
    }
}

impl QuadSerializer for Jsonifier {
    type Error = JsonLdError;

    fn serialize_quads<QS>(&mut self, source: QS) -> StreamResult<&mut Self, QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        self.target.0 = self.convert_quads(source)?;
        Ok(self)
    }
}

/// A utility type alias representing a [`JsonLdSerializer`] which targets a string.
///
/// [`JsonLdSerializer`]: struct.JsonLdSerializer.html
pub type JsonLdStringifier = JsonLdSerializer<Vec<u8>>;

impl JsonLdStringifier {
    /// Create a new serializer which targets a string.
    #[inline]
    pub fn new_stringifier() -> Self {
        JsonLdSerializer::new(Vec::new())
    }
    /// Create a new serializer which targets a string with a custom config.
    #[inline]
    pub fn new_stringifier_with_config(config: JsonLdConfig) -> Self {
        JsonLdSerializer::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for JsonLdStringifier {
    fn as_utf8(&self) -> &[u8] {
        &self.target[..]
    }
}
