//! A JSON-LD serializer implementing the
//! [`Serialize RDF as JSON-LD Algorithm`].
//!
//! [`Serialize RDF as JSON-LD Algorithm`]: https://www.w3.org/TR/json-ld11-api/#serialize-rdf-as-json-ld-algorithm

use crate::error::JsonLdError;
use crate::loader::NoLoader;
use crate::options::JsonLdOptions;
use json_syntax::print::Indent;
use json_syntax::print::{Options, Print};
use json_syntax::Value as JsonValue;
use sophia_api::serializer::{QuadSerializer, Stringifier};
use sophia_api::source::{QuadSource, SinkError, StreamResult};

mod engine;
mod rdf_object;
#[cfg(test)]
mod test;

/// A JSON-LD serializer.
///
/// ## Developers
///
/// * the generic parameter `W` is the output type of the serializer (typically a [`std::io::Write`])
/// * the generic parameter `L` is the type of the [document loader](json_ld::Loader)
pub struct JsonLdSerializer<W, L = NoLoader> {
    options: JsonLdOptions<L>,
    target: W,
}

impl<W> JsonLdSerializer<W> {
    /// Build a new JSON-LD serializer with the default options (no document loader).
    #[inline]
    pub fn new(target: W) -> Self {
        Self::new_with_options(target, JsonLdOptions::default())
    }
}

impl<W, L> JsonLdSerializer<W, L> {
    /// Build a new JSON-LD serializer writing to `write`, with the given options.
    pub const fn new_with_options(target: W, options: JsonLdOptions<L>) -> Self {
        Self { options, target }
    }

    /// Borrow this serializer's options.
    pub const fn options(&self) -> &JsonLdOptions<L> {
        &self.options
    }

    /// Convert a quad stream into a Json object
    fn convert_quads<QS>(
        &mut self,
        source: QS,
    ) -> StreamResult<JsonValue<()>, QS::Error, JsonLdError>
    where
        QS: QuadSource,
    {
        let mut engine = engine::Engine::new_with_options(&self.options);
        engine.process_quads(source)?;
        engine.into_json().map_err(SinkError)
    }
}

impl<W, L> QuadSerializer for JsonLdSerializer<W, L>
where
    W: std::io::Write,
{
    type Error = JsonLdError;

    fn serialize_quads<QS>(&mut self, source: QS) -> StreamResult<&mut Self, QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        let result = self.convert_quads(source)?;
        // TODO if the options contains a context,
        // try to compact 'result' before serializing.
        //
        // For this,
        // - first convert result into an ExpandedDocument
        //   using the TryFromJson trait
        // - then extract the context from the options
        // - compact the ExpandeDocument using this context
        let json_txt = match self.options.spaces() {
            0 => result.compact_print().to_string(),
            x => {
                let mut options = Options::pretty();
                options.indent = Indent::Spaces(x as u8);
                result.print_with(options).to_string()
            }
        };
        self.target
            .write(json_txt.as_bytes())
            .map_err(|e| SinkError(e.into()))?;
        Ok(self)
    }
}

/// A utility type alias of [`JsonLdSerializer`] with `[JsonTarget]` as its target.
///
/// ## Developers
///
/// * the generic parameter `L` is the type of the [document loader](json_ld::Loader)
///   (determined by the `options` parameters)
pub type Jsonifier<L = NoLoader> = JsonLdSerializer<JsonTarget, L>;

/// This type is just a placeholder [`JsonLdSerializer`]
/// targeting a [`JsonValue`].
/// See [`new_jsonifier`] and [`new_jsonifier_with`].
///
/// [`new_jsonifier`]: struct.JsonLdSerializer.html#method.new_jsonifier
/// [`new_jsonifier_with`]: struct.JsonLdSerializer.html#method.new_jsonifier_with
#[derive(Clone, Debug)]
pub struct JsonTarget(JsonValue<()>);

impl Jsonifier {
    /// Create a new serializer which targets a [`JsonValue`].
    #[inline]
    #[must_use]
    pub fn new_jsonifier() -> Self {
        Self::new(JsonTarget(JsonValue::Null))
    }
}

impl<L> Jsonifier<L> {
    /// Create a new serializer which targets a [`JsonValue`] with a custom options.
    #[inline]
    pub const fn new_jsonifier_with_options(options: JsonLdOptions<L>) -> Self {
        Self::new_with_options(JsonTarget(JsonValue::Null), options)
    }

    /// Get a reference to the converted `JsonValue`
    #[inline]
    pub const fn as_json(&self) -> &JsonValue<()> {
        &self.target.0
    }

    /// Extract the converted `JsonValue`
    #[inline]
    pub fn to_json(&mut self) -> JsonValue<()> {
        let mut ret = JsonValue::Null;
        std::mem::swap(&mut self.target.0, &mut ret);
        ret
    }
}

impl<L> QuadSerializer for Jsonifier<L> {
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
/// ## Developers
///
/// * the generic parameter `L` is the type of the [document loader](json_ld::Loader)
///   (determined by the `options` parameters)
pub type JsonLdStringifier<L = NoLoader> = JsonLdSerializer<Vec<u8>, L>;

impl JsonLdStringifier<NoLoader> {
    /// Create a new serializer which targets a string.
    #[inline]
    #[must_use]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
}

impl<L> JsonLdStringifier<L> {
    /// Create a new serializer which targets a string with a custom options.
    #[inline]
    pub const fn new_stringifier_with_options(options: JsonLdOptions<L>) -> Self {
        Self::new_with_options(Vec::new(), options)
    }
}

impl<L> Stringifier for JsonLdStringifier<L> {
    fn as_utf8(&self) -> &[u8] {
        &self.target[..]
    }
}
