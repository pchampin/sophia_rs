//! JSON-LD serializer configuration.

/// JSON-LD serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct JsonLdConfig {
    /// A JSON-LD option determining how value objects containing
    /// a base direction are transformed to and from RDF.
    ///
    /// [`rdfDirection`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-rdfdirection
    pub rdf_direction: Option<RdfDirectionMode>,

    /// The number of spaces to indent new blocks with.
    pub spaces: u16,

    /// The JSON-LD spec version, which can be [`JSON-LD 1.0`] or [`JSON-LD 1.1`].
    ///
    /// [`JSON-LD 1.0`]: https://json-ld.org/spec/FCGS/json-ld-syntax/20130222/
    /// [`JSON-LD 1.1`]: https://www.w3.org/TR/json-ld11/
    pub spec_version: JsonLdSpecVersion,

    /// The [`useNativeTypes`] flag, which causes the `Serialize RDF as JSON-LD Algorithm`
    /// to use native JSON values in value objects avoiding the need for an explicit `@type`.
    ///
    /// [`useNativeTypes`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-usenativetypes
    pub use_native_types: bool,

    /// The [`useRdfType`] flag, which enables special rules for the `Serialize RDF as JSON-LD
    /// Algorithm` causing `rdf:type` properties to be kept as IRIs in the output, rather than use
    /// `@type`.
    ///
    /// [`useRdfType`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-userdftype
    pub use_rdf_type: bool,
}

impl JsonLdConfig {
    /// Build a new JSON-LD serializer configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the number of spaces to indent new blocks with.
    pub fn spaces(mut self, spaces: u16) -> Self {
        self.spaces = spaces;
        self
    }

    /// Set the JSON-LD spec version.
    pub fn spec_version(mut self, version: JsonLdSpecVersion) -> Self {
        self.spec_version = version;
        self
    }

    /// Set the [`useNativeTypes`] flag.
    ///
    /// [`useNativeTypes`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-usenativetypes
    pub fn use_native_types(mut self, flag: bool) -> Self {
        self.use_native_types = flag;
        self
    }

    /// Set the [`useRdfType`] flag.
    ///
    /// [`useRdfType`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-userdftype
    pub fn use_rdf_type(mut self, flag: bool) -> Self {
        self.use_rdf_type = flag;
        self
    }
}

/// The JSON-LD spec version, which can be [`JSON-LD 1.0`] or [`JSON-LD 1.1`].
///
/// [`JSON-LD 1.0`]: https://json-ld.org/spec/FCGS/json-ld-syntax/20130222/
/// [`JSON-LD 1.1`]: https://www.w3.org/TR/json-ld11/
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum JsonLdSpecVersion {
    /// [`JSON-LD 1.0`](https://json-ld.org/spec/FCGS/json-ld-syntax/20130222/)
    JsonLd10,

    /// [`JSON-LD 1.1`](https://www.w3.org/TR/json-ld11/)
    JsonLd11,
}

impl Default for JsonLdSpecVersion {
    fn default() -> Self {
        JsonLdSpecVersion::JsonLd11
    }
}

/// A JSON-LD option determining how value objects containing
/// a base direction are transformed to and from RDF.
///
/// [`rdfDirection`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-rdfdirection
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RdfDirectionMode {
    /// The `i18n-datatype` setting of `rdfDirection` determines that an `RDF literal` is generated
    /// using a datatype IRI based on `https://www.w3.org/ns/i18n#` with both the `language tag`
    /// (if present) and `base direction` encoded. When transforming from RDF, this datatype is
    /// decoded to create a `value object` containing `@language` (if present) and `@direction`.
    I18nDatatype,

    /// The `compound-literal` setting of `rdfDirection` determines that a `blank node` is emitted
    /// instead of a literal, where the blank node is the subject of `rdf:value`, `rdf:direction`,
    /// and `rdf:language` (if present) properties. When transforming from RDF, this object is
    /// decoded to create a `value object` containing `@language` (if present) and `@direction`.
    CompoundLiteral,
}
