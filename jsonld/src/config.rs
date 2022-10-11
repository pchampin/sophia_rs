//! Defines types for configuring JSON-LD processing.

/// JSON-LD serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct JsonLdConfig {
    /// The standard JSON-LD options
    options: JsonLdOptions,

    /// The number of spaces to indent new blocks with.
    pub spaces: u16,
}

impl std::ops::Deref for JsonLdConfig {
    type Target = JsonLdOptions;

    fn deref(&self) -> &Self::Target {
        &self.options
    }
}

impl std::ops::DerefMut for JsonLdConfig {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.options
    }
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
}

/// JSON-LD option, as defined by https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-processingmode
#[derive(Clone, Debug, Default)]
pub struct JsonLdOptions {
    /// The [`base`] IRI against which to resolve relative IRIs.
    ///
    /// [`base`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-base
    pub base: Option<String>,

    /// [`expandContext`] is a context that is used to initialize the active context when expanding a document.
    ///
    /// [`expandContext`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-expandcontext
    pub expand_context: Option<String>,

    /// The [`processingMode`] indicates which JSON-LD version to use during processing,
    /// which can be [`JSON-LD 1.0`] or [`JSON-LD 1.1`].
    ///
    /// [`JSON-LD 1.0`]: https://json-ld.org/spec/FCGS/json-ld-syntax/20130222/
    /// [`JSON-LD 1.1`]: https://www.w3.org/TR/json-ld11/
    /// [`processingMode`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-processingmode
    pub processing_mode: JsonLdSpecVersion,

    /// The [`rdfDirection`] flag, which determines how value objects containing
    /// a base direction are transformed to and from RDF.
    ///
    /// [`rdfDirection`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-rdfdirection
    pub rdf_direction: Option<RdfDirectionMode>,

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

impl JsonLdOptions {
    /// Build a new JSON-LD options.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the [`base`].
    ///
    /// [`base`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-base
    pub fn base(mut self, base: String) -> Self {
        self.base = Some(base);
        self
    }

    /// Set the [`expandContext`].
    ///
    /// [`expandContext`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-expandcontext
    pub fn expand_context(mut self, expand_context: String) -> Self {
        self.expand_context = Some(expand_context);
        self
    }

    /// Set the [`processingMode`].
    ///
    /// [`processingMode`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-processingmode
    pub fn processing_mode(mut self, version: JsonLdSpecVersion) -> Self {
        self.processing_mode = version;
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

impl JsonLdSpecVersion {
    /// Try to convert a given string to the corresponding JsonLdSpecVersion
    pub fn new(txt: &str) -> Option<Self> {
        match txt {
            "json-ld-1.0" => Some(JsonLdSpecVersion::JsonLd10),
            "json-ld-1.1" => Some(JsonLdSpecVersion::JsonLd11),
            _ => None,
        }
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
