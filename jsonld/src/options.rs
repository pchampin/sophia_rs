//! Defines types for configuring JSON-LD processing.

use json_ld::expansion::Policy;
pub use json_ld::rdf::RdfDirection;
use json_ld::syntax::context::Value;
use json_ld::NoLoader;
pub use json_ld::Options;
pub use json_ld::ProcessingMode;
use locspan::Location;
use locspan::Span;
use sophia_iri::Iri;

use crate::loader_factory::DefaultLoaderFactory;
use crate::loader_factory::LoaderFactory;
use crate::vocabulary::ArcIri;

/// JSON-LD option, as defined by <https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-processingmode>.
///
/// NB: this type slightly differs from the standard [`JsonLdOptions`] type:
/// * the standard parameters that are not relevant for this implementation are not present;
/// * some non-standard parameters have been added.
///
/// ## Developers
///
/// * the generic parameter `L` is the type of the [document loader](json_ld::Loader)
#[derive(Default)]
pub struct JsonLdOptions<LF> {
    inner: InnerOptions,
    loader_factory: LF,
    use_native_types: bool,
    use_rdf_type: bool,
    // non standard:
    spaces: u16,
    compact_context: Option<ContextRef>,
}

impl JsonLdOptions<DefaultLoaderFactory<NoLoader>> {
    /// Build a new JSON-LD options.
    pub fn new() -> Self {
        Self::default()
    }
}

impl<LF> JsonLdOptions<LF> {
    /// The [`base`] IRI against which to resolve relative IRIs.
    ///
    /// [`base`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-base
    pub fn base(&self) -> Option<Iri<&str>> {
        self.inner.base.as_ref().map(|i| i.as_ref())
    }

    /// [`compactArrays`] instructs the JSON-LD processor to replace arrays of one element with that element during [compaction].
    ///
    /// [`compactArrays`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-compactarrays
    /// [compaction]: https://www.w3.org/TR/json-ld11-api/#dfn-compact
    pub fn compact_arrays(&self) -> bool {
        self.inner.compact_arrays
    }

    /// [`compactToRelative`] instructs the JSON-LD processor to produce IRIs references relative to the base when [compacting].
    ///
    /// [`compactToRelative`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-compacttorelative
    /// [compacting]: https://www.w3.org/TR/json-ld11-api/#dfn-compact
    pub fn compact_to_relative(&self) -> bool {
        self.inner.compact_to_relative
    }

    /// The [`documentLoader`] is used to retrieve remote documents and contexts.
    /// The returned factory can yield a [`documentLoader`].
    ///
    /// [`documentLoader`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-documentloader
    pub fn document_loader_factory(&self) -> &LF {
        &self.loader_factory
    }

    /// [`expandContext`] is a context that is used to initialize the active context when expanding a document.
    ///
    /// [`expandContext`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-expandcontext
    pub fn expand_context(&self) -> Option<&ContextRef> {
        self.inner.expand_context.as_ref()
    }

    /// When [`ordered`] is true,
    /// certain algorithm processing steps where indicated are ordered lexicographically.
    /// If false, order is not considered in processing.
    ///
    /// [`ordered`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-ordered
    pub fn ordered(&self) -> bool {
        self.inner.ordered
    }

    /// The [`processingMode`] indicates which JSON-LD version to use during processing,
    /// which can be:
    /// * [`JsonLd1_0`] for [JSON-LD 1.0]
    /// * [`JsonLd1_1`] for [JSON-LD 1.1].
    ///
    /// [`processingMode`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-processingmode
    /// [`JsonLd1_0`]: ProcessingMode::JsonLd1_0
    /// [`JsonLd1_1`]: ProcessingMode::JsonLd1_1
    /// [JSON-LD 1.0]: https://json-ld.org/spec/FCGS/json-ld-syntax/20130222/
    /// [JSON-LD 1.1]: https://www.w3.org/TR/json-ld11/
    pub fn processing_mode(&self) -> ProcessingMode {
        self.inner.processing_mode
    }

    /// [`produceGeneralizedRdf`] authorizes the JSON-LD to emit blank nodes for triple predicates, otherwise they will be omitted.
    ///
    /// [`produceGeneralizedRdf`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-producegeneralizedrdf
    pub fn produce_generalized_rdf(&self) -> bool {
        self.inner.produce_generalized_rdf
    }

    /// The [`rdfDirection`] flag, which determines how value objects containing
    /// a base direction are transformed to and from RDF.
    ///
    /// [`rdfDirection`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-rdfdirection
    pub fn rdf_direction(&self) -> Option<RdfDirection> {
        self.inner.rdf_direction
    }

    /// The [`useNativeTypes`] flag, which causes the `Serialize RDF as JSON-LD Algorithm`
    /// to use native JSON values in value objects avoiding the need for an explicit `@type`.
    ///
    /// [`useNativeTypes`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-usenativetypes
    pub fn use_native_types(&self) -> bool {
        self.use_native_types
    }

    /// The [`useRdfType`] flag, which enables special rules for the `Serialize RDF as JSON-LD
    /// Algorithm` causing `rdf:type` properties to be kept as IRIs in the output, rather than use
    /// `@type`.
    ///
    /// [`useRdfType`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-userdftype
    pub fn use_rdf_type(&self) -> bool {
        self.use_rdf_type
    }

    /// Return the [expansion policy](Policy).
    ///
    /// NB: this is not a standard option of JSON-LD,
    /// but a specific option of the [`json_ld` crate](json_ld).
    pub fn expansion_policy(&self) -> Policy {
        self.inner.expansion_policy
    }

    /// Return the number of spaces to use for indentation in the JSON output.
    ///
    /// NB: this is not a standard option of JSON-LD.
    pub fn spaces(&self) -> u16 {
        self.spaces
    }

    /// The context to be used to compact the output, if ant.
    ///
    /// NB: this is not a standard option of JSON-LD.
    pub fn compact_context(&self) -> Option<&ContextRef> {
        self.compact_context.as_ref()
    }

    pub(crate) fn inner(&self) -> &InnerOptions {
        &self.inner
    }

    // Creating Options

    /// Change the [`base`](Self::base) IRI
    ///
    /// See also [`with_no_base`](Self::with_no_base)
    pub fn with_base(mut self, base: ArcIri) -> Self {
        self.inner.base = Some(base);
        self
    }

    /// Change the [`base`](Self::base) IRI
    ///
    /// See also [`with_base`](Self::with_base)
    pub fn with_no_base(mut self) -> Self {
        self.inner.base = None;
        self
    }

    /// Change the [`compact_arrays`](Self::compact_arrays) flag
    pub fn with_compact_arrays(mut self, compact_arrays: bool) -> Self {
        self.inner.compact_arrays = compact_arrays;
        self
    }

    /// Change the [`compact_to_relative`](Self::compact_to_relative) flag
    pub fn with_compact_to_relative(mut self, compact_to_relative: bool) -> Self {
        self.inner.compact_to_relative = compact_to_relative;
        self
    }

    /// Change the [`document_loader`](Self::document_loader)
    pub fn with_document_loader_factory<LF2>(
        self,
        document_loader_factory: LF2,
    ) -> JsonLdOptions<LF2> {
        JsonLdOptions {
            inner: self.inner,
            loader_factory: document_loader_factory,
            use_native_types: self.use_native_types,
            use_rdf_type: self.use_native_types,
            spaces: self.spaces,
            compact_context: self.compact_context,
        }
    }

    /// Change the [`expand_context`](Self::expand_context)
    ///
    /// See also [`with_no_expand_context`](Self::with_no_expand_context)
    pub fn with_expand_context<SE2>(mut self, expand_context: ContextRef) -> Self {
        self.inner.expand_context = Some(expand_context);
        self
    }

    /// Change the [`expand_context`](Self::expand_context)
    ///
    /// See also [`with_expand_context`](Self::with_expand_context)
    pub fn with_no_expand_context(mut self) -> Self {
        self.inner.expand_context = None;
        self
    }

    /// Change the [`ordered`](Self::ordered) flag
    pub fn with_ordered(mut self, ordered: bool) -> Self {
        self.inner.ordered = ordered;
        self
    }

    /// Change the [`processing_mode`](Self::processing_mode)
    pub fn with_processing_mode(mut self, version: ProcessingMode) -> Self {
        self.inner.processing_mode = version;
        self
    }

    /// Change the [`produce_generalized_rdf`](Self::produce_generalized_rdf) flag
    pub fn with_produce_generalized_rdf(mut self, produce_generalized_rdf: bool) -> Self {
        self.inner.produce_generalized_rdf = produce_generalized_rdf;
        self
    }

    /// Change the [`rdf_direction`](Self::rdf_direction)
    ///
    /// See also [`with_no_rdf_direction`](Self::with_no_rdf_direction).
    pub fn with_rdf_direction(mut self, rdf_direction: RdfDirection) -> Self {
        self.inner.rdf_direction = Some(rdf_direction);
        self
    }

    /// Change the [`rdf_direction`](Self::rdf_direction)
    ///
    /// See also [`with_rdf_direction`](Self::with_rdf_direction).
    pub fn with_no_rdf_direction(mut self) -> Self {
        self.inner.rdf_direction = None;
        self
    }

    /// Change the [`use_native_types`](Self::use_native_types) flag
    pub fn with_use_native_types(mut self, flag: bool) -> Self {
        self.use_native_types = flag;
        self
    }

    /// Change the [`use_native_types`](Self::use_native_types) flag
    pub fn with_use_rdf_type(mut self, flag: bool) -> Self {
        self.use_rdf_type = flag;
        self
    }

    /// Change the [expansion policy](Self::expansion_policy)
    pub fn with_expansion_policy(mut self, policy: Policy) -> Self {
        self.inner.expansion_policy = policy;
        self
    }

    /// Changes the [`spaces`](Self::spaces) option
    pub fn with_spaces(mut self, spaces: u16) -> Self {
        self.spaces = spaces;
        self
    }

    /// Change the [`compact_context`](Self::compact_context)
    ///
    /// See also [`with_no_compact_context`](Self::with_no_compact_context)
    pub fn with_compact_context<SE2>(mut self, compact_context: ContextRef) -> Self {
        self.compact_context = Some(compact_context);
        self
    }

    /// Change the [`compact_context`](Self::compact_context)
    ///
    /// See also [`with_compact_context`](Self::with_compact_context)
    pub fn with_no_compact_context(mut self) -> Self {
        self.compact_context = None;
        self
    }
}

impl<LF: LoaderFactory> JsonLdOptions<LF> {
    /// The [`documentLoader`] is used to retrieve remote documents and contexts.
    /// [`documentLoader`]: https://www.w3.org/TR/json-ld11-api/#dom-jsonldoptions-documentloader
    pub fn document_loader(&self) -> LF::Loader<'_> {
        self.loader_factory.yield_loader()
    }
}

impl<LF> std::ops::Deref for JsonLdOptions<LF> {
    type Target = InnerOptions;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Type alias for the context references used by the JSON-LD options.
pub type ContextRef =
    json_ld::RemoteDocumentReference<ArcIri, Location<ArcIri, Span>, Value<Location<ArcIri, Span>>>;

/// Type alias for [`json_ld::Options`] as used in this crate.
type InnerOptions = json_ld::Options<ArcIri, Location<ArcIri, Span>, Value<Location<ArcIri, Span>>>;
