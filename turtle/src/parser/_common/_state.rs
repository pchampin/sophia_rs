/// A trait capturing the commonalities between Turtle and TriG grammars
pub trait State: std::fmt::Debug {
    /// Construct a new state for production `annotationBlocks`
    fn new_annotation_block() -> Self;

    /// Construct a new state for production `ANON`
    fn new_anon(reifier: bool) -> Self;

    /// Construct a new state covering productions `prefixId`, `base` or `version`
    fn new_at_directive() -> Self;

    /// Construct a new state for production `IRIRef` in the context of a base directiive
    fn new_base_iri() -> Self;

    /// Construct a new state for production 'collection'
    fn new_collection() -> Self;

    /// Construct a new state covering production `blankNodePropertyList` and `ANON`
    fn new_blank_node_property_list_or_anon() -> Self;

    /// Construct a new state for production `objectList`
    fn new_object_list() -> Self;

    /// Construct a new state for production `object`
    fn new_object() -> Self;

    /// Construct a new state for production `predicateObjectList`
    fn new_predicate_object_list() -> Self;

    /// Construct a new state covering productions `prefixID` and `sparqlPrefix`
    fn new_prefix_declaration() -> Self;

    /// Construct a new state for production `IRIRef` in the context of a prefix declaration
    fn new_prefix_iri() -> Self;

    /// Construct a new state for production `RdfLiteral`
    fn new_rdf_literal(after_dt: bool) -> Self;

    /// Construct a new state for production `reifiedTriple`
    fn new_reified_triple(inside: bool) -> Self;

    /// Construct a new state for production `reifier`
    fn new_reifier() -> Self;

    /// Construct a new state for production `rtObject`
    fn new_rt_object() -> Self;

    /// Construct a new state for production `rtSubject`
    fn new_rt_subject() -> Self;

    /// Construct a new state for production `STRING_LITERAL_LONG_QUOTE` or `STRING_LITERAL_LONG_SINGLE_QUOTE`
    fn new_string_literal_long(quote: u8) -> Self;

    /// Construct a new state for production `tripleTerm`
    fn new_triple_term() -> Self;

    /// Construct a new state for production `ttObject`
    fn new_tt_object() -> Self;

    /// Construct a new state for production `ttSubject`
    fn new_tt_subject() -> Self;

    /// Construct a new state for production `verb`
    fn new_verb() -> Self;

    /// Construct a new state for production `VersionSpecifier`
    fn new_version_specifier() -> Self;

    /// This state corresponds to productions `annotationBlock`
    fn in_annotation_block(&self) -> bool;

    /// This state corresponds to productions `ANON`
    fn in_anon(&self) -> bool;

    /// This state corresponds to an at directive
    fn in_at_directive(&self) -> bool;

    /// This state corresponds to the production `IRIRef` in the context of a base directive
    fn in_base_iri(&self) -> bool;

    /// This state corresponds to productions `blankNodePropertyList` and `ANON`
    fn in_blank_node_property_list_or_anon(&self) -> bool;

    /// This state corresponds to production `collection`
    fn in_collection(&self) -> bool;

    /// This state corresponds to production `LANG_DIR`
    fn in_lang_dir(&self) -> bool;

    /// This state corresponds to production `objectList`
    fn in_object_list(&self) -> bool;

    /// This state corresponds to production `predicateObjectList`
    fn in_predicate_object_list(&self) -> bool;

    /// This state corresponds to productions `prefixId` or `sparqlPrefix`
    fn in_prefix_declaration(&self) -> bool;

    /// This state corresponds to productions `IRIRef` in the context of a prefix declataion
    fn in_prefix_iri(&self) -> bool;

    /// This state corresponds to productions `RdfLiteral`
    fn in_rdf_literal(&self) -> bool;

    /// This state corresponds to productions `reifiedTriple`
    fn in_reified_triple(&self, inside: bool) -> bool;

    /// This state corresponds to productions `reifier`
    fn in_reifier(&self) -> bool;

    /// This state corresponds to productions `rtObject`
    fn in_rt_object(&self) -> bool;

    /// This state corresponds to productions `rtSubject`
    fn in_rt_subject(&self) -> bool;

    /// This state corresponds to a context where productions `STRING_LITERAL_LONG_QUOTE` or `STRING_LITERAL_LONG_SINGLE_QUOTE` can occur
    fn in_string_literal_long(&self) -> bool;

    /// This state corresponds to production `tripleTerm`
    fn in_triple_term(&self) -> bool;

    /// This state corresponds to production `triples`
    fn in_triples(&self) -> bool;

    /// This state corresponds to productions `ttObject`
    fn in_tt_object(&self) -> bool;

    /// This state corresponds to productions `ttSubject`
    fn in_tt_subject(&self) -> bool;

    /// This state corresponds to production `verb`
    fn in_verb(&self) -> bool;

    /// This state corresponds to a context where production `BooleanLiteral` can occur
    fn in_boolean_literal_context(&self) -> bool;

    /// This state corresponds to a context where production `NumericLiteral` can occur
    fn in_numeric_literal_context(&self) -> bool;

    /// This state corresponds to a context where production `object` can occur
    fn in_object_context(&self) -> bool;

    /// This state corresponds to a context where production `NumericLiteral` can occur
    fn in_predicate_object_list_context(&self) -> bool;

    /// This state corresponds to a context where production `RDFLiteral` can occur
    fn in_rdf_literal_context(&self) -> bool;

    /// This state corresponds to a context where production `verb` can occur
    fn in_verb_context(&self) -> bool;

    /// This state corresponds to productions `prefixID` or `sparqlPrefix`
    fn in_version_specifier(&self) -> bool;

    /// If this state corresponds to production `triple`,
    /// return a mutable reference to the `require_pol` flag.
    fn as_triples_require_pol_mut(&mut self) -> Option<&mut bool>;
}
