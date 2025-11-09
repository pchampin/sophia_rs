//! I define [`TurtleState`]

use crate::parser::_common::State;

/// Represent the "position" of a [`TurtleSource`](super::TurtleSource)
/// in the grammar.
///
/// As much as possible, states are named after [Turtle Grammar] productions,
/// indicating the production being processed.
///
/// Note however that in the [`parse_line`] method,
/// which is where [`State`]s are used drive the parsing process,
/// some of those states are encountered just after entering the production,
/// while others are encountered just before exiting the production.
///
/// [Turtle Grammar]: https://www.w3.org/TR/rdf12-turtle/#sec-grammar-grammar
/// [`parse_line`]: crate::TurtleSource::parse_line
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TurtleState {
    /// in `prefixID` or `base`
    AtDirective,
    /// in `base` or `sparqlBase`, around `IRIREF`
    BaseIri,
    /// in prefixID or sparqlPrefix, around PNAME_NS
    PrefixDeclaration,
    /// in prefixID or sparqlPrefix, around IRIREF
    PrefixIri,
    /// in versionSpecifier
    VersionSpecifier,
    /// in `triples`, around a (possibly optional) `predicateObjectList`;
    /// the argument indicate whether the `predicateObjectList `is required
    Triples(bool),
    /// in `predicateObjectList`
    PredicateObjectList,
    /// in `objectList`
    ObjectList,
    /// in `verb`
    Verb,
    /// in `object`
    Object,
    /// in `blankNodePropertyList` or `ANON`
    BlankNodePropertyListOrAnon,
    /// in `collection`
    Collection,
    /// in `RDFLiteral`;
    /// the argument indicates whether we are after the datatype marker (true) or elsewhere (false)
    RdfLiteral(bool),
    /// in `reifier`
    Reifier,
    /// in `reifiedTriple`;
    /// the argument indicates whether we are inside (true) or at the end (false) of the production
    ReifiedTriple(bool),
    /// in `rtSubject`
    RtSubject,
    /// in `rtObject`
    RtObject,
    /// in `tripleTerm`
    TripleTerm,
    /// in `ttSubject`
    TtSubject,
    /// in `ttObject`
    TtObject,
    /// in `annotationBlock`, after the opening bracker '{|'
    AnnotationBlock,
    /// in STRING_LITERAL_LONG_SINGLE_QUOTE or STRING_LITERAL_LONG_QUOTE
    StringLiteralLong(u8),
    /// in `ANON`;
    /// the argument indicates whether this ANON is a reifier
    Anon(bool),
}

impl State for TurtleState {
    fn new_annotation_block() -> Self {
        Self::AnnotationBlock
    }

    fn new_anon(reifier: bool) -> Self {
        Self::Anon(reifier)
    }

    fn new_at_directive() -> Self {
        Self::AtDirective
    }

    fn new_base_iri() -> Self {
        Self::BaseIri
    }

    fn new_collection() -> Self {
        Self::Collection
    }

    fn new_blank_node_property_list_or_anon() -> Self {
        Self::BlankNodePropertyListOrAnon
    }

    fn new_object_list() -> Self {
        Self::ObjectList
    }

    fn new_object() -> Self {
        Self::Object
    }

    fn new_predicate_object_list() -> Self {
        Self::PredicateObjectList
    }

    fn new_prefix_declaration() -> Self {
        Self::PrefixDeclaration
    }

    fn new_prefix_iri() -> Self {
        Self::PrefixIri
    }

    fn new_rdf_literal(after_dt: bool) -> Self {
        Self::RdfLiteral(after_dt)
    }

    fn new_reified_triple(inside: bool) -> Self {
        Self::ReifiedTriple(inside)
    }

    fn new_reifier() -> Self {
        Self::Reifier
    }

    fn new_rt_object() -> Self {
        Self::RtObject
    }

    fn new_rt_subject() -> Self {
        Self::RtSubject
    }

    fn new_string_literal_long(quote: u8) -> Self {
        debug_assert!(matches!(quote, b'"' | b'\''));
        Self::StringLiteralLong(quote)
    }

    fn new_triple_term() -> Self {
        Self::TripleTerm
    }

    fn new_tt_object() -> Self {
        Self::TtObject
    }

    fn new_tt_subject() -> Self {
        Self::TtSubject
    }

    fn new_verb() -> Self {
        Self::Verb
    }

    fn new_version_specifier() -> Self {
        Self::VersionSpecifier
    }

    fn in_annotation_block(&self) -> bool {
        matches!(self, Self::AnnotationBlock)
    }

    fn in_anon(&self) -> bool {
        matches!(self, Self::Anon(_))
    }

    fn in_at_directive(&self) -> bool {
        matches!(self, Self::AtDirective)
    }

    fn in_base_iri(&self) -> bool {
        matches!(self, Self::BaseIri)
    }

    fn in_blank_node_property_list_or_anon(&self) -> bool {
        matches!(self, Self::BlankNodePropertyListOrAnon)
    }

    fn in_collection(&self) -> bool {
        matches!(self, Self::Collection)
    }

    fn in_lang_dir(&self) -> bool {
        matches!(self, Self::RdfLiteral(_))
    }

    fn in_object_list(&self) -> bool {
        matches!(self, Self::ObjectList)
    }

    fn in_predicate_object_list(&self) -> bool {
        matches!(self, Self::PredicateObjectList)
    }

    fn in_prefix_declaration(&self) -> bool {
        matches!(self, Self::PrefixDeclaration)
    }

    fn in_prefix_iri(&self) -> bool {
        matches!(self, Self::PrefixIri)
    }

    fn in_rdf_literal(&self) -> bool {
        matches!(self, Self::RdfLiteral(_))
    }

    fn in_reified_triple(&self, inside: bool) -> bool {
        if let Self::ReifiedTriple(i) = self {
            *i == inside
        } else {
            false
        }
    }

    fn in_reifier(&self) -> bool {
        matches!(self, Self::Reifier)
    }

    fn in_rt_object(&self) -> bool {
        matches!(self, Self::RtObject)
    }

    fn in_rt_subject(&self) -> bool {
        matches!(self, Self::RtSubject)
    }

    fn in_string_literal_long(&self) -> bool {
        matches!(self, Self::StringLiteralLong(_))
    }

    fn in_triple_term(&self) -> bool {
        matches!(self, Self::TripleTerm)
    }

    fn in_triples(&self) -> bool {
        matches!(self, Self::Triples(_))
    }

    fn in_tt_object(&self) -> bool {
        matches!(self, Self::TtObject)
    }

    fn in_tt_subject(&self) -> bool {
        matches!(self, Self::TtSubject)
    }

    fn in_verb(&self) -> bool {
        matches!(self, Self::Verb)
    }

    fn in_version_specifier(&self) -> bool {
        matches!(self, Self::VersionSpecifier)
    }

    fn in_boolean_literal_context(&self) -> bool {
        matches!(
            self,
            Self::ObjectList | Self::Collection | Self::RtObject | Self::TtObject
        )
    }

    fn in_numeric_literal_context(&self) -> bool {
        matches!(
            self,
            Self::ObjectList | Self::Collection | Self::RtObject | Self::TtObject
        )
    }

    fn in_object_context(&self) -> bool {
        matches!(self, Self::ObjectList | Self::Collection)
    }

    fn in_predicate_object_list_context(&self) -> bool {
        matches!(
            self,
            TurtleState::AnnotationBlock
                | TurtleState::BlankNodePropertyListOrAnon
                | TurtleState::Triples(_)
        )
    }

    fn in_rdf_literal_context(&self) -> bool {
        matches!(self, Self::Object | Self::RtObject | Self::TtObject)
    }

    fn in_verb_context(&self) -> bool {
        matches!(
            self,
            TurtleState::PredicateObjectList
                | TurtleState::ReifiedTriple(true)
                | TurtleState::TripleTerm
        )
    }

    fn as_triples_require_pol_mut(&mut self) -> Option<&mut bool> {
        if let Self::Triples(ret) = self {
            Some(ret)
        } else {
            None
        }
    }
}
