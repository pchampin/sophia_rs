//! A JSON-LD parser implementing the
//! [`Deserialize JSON-LD to RDF Algorithm`].
//!
//! [`Deserialize JSON-LD to RDF Algorithm`]: https://www.w3.org/TR/json-ld11-api/#deserialize-json-ld-to-rdf-algorithm
//!
//! IMPORTANT: for the moment,
//! * the implementation only support JSON-LD documents in the *expanded* form;
//! * it does not support JSON canonicalization for the `@json` datatype,
//! * numbers are not always serialized exactly as per the spec
//!   (e.g. 1E0 instead of 1.0E0),
//! * it always produce generalized RDF (e.g. bnodes as predicate).

use crate::{JsonLdConfig, JsonLdError, RdfDirectionMode};
use json::JsonValue;
use sophia_api::ns::{rdf, xsd};
use sophia_api::parser::QuadParser;
use sophia_api::quad::stream::QuadSource;
use sophia_api::quad::streaming_mode::{ByValue, StreamedQuad};
use sophia_api::triple::stream::StreamError::{SinkError, SourceError};
use sophia_term::{BoxTerm, MownTerm};
use std::borrow::Borrow;
use std::collections::btree_map::Entry;
use std::io::BufRead;

mod bnode_gen;
use bnode_gen::*;
mod node_map;
use node_map::*;
mod ext_trait;
use ext_trait::*;
#[cfg(test)]
mod test;

/// A JSON-LD parser.
#[derive(Clone, Debug, Default)]
pub struct JsonLdParser {
    config: JsonLdConfig,
}

impl JsonLdParser {
    /// Build a new JSON-LD parser with the default config.
    #[inline]
    pub fn new() -> JsonLdParser {
        Self::default()
    }

    /// Build a new JSON-LD parser with the given config.
    pub fn new_with_config(config: JsonLdConfig) -> JsonLdParser {
        JsonLdParser { config }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &JsonLdConfig {
        &self.config
    }

    /// Parse directly a [`JsonValue`]
    pub fn parse_json_value<'a>(&self, value: &'a JsonValue) -> JsonLdParserSource<&'a JsonValue> {
        JsonLdParserSource {
            expanded: Ok(value),
            config: self.config.clone(),
        }
    }
}

impl<B: BufRead> QuadParser<B> for JsonLdParser {
    type Source = JsonLdParserSource<JsonValue>;

    fn parse(&self, bufread: B) -> Self::Source {
        let expanded = || -> Result<JsonValue, JsonLdError> {
            let mut src = String::new();
            for line in bufread.lines() {
                src.push_str(&line?)
            }
            Ok(json::parse(&src)?)
        }();
        JsonLdParserSource {
            expanded,
            config: self.config.clone(),
        }
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(JsonLdParser, QuadParser);

/// The [`QuadSource`] produced by a [`JsonLdParser`].
pub struct JsonLdParserSource<T: Borrow<JsonValue>> {
    expanded: Result<T, JsonLdError>,
    config: JsonLdConfig,
}

impl<T: Borrow<JsonValue>> QuadSource for JsonLdParserSource<T> {
    type Error = JsonLdError;

    type Quad = ByValue<([BoxTerm; 3], Option<BoxTerm>)>;

    fn try_for_some_quad<F, E>(
        &mut self,
        f: &mut F,
    ) -> sophia_api::triple::stream::StreamResult<bool, Self::Error, E>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), E>,
        E: std::error::Error,
    {
        let mut expanded = Err(JsonLdError::ExhaustedSource);
        std::mem::swap(&mut expanded, &mut self.expanded);
        let expanded = expanded.map_err(SourceError)?;
        let expanded = expanded.borrow();
        let bnode_gen = BNodeGen::new();
        let node_map = generate_node_map(expanded, &bnode_gen).map_err(SourceError)?;
        for (graph_name, nodes) in node_map {
            let graph_name = if graph_name == "@default" {
                None
            } else {
                match try_term(graph_name) {
                    Some(x) => Some(x),
                    None => continue,
                }
            };
            for (node_id, node_obj) in nodes {
                let node_id = match try_term(node_id) {
                    Some(x) => x,
                    None => continue,
                };
                for typ in node_obj.types {
                    let typ = if &typ[..2] == "_:" {
                        match MownTerm::new_bnode(&typ[2..]) {
                            Ok(typ) => typ,
                            Err(_) => continue,
                        }
                    } else {
                        match MownTerm::new_iri(typ) {
                            Ok(typ) => typ,
                            Err(_) => continue,
                        }
                    };
                    f(StreamedQuad::by_value(patch((
                        [node_id.clone(), rdf::type_.into(), typ],
                        graph_name.clone(),
                    ))))
                    .map_err(SinkError)?;
                }
                for (property, values) in node_obj.properties {
                    let property = match try_term(property) {
                        Some(x) => x,
                        None => continue,
                    };
                    for value in values {
                        let mut list_triples = vec![];
                        let value = match object_to_rdf(
                            &value,
                            &mut list_triples,
                            &self.config,
                            &bnode_gen,
                        ) {
                            Ok(x) => x,
                            Err(()) => continue,
                        };
                        f(StreamedQuad::by_value(patch((
                            [node_id.clone(), property.clone(), value],
                            graph_name.clone(),
                        ))))
                        .map_err(SinkError)?;
                        for t in list_triples {
                            f(StreamedQuad::by_value(patch((t, graph_name.clone()))))
                                .map_err(SinkError)?;
                        }
                    }
                }
            }
        }
        Ok(false)
    }
}

fn try_term(txt: &str) -> Option<MownTerm> {
    if txt.starts_with("_:") {
        Some(MownTerm::new_bnode_unchecked(&txt[2..]))
    } else {
        MownTerm::new_iri(txt).ok()
    }
}

fn patch(([s, p, o], g): ([MownTerm; 3], Option<MownTerm>)) -> ([BoxTerm; 3], Option<BoxTerm>) {
    (
        [s.map(Into::into), p.map(Into::into), o.map(Into::into)],
        g.map(|gn| gn.map(Into::into)),
    )
}

/// Implements Object to RDF conversion,
/// according to https://www.w3.org/TR/json-ld11-api/#object-to-rdf-conversion
fn object_to_rdf<'a>(
    obj: &NodeObjectEntry<'a>,
    list_triples: &mut Vec<[MownTerm<'a>; 3]>,
    config: &JsonLdConfig,
    bnode_gen: &'a BNodeGen,
) -> Result<MownTerm<'a>, ()> {
    match obj {
        NodeObjectEntry::Id(id) => try_term(id).ok_or(()),
        NodeObjectEntry::List(vals) => {
            let vals: Result<Vec<_>, ()> = vals
                .into_iter()
                .map(|i| object_to_rdf(i, list_triples, config, bnode_gen))
                .collect();
            // insert rdf:first triples and substitude items with their bnodes
            let bns: Vec<_> = vals?
                .into_iter()
                .map(|val| {
                    let bn = MownTerm::new_bnode_unchecked(&bnode_gen.fresh()[2..]);
                    list_triples.push([bn.clone(), rdf::first.into(), val]);
                    bn
                })
                .collect();
            // insert rdf:rest triples and return root
            let root = bns.into_iter().rfold(rdf::nil.into(), |next, this| {
                list_triples.push([this.clone(), rdf::rest.into(), next]);
                this
            });
            Ok(root)
        }
        NodeObjectEntry::Value(obj) => {
            let obj = obj
                .as_object()
                .expect("NodeObjectEntry::Value must be an object");
            let value = obj.get("@value").expect("must be a value object");
            let datatype = obj.get("@type").and_then(JsonValue::as_str);
            if datatype == Some("@json") {
                // TODO normalize the JSON value
                return Ok(MownTerm::new_literal_dt_unchecked(
                    format!("{}", value),
                    rdf::JSON,
                ));
            }
            let datatype = datatype
                .map(|txt| MownTerm::new_iri(txt))
                .transpose()
                .map_err(|_| ())?;
            if let Some(b) = value.as_bool() {
                Ok(MownTerm::new_literal_dt_unchecked(
                    if b { "true" } else { "false" },
                    datatype.unwrap_or_else(|| xsd::boolean.into()),
                ))
            } else if let Some(i) = value.as_i64() {
                let datatype = datatype.unwrap_or_else(|| xsd::integer.into());
                let lex = if xsd::double == datatype {
                    format!("{}.0E0", i)
                } else {
                    format!("{}", i)
                };
                Ok(MownTerm::new_literal_dt_unchecked(lex, datatype))
            } else if let Some(i) = value.as_u64() {
                let datatype = datatype.unwrap_or_else(|| xsd::integer.into());
                let lex = if xsd::double == datatype {
                    format!("{}.0E0", i)
                } else {
                    format!("{}", i)
                };
                Ok(MownTerm::new_literal_dt_unchecked(lex, datatype))
            } else if let Some(f) = value.as_f64() {
                Ok(MownTerm::new_literal_dt_unchecked(
                    format!("{:.E}", f),
                    datatype.unwrap_or_else(|| xsd::double.into()),
                ))
            } else if let Some(s) = value.as_str() {
                if let (Some(direction), Some(cfg)) = (obj.get("@direction"), config.rdf_direction)
                {
                    let tag = obj
                        .get("@language")
                        .and_then(JsonValue::as_str)
                        .unwrap_or("")
                        .to_ascii_lowercase();
                    let dir = direction.as_str().ok_or(())?;
                    match cfg {
                        RdfDirectionMode::I18nDatatype => {
                            let datatype = format!("https://www.w3.org/ns/i18n#{}_{}", tag, dir,);
                            let datatype = sophia_term::iri::Iri::new_unchecked(datatype);
                            Ok(MownTerm::new_literal_dt_unchecked(s, datatype))
                        }
                        RdfDirectionMode::CompoundLiteral => {
                            let bn = MownTerm::new_bnode_unchecked(&bnode_gen.fresh()[2..]);
                            list_triples.push([
                                bn.clone(),
                                rdf::value.into(),
                                MownTerm::new_literal_dt_unchecked(s, xsd::string),
                            ]);
                            if !tag.is_empty() {
                                list_triples.push([
                                    bn.clone(),
                                    rdf::language.into(),
                                    MownTerm::new_literal_dt_unchecked(tag, xsd::string),
                                ]);
                            }
                            list_triples.push([
                                bn.clone(),
                                rdf::direction.into(),
                                MownTerm::new_literal_dt_unchecked(dir, xsd::string),
                            ]);
                            Ok(bn)
                        }
                    }
                } else {
                    if let Some(tag) = obj.get("@language") {
                        MownTerm::new_literal_lang(s, tag.as_str().unwrap_or("")).map_err(|_| ())
                    } else {
                        Ok(MownTerm::new_literal_dt_unchecked(
                            s,
                            datatype.unwrap_or_else(|| xsd::string.into()),
                        ))
                    }
                }
            } else {
                Err(())
            }
        }
    }
}

fn generate_node_map<'a>(
    expanded: &'a JsonValue,
    bnode_gen: &'a BNodeGen<'a>,
) -> Result<NodeMap<'a>, JsonLdError> {
    let mut node_map = [("@default", NamedGraph::new())].into_iter().collect();
    let mut list = None;
    populate_node_map(
        expanded,
        &mut node_map,
        "@default",
        ActiveSubject::Null,
        None,
        &mut list,
        &bnode_gen,
    )?;
    Ok(node_map)
}

fn populate_node_map<'a>(
    expanded: &'a JsonValue,
    node_map: &mut NodeMap<'a>,
    active_graph: &'a str,
    active_subject: ActiveSubject<'a>,
    active_property: Option<&'a str>,
    list: &mut Option<Vec<NodeObjectEntry<'a>>>,
    bnode_gen: &'a BNodeGen<'a>,
) -> Result<(), JsonLdError> {
    match expanded {
        JsonValue::Array(element) => {
            // Step 1.
            for item in element {
                populate_node_map(
                    item,
                    node_map,
                    active_graph,
                    active_subject,
                    active_property,
                    list,
                    bnode_gen,
                )?;
            }
            Ok(())
        }
        JsonValue::Object(element) => {
            // Step 2.
            /*
            // graph and subject_node are set only when needed,
            // because the borrow-checker does not like it when they are set up-front
            let graph = node_map.get_mut(active_graph).unwrap();
            let subject_node = match active_subject {
                ActiveSubject::Id(id) => graph.get_mut(id),
                _ => None,
            };
            */
            // Step 3.
            let mut types: Vec<&str> = element
                .get_all("@type")
                .map(|item| match item.as_str() {
                    Some(s) => {
                        if s.starts_with("_:") {
                            Ok(bnode_gen.get(&s))
                        } else {
                            Ok(&s[..])
                        }
                    }
                    None => Err(JsonLdError::CanNotDeserialize(format!(
                        "Unexpected value for @type: {}",
                        item
                    ))),
                })
                .collect::<Result<Vec<&str>, JsonLdError>>()?;
            // Step 4.
            if let Some(_) = element.get("@value") {
                let element = NodeObjectEntry::Value(expanded);
                match list {
                    None => {
                        let graph = node_map.get_mut(active_graph).unwrap();
                        let subject_node = match active_subject {
                            ActiveSubject::Id(id) => graph.get_mut(id).unwrap(),
                            _ => unreachable!("4.1.1. subject_node must be a map"),
                        };
                        let active_property = active_property.unwrap();
                        match subject_node.properties.entry(active_property) {
                            Entry::Vacant(e) => {
                                e.insert(vec![element]);
                            }
                            Entry::Occupied(mut e) => {
                                let v = e.get_mut();
                                if v.iter().all(|i| i != &element) {
                                    v.push(element)
                                }
                            }
                        }
                    }
                    Some(v) => {
                        v.push(element);
                    }
                }
            }
            // Step 5.
            else if let Some(lst) = element.get("@list") {
                let lst = lst.as_array().ok_or_else(|| {
                    JsonLdError::CanNotDeserialize(format!(
                        "@list should be an array, found {}",
                        lst
                    ))
                })?;
                let mut result = Some(vec![]);
                for i in lst {
                    populate_node_map(
                        i,
                        node_map,
                        active_graph,
                        active_subject,
                        active_property,
                        &mut result,
                        bnode_gen,
                    )?;
                }
                let result = result.unwrap();
                match list {
                    None => {
                        let graph = node_map.get_mut(active_graph).unwrap();
                        let subject_node = match active_subject {
                            ActiveSubject::Id(id) => graph.get_mut(id).unwrap(),
                            _ => unreachable!("5.3. subject_node must be a map"),
                        };
                        let active_property = active_property.unwrap();
                        let result = NodeObjectEntry::List(result);
                        match subject_node.properties.entry(active_property) {
                            Entry::Vacant(e) => {
                                e.insert(vec![result]);
                            }
                            Entry::Occupied(mut e) => {
                                e.get_mut().push(result);
                            }
                        }
                    }
                    Some(list) => {
                        list.push(NodeObjectEntry::List(result));
                    }
                }
            }
            // Step 6.
            else {
                let id = match element.get("@id") {
                    None => bnode_gen.fresh(),
                    Some(id) if id.is_string() => {
                        let id = id.as_str().unwrap();
                        if id.starts_with("_:") {
                            bnode_gen.get(id)
                        } else {
                            id
                        }
                    }
                    Some(JsonValue::Null) => return Ok(()),
                    Some(id) => {
                        return Err(JsonLdError::CanNotDeserialize(format!(
                            "Unexpected value for @id: {}",
                            id
                        )))
                    }
                };
                let graph = node_map.get_mut(active_graph).unwrap();
                let node = match graph.entry(id) {
                    Entry::Vacant(e) => e.insert(NodeObject::new(id)),
                    Entry::Occupied(e) => e.into_mut(),
                };
                // Step 6.5
                if let ActiveSubject::Reversed(rid) = active_subject {
                    let active_subject = NodeObjectEntry::Id(rid);
                    let active_property = active_property.unwrap();
                    match node.properties.entry(active_property) {
                        Entry::Vacant(e) => {
                            e.insert(vec![active_subject]);
                        }
                        Entry::Occupied(mut e) => {
                            let values = e.get_mut();
                            if values.iter().all(|i| i != &active_subject) {
                                values.push(active_subject);
                            }
                        }
                    }
                }
                // Step 6.6
                else if let Some(active_property) = active_property {
                    let reference = NodeObjectEntry::Id(id);
                    match list {
                        None => {
                            let subject_node = match active_subject {
                                ActiveSubject::Id(id) => graph.get_mut(id).unwrap(),
                                _ => unreachable!("6.6. seems to imply that subject_node is a map"),
                            };
                            match subject_node.properties.entry(active_property) {
                                Entry::Vacant(e) => {
                                    e.insert(vec![reference]);
                                }
                                Entry::Occupied(mut e) => {
                                    let values = e.get_mut();
                                    if values.iter().all(|i| i != &reference) {
                                        values.push(reference);
                                    }
                                }
                            }
                        }
                        Some(list) => {
                            list.push(reference);
                        }
                    }
                }
                // NB: Step 6.6. above does not use 'node', but needs to borrow graph mutable;
                // by resetting node below, we allow Step 6.6 to drop the previous version of 'node'.
                let node = graph.get_mut(id).unwrap();
                // Step 6.7
                if !types.is_empty() {
                    if node.types.is_empty() {
                        node.types = types;
                    } else {
                        node.types.append(&mut types);
                        node.types.sort();
                        node.types.dedup();
                    }
                }
                // Step 6.8
                if let Some(index) = element.get("@index") {
                    if node.index.is_some() {
                        return Err(JsonLdError::ConflictingIndex);
                    }
                    node.index = Some(index);
                }
                // Step 6.9
                if let Some(reverse_map) = element.get("@reverse") {
                    let reverse_map = reverse_map.as_object().ok_or_else(|| {
                        JsonLdError::CanNotDeserialize(format!(
                            "@reverse should be map, found {}",
                            reverse_map
                        ))
                    })?;
                    let referenced_node = ActiveSubject::Reversed(id);
                    for (property, values) in reverse_map.iter() {
                        for value in all_items(values) {
                            populate_node_map(
                                value,
                                node_map,
                                active_graph,
                                referenced_node,
                                Some(property),
                                &mut None,
                                bnode_gen,
                            )?;
                        }
                    }
                }
                // Step 6.10
                if let Some(graph) = element.get("@graph") {
                    let graph = graph.as_array().ok_or_else(|| {
                        JsonLdError::CanNotDeserialize(format!(
                            "@graph should be an array, found {}",
                            graph
                        ))
                    })?;
                    if let Entry::Vacant(e) = node_map.entry(id) {
                        e.insert(Default::default());
                    };
                    for value in graph {
                        populate_node_map(
                            value,
                            node_map,
                            id,
                            ActiveSubject::Null,
                            None,
                            &mut None,
                            bnode_gen,
                        )?;
                    }
                }
                // Step 6.11
                if let Some(included) = element.get("@included") {
                    let included = included.as_array().ok_or_else(|| {
                        JsonLdError::CanNotDeserialize(format!(
                            "@included should be an array, found {}",
                            included
                        ))
                    })?;
                    for value in included {
                        populate_node_map(
                            value,
                            node_map,
                            active_graph,
                            ActiveSubject::Null,
                            None,
                            &mut None,
                            bnode_gen,
                        )?;
                    }
                }
                // Step 6.12
                for (property, value) in element.iter() {
                    if property.starts_with("@") && REMOVED.iter().any(|i| *i == &property[1..]) {
                        continue;
                    }
                    let property = if property.starts_with("_:") {
                        bnode_gen.get(property)
                    } else {
                        property
                    };
                    populate_node_map(
                        value,
                        node_map,
                        active_graph,
                        ActiveSubject::Id(id),
                        Some(property),
                        &mut None,
                        bnode_gen,
                    )?;
                }
            }
            Ok(())
        }
        x => Err(JsonLdError::CanNotDeserialize(format!(
            "Expected list or map, got {}",
            x
        ))),
    }
}

static REMOVED: [&str; 6] = ["id", "type", "index", "reverse", "graph", "included"];

#[derive(Clone, Copy, Debug)]
enum ActiveSubject<'a> {
    Null,
    Id(&'a str),
    Reversed(&'a str),
}
