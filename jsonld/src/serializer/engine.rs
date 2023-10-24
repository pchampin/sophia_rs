use super::rdf_object::*;
use crate::error::*;
use crate::options::{ProcessingMode::*, *};
use crate::util_traits::*;
use json_syntax::object::Object;
use json_syntax::{Parse, Value as JsonValue};
use locspan::Meta;
use sophia_api::ns::rdf;
use sophia_api::quad::Quad;
use sophia_api::source::{QuadSource, SinkError, StreamResult};
use sophia_api::term::{Term, TermKind, TryFromTerm};
use std::collections::hash_map::Entry::*;
use std::collections::{HashMap, HashSet};

/// JSON-LD serializer engine
pub struct Engine<'a, L> {
    options: &'a JsonLdOptions<L>,
    // Map GsIds to index
    index: HashMap<(Box<str>, Box<str>), usize>,
    // Maps index to their graph_id (" " for default graph) and node_id
    gs_id: Vec<(Box<str>, Box<str>)>,
    // Attributes of each node (in default graph and named graphs)
    node: Vec<HashMap<Box<str>, Vec<RdfObject>>>,
    // Maps each graph_name index to its subjects indexes
    unique_parent: HashMap<Box<str>, Option<(usize, Box<str>)>>,
    // List seeds
    list_seeds: Vec<usize>,
    // Mark bnode ids as list node, and map to their index
    list_node: HashMap<Box<str>, usize>,
    // Mark index of bnode as compound literals
    compound_literals: HashSet<usize>,
}

impl<'a, L> Engine<'a, L> {
    /// Build a new conversion engine
    pub fn new_with_options(options: &'a JsonLdOptions<L>) -> Self {
        Engine {
            options,
            index: HashMap::new(),
            gs_id: Vec::new(),
            node: Vec::new(),
            unique_parent: HashMap::new(),
            list_seeds: Vec::new(),
            list_node: HashMap::new(),
            compound_literals: HashSet::new(),
        }
    }

    pub fn process_quads<QS>(&mut self, mut source: QS) -> StreamResult<(), QS::Error, JsonLdError>
    where
        QS: QuadSource,
    {
        match self.options.processing_mode() {
            JsonLd1_0 | JsonLd1_1 => (),
            #[allow(unreachable_patterns)]
            // in case futures versions of the jsonld crate implement other modes
            _ => {
                return Err(SinkError(JsonLdError::UnsupportedVersion(
                    self.options.processing_mode(),
                )));
            }
        }
        source.try_for_each_quad(|q| {
            if !q.is_jsonld() {
                return Ok(());
            }
            let g_id = q.g().map(|g| g.as_id()).unwrap_or_else(|| Box::from(" "));
            let s_id = q.s().as_id();
            let is = self.index(g_id.clone(), s_id.clone());
            if q.g().is_some() {
                let ig = self.index(" ".to_string(), g_id.clone());
                self.node[ig].push_if_new("@graph", RdfObject::Node(is, s_id));
            }
            let obj = self.make_rdf_object(q.o(), &g_id);
            let p = if rdf::type_ == q.p() && obj.is_iri() && !self.options.use_rdf_type() {
                Box::from("@type")
            } else {
                q.p().as_id()
            };
            self.node[is].push_if_new(p, obj);

            if q.s().is_bnode() {
                if rdf::rest == q.p() && rdf::nil == q.o() {
                    self.list_seeds.push_if_new(is);
                } else if self.options.rdf_direction() == Some(RdfDirection::CompoundLiteral)
                    && rdf::direction == q.p()
                {
                    self.compound_literals.insert(is);
                }
            }
            if q.o().is_bnode() {
                let parent = (is, q.p().as_id());
                match self.unique_parent.entry(q.o().as_id()) {
                    Vacant(e) => {
                        e.insert(Some(parent));
                    }
                    Occupied(mut e) => {
                        if let Some(p) = e.get() {
                            if p != &parent {
                                e.insert(None);
                            }
                        }
                    }
                }
            }
            Ok(())
        })
    }

    fn index<T: Into<Box<str>>, U: Into<Box<str>>>(&mut self, g_id: T, s_id: U) -> usize {
        let g_id: Box<str> = g_id.into();
        let s_id: Box<str> = s_id.into();
        match self.index.entry((g_id.clone(), s_id.clone())) {
            Vacant(e) => {
                let i = self.gs_id.len();
                e.insert(i);
                self.gs_id.push((g_id, s_id));
                self.node.push(HashMap::new());
                i
            }
            Occupied(e) => *e.get(),
        }
    }

    fn make_rdf_object<T>(&mut self, o: T, g_id: &str) -> RdfObject
    where
        T: Term,
    {
        match o.kind() {
            TermKind::Literal => RdfObject::try_from_term(o).unwrap(),
            _ => {
                let o_id = o.as_id();
                RdfObject::Node(self.index(g_id.to_string(), o_id.clone()), o_id)
            }
        }
    }

    /// Get the result as a JsonValue.
    pub fn into_json(mut self) -> Result<JsonValue<()>, JsonLdError> {
        // check all list_seeds to mark them, if appropriate, as list nodes,
        // and also recursively mark other list nodes (traversing back rdf:rest links)
        let list_seeds = std::mem::take(&mut self.list_seeds);
        for inode in list_seeds.into_iter() {
            self.mark_list_node(inode);
        }
        // check that candidate compound literals are indeed compound literels
        if self.options.rdf_direction() == Some(RdfDirection::CompoundLiteral) {
            let mut compound_literals = std::mem::take(&mut self.compound_literals);
            compound_literals.retain(|is| is_compound_literal(&self.node[*is]));
            self.compound_literals = compound_literals;
        }

        self.node
            .iter()
            .enumerate()
            .filter_map(|(inode, node)| self.jsonify(inode, node, true).transpose())
            .collect::<Result<Vec<_>, _>>()
            .map(Into::into)
    }

    /// If this node is a bnode with only 1 rdf:value & 1 rdf:rest),
    /// and it has a unique parent,
    /// then mark it as being a list node
    /// (i.e. it must not be rendered directly)
    fn mark_list_node(&mut self, inode: usize) {
        let (g_id, s_id) = &self.gs_id[inode];
        debug_assert!(s_id.starts_with("_:"), "{}", s_id);
        if let Some((iparent, pp)) = &self.unique_parent[s_id] {
            if self.options.processing_mode() == JsonLd1_0 && pp.as_ref() == RDF_FIRST {
                return;
            }
            // node 'gs_id' has a unique parent
            let (pg_id, ps_id) = &self.gs_id[*iparent];
            if pg_id == g_id {
                // unique parent is in the same graph
                let map = &mut self.node[inode];
                if is_list_node(map) {
                    // this node is indeed a list node
                    self.list_node.insert(s_id.clone(), *iparent);
                    if ps_id.starts_with("_:") && pp.as_ref() == RDF_REST {
                        let iparent = *iparent;
                        // the explicit copy of iparent above is required,
                        // to release the immutable borrow on self,
                        // so that we can mutably borrow self below
                        self.mark_list_node(iparent);
                    }
                }
            }
        }
    }

    fn jsonify(
        &self,
        inode: usize,
        node: &HashMap<Box<str>, Vec<RdfObject>>,
        root: bool,
    ) -> Result<Option<Meta<JsonValue<()>, ()>>, JsonLdError> {
        //println!("=== jsonify {}", inode);
        if node.is_empty() {
            //println!("=== skipped because empty");
            // this node appears only in the object position,
            // it has no outgoing arc
            return Ok(None);
        }
        let (g_id, s_id) = &self.gs_id[inode];
        //println!("=== --- {} | {}", g_id, s_id);
        if root && g_id.as_ref() != " " {
            //println!("=== skipped in root");
            // this node is described in a named graph,
            // we will include it later
            return Ok(None);
        }
        if self.list_node.contains_key(s_id)
            || (self.options.rdf_direction() == Some(RdfDirection::CompoundLiteral)
                && self.compound_literals.contains(&inode))
        {
            //println!("=== skiped (list node or compound literal)");
            return Ok(None);
        }
        //println!("=== --- doing it");
        let mut obj = self.make_node_object(s_id, node)?;
        if root {
            if let Some(ng) = node.get("@graph") {
                //println!("=== --- @graph for {}", s_id);
                push_entry(
                    &mut obj,
                    "@graph",
                    ng.iter()
                        .filter_map(|rdf_obj| match rdf_obj {
                            RdfObject::Node(inode2, _) => self
                                .jsonify(*inode2, &self.node[*inode2], false)
                                .transpose(),
                            _ => None,
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into(),
                );
            }
        }
        let obj = JsonValue::from(obj);
        Ok(Some(obj.into()))
    }

    fn make_node_object(
        &self,
        id: &str,
        node: &HashMap<Box<str>, Vec<RdfObject>>,
    ) -> Result<Object<()>, JsonLdError> {
        let mut obj = Object::new();
        push_entry(&mut obj, "@id", id.into());
        for (key, vals) in node.iter() {
            if key.as_ref() == "@graph" {
                continue;
            }
            let (key, vals) = if key.as_ref() == "@type" {
                let vals = vals
                    .iter()
                    .filter_map(|o| match o {
                        RdfObject::Node(_, nid) => Some(Meta::from(JsonValue::from(nid.as_ref()))),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();
                ("@type", vals)
            } else {
                let vals = vals
                    .iter()
                    .map(|o| self.convert_rdf_object(o))
                    .collect::<Result<Vec<_>, _>>()?;
                (key.as_ref(), vals)
            };
            push_entry(&mut obj, key, JsonValue::from(vals));
        }
        Ok(obj)
    }

    fn convert_rdf_object(&self, val: &RdfObject) -> Result<Meta<JsonValue<()>, ()>, JsonLdError> {
        let ret = match val {
            RdfObject::LangString(lex, tag) => {
                let value = JsonValue::from(lex.as_ref());
                let language = JsonValue::from(tag.as_str());
                json_syntax::json!({
                    "@value": value,
                    "@language": language,
                })
            }
            RdfObject::TypedLiteral(lex, dt) => {
                let txt = lex.as_ref();
                let dt_str = dt.as_str();
                let mut obj = Object::new();
                if self.options.use_native_types() {
                    if dt_str == XSD_INTEGER || dt_str == XSD_DOUBLE {
                        if let Ok(val) = txt.parse::<f64>() {
                            if let Ok(val) = JsonValue::try_from(val) {
                                push_entry(&mut obj, "@value", val);
                            }
                        }
                    } else if dt_str == XSD_BOOLEAN {
                        if let Ok(val) = txt.parse::<bool>() {
                            push_entry(&mut obj, "@value", val.into());
                        }
                    }
                }
                if self.options.rdf_direction() == Some(RdfDirection::I18nDatatype)
                    && dt_str.starts_with(NS_18N)
                {
                    let mut iter = dt_str[NS_18N.len()..].splitn(2, '_');
                    let tag = iter.next().unwrap();
                    let dir = iter.next();
                    push_entry(&mut obj, "@value", txt.into());
                    if !tag.is_empty() {
                        push_entry(&mut obj, "@language", tag.into());
                    }
                    if let Some(dir) = dir {
                        if !dir.is_empty() {
                            push_entry(&mut obj, "@direction", dir.into());
                        }
                    }
                }
                if dt_str == RDF_JSON {
                    let _json_value = JsonValue::parse_str(txt, |span| span)?;
                    // TODO Ideally, we should be able to strip the metadata like that:
                    //let json_value = json_value.map_metadata_recursively(|_| ());
                    // but because of a bug <https://github.com/timothee-haudebourg/json-syntax/issues/3>
                    // we must instead parse the value again, without trackingh location
                    let json_value = JsonValue::parse_str(txt, |_| ()).unwrap();
                    push_entry(&mut obj, "@value", json_value.0);
                    push_entry(&mut obj, "@type", "@json".into());
                }
                if obj.is_empty() {
                    // useNativeTypes is false, or conversion failed
                    push_entry(&mut obj, "@value", txt.to_string().into());
                    if dt_str != XSD_STRING {
                        push_entry(&mut obj, "@type", dt_str.into());
                    }
                }
                let obj = JsonValue::from(obj);
                obj.into()
            }
            RdfObject::Node(inode, id) => {
                if id.as_ref() == RDF_NIL {
                    json_syntax::json!({
                        "@list": [],
                    })
                } else if &id[..2] != "_:" {
                    // any other IRI, do not even check list_nodes or compound_literals
                    json_syntax::json!({
                        "@id": JsonValue::from(id.as_ref()),
                    })
                } else if self.list_node.contains_key(id) {
                    let mut list_items = Vec::new();
                    self.populate_list(&mut list_items, *inode)?;
                    json_syntax::json!({
                        "@list": JsonValue::from(list_items),
                    })
                } else if self.options.rdf_direction() == Some(RdfDirection::CompoundLiteral)
                    && self.compound_literals.contains(inode)
                {
                    let node = &self.node[*inode];
                    let mut obj: Meta<JsonValue<()>, ()> = json_syntax::json!({
                        "@value": JsonValue::from(node[RDF_VALUE][0].as_str()),
                        "@direction": JsonValue::from(node[RDF_DIRECTION][0].as_str()),
                    });
                    if let Some(tags) = node.get(RDF_LANGUAGE) {
                        let obj = obj.as_object_mut().unwrap();
                        push_entry(obj, "@language", tags[0].as_str().into());
                    }
                    obj
                } else {
                    // any other blank node
                    json_syntax::json!({
                        "@id": JsonValue::from(id.as_ref()),
                    })
                }
            }
        };
        Ok(ret)
    }

    fn populate_list(
        &self,
        list_items: &mut Vec<Meta<JsonValue<()>, ()>>,
        inode: usize,
    ) -> Result<(), JsonLdError> {
        //println!("=== populate_list {}", gs_id);
        let map = &self.node[inode];
        list_items.push(self.convert_rdf_object(&map[RDF_FIRST][0])?);
        if let RdfObject::Node(inext, id) = &map[RDF_REST][0] {
            if id.as_ref() != RDF_NIL {
                self.populate_list(list_items, *inext)?;
            }
        }
        Ok(())
    }
}

// check if node is a list map (bnode w/ exactly 1 rdf:first and 1 rdf:rest, possibly a rdf:List)
// IMPORTANT: for this to be accurate, it must also hold that rdf:rest points to a list node,
// but this function is only called in situations where this is true
fn is_list_node(node: &HashMap<Box<str>, Vec<RdfObject>>) -> bool {
    2 <= node.len()
        && node.len() <= 3
        && node.get(RDF_FIRST).map(|v| v.len() == 1).unwrap_or(false)
        && node
            .get(RDF_REST)
            .map(|v| v.len() == 1 && v[0].is_node())
            .unwrap_or(false)
        && (node.len() == 2
            || node
                .get("@type")
                .map(|v| v.len() == 1 && v[0].eq_node(RDF_LIST))
                .unwrap_or(false))
}

// check if node is a compound literal
fn is_compound_literal(node: &HashMap<Box<str>, Vec<RdfObject>>) -> bool {
    2 <= node.len()
        && node.len() <= 3
        && node
            .get(RDF_DIRECTION)
            .map(|v| v.len() == 1 && v[0].is_literal())
            .unwrap_or(false)
        && node
            .get(RDF_VALUE)
            .map(|v| v.len() == 1 && v[0].is_literal())
            .unwrap_or(false)
        && (node.len() == 2
            || node
                .get(RDF_LANGUAGE)
                .map(|v| v.len() == 1 && v[0].is_literal())
                .unwrap_or(false))
}

const NS_18N: &str = "https://www.w3.org/ns/i18n#";
const RDF_DIRECTION: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#direction";
const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
const RDF_JSON: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#JSON";
const RDF_LANGUAGE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#language";
const RDF_LIST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#List";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const RDF_VALUE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#value";
const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";
const XSD_DOUBLE: &str = "http://www.w3.org/2001/XMLSchema#double";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

fn push_entry(obj: &mut Object<()>, key: &str, value: JsonValue<()>) -> bool {
    obj.push(Meta::new(key.into(), ()), value.into())
}
