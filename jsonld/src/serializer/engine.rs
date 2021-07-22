use super::rdf_object::*;
use crate::config::{JsonLdSpecVersion::*, *};
use crate::error::*;
use crate::util_traits::*;
use json::object::Object;
use json::JsonValue;
use sophia_api::ns::{rdf, xsd};
use sophia_api::quad::{stream::*, Quad};
use sophia_api::term::{TTerm, TermKind, TryCopyTerm};
use sophia_api::triple::stream::{SinkError, StreamResult};
use sophia_term::literal::Literal;
use std::collections::hash_map::Entry::*;
use std::collections::{HashMap, HashSet};

/// JSON-LD serializer engine
pub struct Engine {
    config: JsonLdConfig,
    // Map GsIds to index
    index: HashMap<(String, String), usize>,
    // Maps index to their graph_id (" " for default graph) and node_id
    gs_id: Vec<(String, String)>,
    // Attributes of each node (in default graph and named graphs)
    node: Vec<HashMap<String, Vec<RdfObject>>>,
    // Maps each graph_name index to its subjects indexes
    unique_parent: HashMap<String, Option<(usize, String)>>,
    // List seeds
    list_seeds: Vec<usize>,
    // Mark bnode ids as list node, and map to their index
    list_node: HashMap<String, usize>,
    // Mark index of bnode as compound literals
    compound_literals: HashSet<usize>,
}

impl Engine {
    /// Build a new JSON-LD serializer writing to `write`, with the given config.
    pub fn new_with_config(config: JsonLdConfig) -> Engine {
        Engine {
            config,
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
        if self.config.spec_version > JsonLd11 {
            return Err(SinkError(JsonLdError::UnsupportedVersion(
                self.config.spec_version,
            )));
        }
        source.try_for_each_quad(|q| {
            if !q.is_jsonld() {
                return Ok(());
            }
            let g_id = q.g().map(|g| g.as_id()).unwrap_or_else(|| " ".to_string());
            let s_id = q.s().as_id();
            let is = self.index(g_id.clone(), s_id.clone());
            if q.g().is_some() {
                let ig = self.index(" ".to_string(), g_id.clone());
                self.node[ig].push_if_new("@graph".to_string(), RdfObject::Node(is, s_id));
            }
            let obj = self.make_rdf_object(q.o(), &g_id);
            self.node[is].push_if_new(q.p().as_id(), obj);

            if q.s().is_bnode() {
                if &rdf::rest == q.p() && &rdf::nil == q.o() {
                    self.list_seeds.push_if_new(is);
                } else if self.config.rdf_direction == Some(RdfDirectionMode::CompoundLiteral)
                    && &rdf::direction == q.p()
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

    fn index(&mut self, g_id: String, s_id: String) -> usize {
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

    fn make_rdf_object<T>(&mut self, o: &T, g_id: &str) -> RdfObject
    where
        T: TTerm + ?Sized,
    {
        match o.kind() {
            TermKind::Literal => RdfObject::Literal(Literal::try_copy(o).unwrap()),
            _ => {
                let o_id = o.as_id();
                RdfObject::Node(self.index(g_id.to_string(), o_id.clone()), o_id)
            }
        }
    }

    /// Get the result as a JsonValue.
    pub fn into_json(mut self) -> Result<JsonValue, JsonLdError> {
        // check all list_seeds to mark them, if appropriate, as list nodes,
        // and also recursively mark other list nodes (traversing back rdf:rest links)
        let list_seeds = std::mem::take(&mut self.list_seeds);
        for inode in list_seeds.into_iter() {
            self.mark_list_node(inode);
        }
        // check that candidate compound literals are indeed compound literels
        if self.config.rdf_direction == Some(RdfDirectionMode::CompoundLiteral) {
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
            if self.config.spec_version == JsonLd10 && pp == RDF_FIRST {
                return;
            }
            // node 'gs_id' has a unique parent
            let (pg_id, ps_id) = &self.gs_id[*iparent];
            if pg_id == g_id {
                // unique parent is in the same graph
                let map = &mut self.node[inode];
                if is_list_node(map) {
                    // this node is indeed a list node
                    self.list_node.insert(s_id.to_string(), *iparent);
                    if ps_id.starts_with("_:") && pp == RDF_REST {
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
        node: &HashMap<String, Vec<RdfObject>>,
        root: bool,
    ) -> Result<Option<JsonValue>, JsonLdError> {
        //println!("=== jsonify {}", inode);
        if node.is_empty() {
            //println!("=== skipped because empty");
            // this node appears only in the object position,
            // it has no outgoing arc
            return Ok(None);
        }
        let (g_id, s_id) = &self.gs_id[inode];
        //println!("=== --- {} | {}", g_id, s_id);
        if root && g_id != " " {
            //println!("=== skipped in root");
            // this node is described in a named graph,
            // we will include it later
            return Ok(None);
        }
        if self.list_node.contains_key(s_id)
            || (self.config.rdf_direction == Some(RdfDirectionMode::CompoundLiteral)
                && self.compound_literals.contains(&inode))
        {
            //println!("=== skiped (list node or compound literal)");
            return Ok(None);
        }
        //println!("=== --- doing it");
        let mut obj = self.make_node_object(&s_id, node)?;
        if root {
            if let Some(ng) = node.get("@graph") {
                //println!("=== --- @graph for {}", s_id);
                obj["@graph"] = ng
                    .iter()
                    .filter_map(|rdf_obj| match rdf_obj {
                        RdfObject::Node(inode2, _) => self
                            .jsonify(*inode2, &self.node[*inode2], false)
                            .transpose(),
                        _ => None,
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into();
            }
        }
        Ok(Some(obj.into()))
    }

    fn make_node_object(
        &self,
        id: &str,
        node: &HashMap<String, Vec<RdfObject>>,
    ) -> Result<Object, JsonLdError> {
        let mut obj = Object::new();
        obj.insert("@id", id.into());
        for (key, vals) in node.iter() {
            if key == "@graph" {
                continue;
            }
            let (key, vals) = if !self.config.use_rdf_type && key == RDF_TYPE {
                let vals = vals
                    .iter()
                    .filter_map(|o| match o {
                        RdfObject::Literal(_) => None,
                        RdfObject::Node(_, nid) => Some(JsonValue::from(nid.as_str())),
                    })
                    .collect::<Vec<_>>();
                ("@type", vals)
            } else {
                let vals = vals
                    .iter()
                    .map(|o| self.convert_rdf_object(o))
                    .collect::<Result<Vec<_>, _>>()?;
                (key.as_str(), vals)
            };
            obj.insert(key, JsonValue::from(vals));
        }
        Ok(obj)
    }

    fn convert_rdf_object(&self, val: &RdfObject) -> Result<JsonValue, JsonLdError> {
        let ret = match val {
            RdfObject::Literal(lit) => {
                let value = JsonValue::from(lit.txt().as_ref());
                match lit.lang() {
                    Some(tag) => {
                        json::object! {
                            "@value": value,
                            "@language": JsonValue::from(tag.as_ref()),
                        }
                    }
                    None => {
                        let txt = lit.txt().as_ref();
                        let dt = lit.dt();
                        let dt_str = dt.value();
                        let mut obj = Object::new();
                        if self.config.use_native_types {
                            if dt == xsd::integer || dt == xsd::double {
                                if let Ok(val) = txt.parse::<f64>() {
                                    obj.insert("@value", val.into());
                                }
                            } else if dt == xsd::boolean {
                                if let Ok(val) = txt.parse::<bool>() {
                                    obj.insert("@value", val.into());
                                }
                            }
                        }
                        if self.config.rdf_direction == Some(RdfDirectionMode::I18nDatatype)
                            && dt_str.starts_with(NS_18N)
                        {
                            let mut iter = dt_str[NS_18N.len()..].splitn(2, '_');
                            let tag = iter.next().unwrap();
                            let dir = iter.next();
                            obj.insert("@value", txt.into());
                            if !tag.is_empty() {
                                obj.insert("@language", tag.into());
                            }
                            if let Some(dir) = dir {
                                if !dir.is_empty() {
                                    obj.insert("@direction", dir.into());
                                }
                            }
                        }
                        if dt == rdf::JSON {
                            let json_value = json::parse(&txt)?;
                            obj.insert("@value", json_value);
                            obj.insert("@type", "@json".into());
                        }
                        if obj.is_empty() {
                            // useNativeTypes is false, or conversion failed
                            obj.insert("@value", txt.to_string().into());
                            if dt != xsd::string {
                                obj.insert("@type", dt.value().to_string().into());
                            }
                        }
                        obj.into()
                    }
                }
            }
            RdfObject::Node(inode, id) => {
                if id == RDF_NIL {
                    json::object! {
                        "@list": [],
                    }
                } else if &id[..2] != "_:" {
                    // any other IRI, do not even check list_nodes or compound_literals
                    json::object! {
                        "@id": id.as_str(),
                    }
                } else if self.list_node.contains_key(id) {
                    let mut list_items = Vec::new();
                    self.populate_list(&mut list_items, *inode)?;
                    json::object! {
                        "@list": list_items,
                    }
                } else if self.config.rdf_direction == Some(RdfDirectionMode::CompoundLiteral)
                    && self.compound_literals.contains(inode)
                {
                    let node = &self.node[*inode];
                    let mut obj = json::object! {
                        "@value": node[RDF_VALUE][0].as_str(),
                        "@direction": node[RDF_DIRECTION][0].as_str(),
                    };
                    if let Some(tags) = node.get(RDF_LANGUAGE) {
                        obj["@language"] = tags[0].as_str().into();
                    }
                    obj
                } else {
                    // any other blank node
                    json::object! {
                        "@id": id.as_str(),
                    }
                }
            }
        };
        Ok(ret)
    }

    fn populate_list(
        &self,
        list_items: &mut Vec<JsonValue>,
        inode: usize,
    ) -> Result<(), JsonLdError> {
        //println!("=== populate_list {}", gs_id);
        let map = &self.node[inode];
        list_items.push(self.convert_rdf_object(&map[RDF_FIRST][0])?);
        if let RdfObject::Node(inext, id) = &map[RDF_REST][0] {
            if id != RDF_NIL {
                self.populate_list(list_items, *inext)?;
            }
        }
        Ok(())
    }
}

// check if node is a list map (bnode w/ exactly 1 rdf:first and 1 rdf:rest, possibly a rdf:List)
// IMPORTANT: for this to be accurate, it must also hold that rdf:rest points to a list node,
// but this function is only called in situations where this is true
fn is_list_node(node: &HashMap<String, Vec<RdfObject>>) -> bool {
    2 <= node.len()
        && node.len() <= 3
        && node.get(RDF_FIRST).map(|v| v.len() == 1).unwrap_or(false)
        && node
            .get(RDF_REST)
            .map(|v| v.len() == 1 && v[0].is_node())
            .unwrap_or(false)
        && (node.len() == 2
            || node
                .get(RDF_TYPE)
                .map(|v| v.len() == 1 && v[0].is_node() && v[0].eq_node(&RDF_LIST))
                .unwrap_or(false))
}

// check if node is a compound literal
fn is_compound_literal(node: &HashMap<String, Vec<RdfObject>>) -> bool {
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
const RDF_LANGUAGE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#language";
const RDF_LIST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#List";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
const RDF_VALUE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#value";
