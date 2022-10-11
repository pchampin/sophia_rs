use json::JsonValue;
use std::collections::BTreeMap;

/// A GraphName-to-nodes map
pub type NodeMap<'a> = BTreeMap<&'a str, NamedGraph<'a>>;

/// A Id-to-node map
pub type NamedGraph<'a> = BTreeMap<&'a str, NodeObject<'a>>;

/// A Property-to-values map
#[derive(Clone, Debug)]
pub struct NodeObject<'a> {
    #[allow(dead_code)]
    pub(crate) id: &'a str,
    pub(crate) types: Vec<&'a str>,
    pub(crate) index: Option<&'a JsonValue>,
    pub(crate) properties: BTreeMap<&'a str, Vec<NodeObjectEntry<'a>>>,
}

impl<'a> NodeObject<'a> {
    pub fn new(id: &'a str) -> Self {
        NodeObject {
            id,
            types: vec![],
            index: None,
            properties: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NodeObjectEntry<'a> {
    Id(&'a str),
    List(Vec<NodeObjectEntry<'a>>),
    Value(&'a JsonValue), // JsonValue MUST be an Object
}
