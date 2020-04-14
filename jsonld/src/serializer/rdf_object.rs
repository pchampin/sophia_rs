//! A private enum type used internally by JsonLdSerializer
use sophia_term::literal::Literal;
use sophia_term::TermData;

#[derive(Clone, Debug, PartialEq)]
pub enum RdfObject {
    Literal(Literal<Box<str>>),
    Node(usize, String),
}

impl RdfObject {
    pub fn is_literal(&self) -> bool {
        matches!(self, RdfObject::Literal(_))
    }
    pub fn is_node(&self) -> bool {
        matches!(self, RdfObject::Node(..))
    }
    pub fn eq_node(&self, other_id: &str) -> bool {
        matches!(self, RdfObject::Node(_, id) if id==other_id)
    }
    pub fn as_str(&self) -> &str {
        match self {
            RdfObject::Literal(lit) => lit.txt().as_ref(),
            RdfObject::Node(_, id) => &id,
        }
    }
}

impl<T: TermData + Into<Box<str>>> From<Literal<T>> for RdfObject {
    fn from(other: Literal<T>) -> Self {
        RdfObject::Literal(other.map_into())
    }
}

impl From<(usize, String)> for RdfObject {
    fn from(other: (usize, String)) -> Self {
        RdfObject::Node(other.0, other.1)
    }
}
