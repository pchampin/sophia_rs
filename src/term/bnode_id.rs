// this module is transparently re-exported by its parent `term`
use std::hash::{Hash,Hasher};

use super::*;

#[derive(Clone,Debug,Eq)]
pub struct BNodeId<T: Borrow<str>> {
    value: T,
    n3: bool,
}

impl<T> BNodeId<T> where
    T: Borrow<str>
{
    pub fn new(value: T) -> BNodeId<T> {
        let n3 = N3_BNODE_ID.is_match(value.borrow());
        BNodeId{value, n3}
    }

    pub fn from_with<'a, U, F> (other: &'a BNodeId<U>, mut factory: F) -> BNodeId<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        BNodeId{
            value: factory(other.value.borrow()),
            n3: other.n3,
        }
    }
}

impl<T> Borrow<str> for BNodeId<T> where
    T:Borrow<str>
{
    fn borrow(&self) -> &str {
        self.value.borrow()
    }
}

impl<T, U> PartialEq<BNodeId<U>> for BNodeId<T> where
    T: Borrow<str>,
    U: Borrow<str>,
{
    fn eq(&self, other: &BNodeId<U>) -> bool {
        self.value.borrow() == other.value.borrow()
    }
}

impl<T> Hash for BNodeId<T> where
    T: Borrow<str>,
{
    fn hash<H:Hasher> (&self, state: &mut H) {
        self.value.borrow().hash(state)
    }
}


lazy_static! {
    pub static ref N3_BNODE_ID: Regex = Regex::new(r"(?x)
      ^
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9]
      (
          [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
          |
          \u{2e} [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
      )*
      $
    ").unwrap();
}