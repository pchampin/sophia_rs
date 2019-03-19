// this module is transparently re-exported by its parent `term`
use std::hash::{Hash,Hasher};
use std::ops::Deref;

use super::*;

/// Internal representation of a blank node identifier.
/// 
/// May be encountered when pattern-matching on [`Term`](enum.Term.html)s
/// of the [`BNode`](enum.Term.html#variant.BNode) variant.
/// For that purpose, note that `BNodeId`
///  - derefs implicitly to its internal type `T`;
///  - can be directly compared to a `&str` with the `==` operator.
/// 
/// Example :
/// ```
/// use sophia::term::*;
/// 
/// fn is_foobar(t: BoxTerm) -> bool {
///     match t {
///         BNode(id) =>
///             id.starts_with("foo") || id == "bar",
///         _ =>
///             false,
///     }
/// }
/// ```
/// 
/// See [module documentation](index.html)
/// for more detail.
#[derive(Clone,Copy,Debug,Eq)]
pub struct BNodeId<T: AsRef<str>> {
    value: T,
    n3: bool,
}

impl<T> BNodeId<T> where
    T: AsRef<str>
{
    /// You would usually not use this constructor directly,
    /// but instead use [`Term::new_bnode`](enum.Term.html#method.new_bnode)
    /// or [`Term::new_bnode_unchecked`](enum.Term.html#method.new_bnode_unchecked).
    pub(crate) fn new(value: T) -> BNodeId<T> {
        let n3 = N3_BNODE_ID.is_match(value.as_ref());
        BNodeId{value, n3}
    }

    /// You would usually not use this constructor directly,
    /// but instead use [`Term::from_with`](enum.Term.html#method.from_with).
    pub(crate) fn from_with<'a, U, F> (other: &'a BNodeId<U>, mut factory: F) -> BNodeId<T> where
        U: AsRef<str>,
        F: FnMut(&'a str) -> T,
    {
        BNodeId{
            value: factory(other.value.as_ref()),
            n3: other.n3,
        }
    }

    /// Whether this blank node identifier is compatible with the N3 family
    /// of syntaxes (N-Triples, Turtle...).
    /// This can be useful for serializers.
    pub fn is_n3(&self) -> bool {
        self.n3
    }
}

impl<T> AsRef<str> for BNodeId<T> where
    T:AsRef<str>
{
    fn as_ref(&self) -> &str {
        &self.value.as_ref()
    }
}

impl<T> Deref for BNodeId<T> where
    T:AsRef<str>
{
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T, U> PartialEq<BNodeId<U>> for BNodeId<T> where
    T: AsRef<str>,
    U: AsRef<str>,
{
    fn eq(&self, other: &BNodeId<U>) -> bool {
        self.value.as_ref() == other.value.as_ref()
    }
}

impl<'a, T> PartialEq<&'a str> for BNodeId<T> where
    T: AsRef<str>,
{
    fn eq(&self, other: &&'a str) -> bool {
        self.value.as_ref() == *other
    }
}

impl<T> Hash for BNodeId<T> where
    T: AsRef<str>,
{
    fn hash<H:Hasher> (&self, state: &mut H) {
        self.value.as_ref().hash(state)
    }
}


lazy_static! {
    static ref N3_BNODE_ID: Regex = Regex::new(r"(?x)
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



#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::term::test module).
}