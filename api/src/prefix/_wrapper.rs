use super::*;
use std::borrow::Borrow;

sophia_iri::wrap! { Prefix borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `PN_PREFIX?` rule in Turtle/SPARQL.
    pub fn new(prefix: T) -> Result<Self, InvalidPrefix> {
        if is_valid_prefix(prefix.borrow()) {
            Ok(Prefix(prefix))
        } else {
            Err(InvalidPrefix(prefix.borrow().to_string()))
        }
    }

    /// Gets a reference to the underlying &str.
    pub fn as_str(&self) -> &str {
        self.0.borrow()
    }
}

impl<T: Borrow<str>> IsPrefix for Prefix<T> {}
