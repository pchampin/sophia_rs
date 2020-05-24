//! This module is transparently re-exported by its parent `lib`.
use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};

use super::*;

impl PartialEq for dyn TTerm {
    fn eq(&self, other: &dyn TTerm) -> bool {
        term_eq(self, other)
    }
}

impl Eq for dyn TTerm {}

impl PartialOrd for dyn TTerm {
    fn partial_cmp(&self, other: &dyn TTerm) -> Option<Ordering> {
        Some(term_cmp(self, other))
    }
}

impl Ord for dyn TTerm {
    fn cmp(&self, other: &dyn TTerm) -> Ordering {
        term_cmp(self, other)
    }
}

impl Hash for dyn TTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}

impl Display for dyn TTerm {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        term_format(self, fmt)
    }
}
