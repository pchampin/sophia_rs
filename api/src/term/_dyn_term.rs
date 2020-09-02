//! Implementations on trait objects of `TTerm`.
//!
//! The `'a` lifetimes are required because Rust assumes by default that each
//! `dyn TTerm` instance is actually `dyn TTerm + 'static` so for example
//! without the `'a` Rust requires a `'static` borrow if we want to calculate
//! the hash of a `dyn TTerm` object.

use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::{Hash, Hasher};

use super::*;

impl<'a> PartialEq for dyn TTerm + 'a {
    fn eq(&self, other: &(dyn TTerm + 'a)) -> bool {
        term_eq(self, other)
    }
}

impl<'a> Eq for dyn TTerm + 'a {}

impl<'a> PartialOrd for dyn TTerm + 'a {
    fn partial_cmp(&self, other: &(dyn TTerm + 'a)) -> Option<Ordering> {
        Some(term_cmp(self, other))
    }
}

impl<'a> Ord for dyn TTerm + 'a {
    fn cmp(&self, other: &(dyn TTerm + 'a)) -> Ordering {
        term_cmp(self, other)
    }
}

impl<'a> Hash for dyn TTerm + 'a {
    fn hash<H: Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}

impl<'a> Display for dyn TTerm + 'a {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        term_format(self, fmt)
    }
}
