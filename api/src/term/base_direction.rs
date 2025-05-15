//! I define the [`BaseDir`] enum.

use std::{fmt::Display, str::FromStr};

/// A datatype capturing the notion of [base direction](https://www.w3.org/TR/rdf12-concepts/#section-text-direction)
/// defined by RDF 1.2.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum BaseDir {
    /// Left-to-right
    Ltr,
    /// Right-to-left
    Rtl,
}

impl Display for BaseDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ltr => "ltr",
                Self::Rtl => "rtl",
            }
        )
    }
}

impl FromStr for BaseDir {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ltr" => Ok(Self::Ltr),
            "rtl" => Ok(Self::Rtl),
            _ => Err(()),
        }
    }
}
