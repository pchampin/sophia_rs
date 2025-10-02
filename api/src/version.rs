//! Define [`Version`]

/// Represent the version labels defined in <https://www.w3.org/TR/rdf12-concepts/#defined-version-labels>. b
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub enum Version {
    /// RDF 1.1 syntax except for use of a version directive
    Rdf11,
    /// RDF 1.2 syntax without triple terms
    Rdf12Basic,
    /// RDF 1.2 syntax
    #[default]
    Rdf12,
}

impl std::str::FromStr for Version {
    type Err = UnrecognizedVersion;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1.1" => Ok(Self::Rdf11),
            "1.2-basic" => Ok(Self::Rdf12Basic),
            "1.2" => Ok(Self::Rdf12),
            txt => Err(UnrecognizedVersion(txt.into())),
        }
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Version::Rdf11 => f.write_str("1.1"),
            Version::Rdf12Basic => f.write_str("1.2-basic"),
            Version::Rdf12 => f.write_str("1.2"),
        }
    }
}

/// Error raised when a version specifier can not be parsed.
#[derive(Clone, Debug)]
pub struct UnrecognizedVersion(String);

impl std::fmt::Display for UnrecognizedVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UnrecognizedVersion({})", self.0)
    }
}

impl std::error::Error for UnrecognizedVersion {}

#[cfg(test)]
mod test {
    use super::*;

    use test_case::test_case;

    #[test_case(Version::Rdf11)]
    #[test_case(Version::Rdf12Basic)]
    #[test_case(Version::Rdf12)]
    fn round_trip(v: Version) {
        assert_eq!(v, v.to_string().parse().unwrap())
    }
}
