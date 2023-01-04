use super::*;
use std::borrow::Borrow;

/// Marker trait guaranteeing that the underlying `str` is a valid Turtle/SPARQL prefix.
pub trait IsPrefix: Borrow<str> {}

//

/// Automatic trait for [`IsPrefix`], providing cheap conversion to [`Prefix`].
pub trait AsPrefix {
    /// Extract an [`Prefix`] wrapping the underlying `str`.
    fn as_prefix(&self) -> Prefix<&str>;
}

impl<T: IsPrefix> AsPrefix for T {
    fn as_prefix(&self) -> Prefix<&str> {
        Prefix::new_unchecked(self.borrow())
    }
}

#[cfg(test)]
#[allow(clippy::unused_unit)] // test_case! generated warnings
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case(""; "empty")]
    #[test_case("a")]
    #[test_case("foo")]
    #[test_case("é.hê"; "with dot and accents")]
    fn valid_prefix(p: &str) {
        assert!(is_valid_prefix(p));
        assert!(Prefix::new(p).is_ok());
    }

    #[test_case(" "; "space")]
    #[test_case("1a")]
    #[test_case("a."; "ending with dot")]
    fn invalid_prefix(p: &str) {
        assert!(!is_valid_prefix(p));
        assert!(Prefix::new(p).is_err());
    }
}
