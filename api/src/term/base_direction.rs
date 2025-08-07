//! I define the [`BaseDirection`] enum.

use std::{fmt::Display, str::FromStr};

use super::SimpleTerm;

/// A datatype capturing the notion of [base direction](https://www.w3.org/TR/rdf12-concepts/#section-text-direction)
/// defined by RDF 1.2.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum BaseDirection {
    /// Left-to-right
    Ltr,
    /// Right-to-left
    Rtl,
}

impl Display for BaseDirection {
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

impl FromStr for BaseDirection {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ltr" => Ok(Self::Ltr),
            "rtl" => Ok(Self::Rtl),
            _ => Err(()),
        }
    }
}

impl<'a> std::ops::Mul<BaseDirection> for SimpleTerm<'a> {
    type Output = SimpleTerm<'a>;

    fn mul(self, rhs: BaseDirection) -> Self::Output {
        if let SimpleTerm::LiteralLanguage(lex, tag, None) = self {
            SimpleTerm::LiteralLanguage(lex, tag, Some(rhs))
        } else {
            log::warn!("Ignoring multiplication by base-dir for {self:?}");
            self
        }
    }
}

#[cfg(test)]
mod test {
    use mownstr::MownStr;

    use super::*;
    use crate::term::{IriRef, LanguageTag};

    #[test]
    fn product() {
        let tag = LanguageTag::new_unchecked("en");
        let lit1 = "hello" * tag * BaseDirection::Ltr;
        let lit2 = SimpleTerm::LiteralLanguage(
            "hello".into(),
            tag.map_unchecked(MownStr::from),
            Some(BaseDirection::Ltr),
        );
        assert_eq!(lit1, lit2);
    }

    #[test]
    fn product_noop_iri() {
        let lit1 = SimpleTerm::Iri(IriRef::new_unchecked(MownStr::from("x:a")));
        let lit2 = lit1.clone() * BaseDirection::Ltr;
        assert_eq!(lit1, lit2);
    }

    #[test]
    fn product_noop_dir_lang_string() {
        let tag = LanguageTag::new_unchecked("en");
        let lit1 = SimpleTerm::LiteralLanguage(
            "hello".into(),
            tag.map_unchecked(MownStr::from),
            Some(BaseDirection::Ltr),
        );
        let lit2 = lit1.clone() * BaseDirection::Rtl;
        assert_eq!(lit1, lit2);
    }
}
