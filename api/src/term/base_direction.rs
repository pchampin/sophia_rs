//! I define the [`BaseDirection`] enum.

use std::{fmt::Display, str::FromStr};

use super::{SimpleTerm, language_tag::I18nString};

/// A datatype capturing the notion of [base direction](https://www.w3.org/TR/rdf12-concepts/#section-text-direction)
/// defined by RDF 1.2.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum BaseDirection {
    /// Left-to-right
    Ltr,
    /// Right-to-left
    Rtl,
}

impl BaseDirection {
    /// Textual representation of this [`BaseDirection`]
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Ltr => "ltr",
            Self::Rtl => "rtl",
        }
    }
}

impl Display for BaseDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str(),)
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

impl<'a> std::ops::Mul<BaseDirection> for I18nString<&'a str> {
    type Output = SimpleTerm<'a>;

    fn mul(self, rhs: BaseDirection) -> Self::Output {
        let I18nString { value, language } = self;
        let lex = value.into();
        let tag = language.map_unchecked(Into::into);
        SimpleTerm::LiteralLanguage(lex, tag, Some(rhs))
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

#[cfg(feature = "serde")]
mod _serde {
    use super::*;
    use serde::{
        Deserialize, Serialize,
        de::{Error, Unexpected},
    };

    impl<'a> Deserialize<'a> for BaseDirection {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'a>,
        {
            let inner = <&str>::deserialize(deserializer)?;
            BaseDirection::from_str(inner).map_err(|_| {
                D::Error::invalid_value(Unexpected::Str(inner), &"valid BaseDirection")
            })
        }
    }

    impl Serialize for BaseDirection {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            self.as_str().serialize(serializer)
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[derive(Serialize, Deserialize)]
        struct MyTable {
            dir: BaseDirection,
        }

        #[derive(Serialize, Deserialize)]
        struct MyUncheckedTable {
            dir: String,
        }

        #[test]
        fn valid_prefix() {
            let data = MyUncheckedTable { dir: "ltr".into() };
            let toml_str = toml::to_string(&data).unwrap();
            let data2 = toml::from_str::<MyTable>(&toml_str).unwrap();
            assert_eq!(data.dir, data2.dir.as_str());
        }

        #[test]
        fn invalid_prefix() {
            let data = MyUncheckedTable { dir: "f o".into() };
            let toml_str = toml::to_string(&data).unwrap();
            let data2 = toml::from_str::<MyTable>(&toml_str);
            assert!(data2.is_err());
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
