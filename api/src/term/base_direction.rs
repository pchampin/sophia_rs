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

    fn from_bytes(s: &[u8]) -> Result<Self, ()> {
        match s {
            b"ltr" => Ok(Self::Ltr),
            b"rtl" => Ok(Self::Rtl),
            _ => Err(()),
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
        Self::from_bytes(s.as_bytes())
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
        de::{Error, Unexpected, Visitor},
    };

    struct BaseDirectionVisitor;

    impl<'de> Visitor<'de> for BaseDirectionVisitor {
        type Value = BaseDirection;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str(VISITOR_MSG)
        }

        fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
        where
            E: Error,
        {
            BaseDirection::from_str(v)
                .map_err(|_| Error::invalid_value(Unexpected::Str(v), &VISITOR_MSG))
        }

        fn visit_bytes<E>(self, v: &[u8]) -> std::result::Result<Self::Value, E>
        where
            E: Error,
        {
            BaseDirection::from_bytes(v)
                .map_err(|_| Error::invalid_value(Unexpected::Bytes(v), &VISITOR_MSG))
        }
    }

    static VISITOR_MSG: &str = r#"either "ltr" or "rtl""#;

    impl<'a> Deserialize<'a> for BaseDirection {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'a>,
        {
            deserializer.deserialize_str(BaseDirectionVisitor)
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
        fn valid_base_dir_str() {
            let data = MyUncheckedTable { dir: "ltr".into() };
            let json_str = serde_json::to_string(&data).unwrap();
            let data2 = serde_json::from_str::<MyTable>(&json_str).unwrap();
            assert_eq!(data.dir, data2.dir.as_str());
        }

        #[test]
        fn valid_base_dir_reader() {
            let data = MyUncheckedTable { dir: "ltr".into() };
            let json_str = serde_json::to_string(&data).unwrap();
            let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes()).unwrap();
            assert_eq!(data.dir, data2.dir.as_str());
        }

        #[test]
        fn invalid_base_dir_str() {
            let data = MyUncheckedTable { dir: "f o".into() };
            let json_str = serde_json::to_string(&data).unwrap();
            let data2 = serde_json::from_str::<MyTable>(&json_str);
            assert!(data2.is_err());
        }

        #[test]
        fn invalid_base_dir_reader() {
            let data = MyUncheckedTable { dir: "f o".into() };
            let json_str = serde_json::to_string(&data).unwrap();
            let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes());
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
