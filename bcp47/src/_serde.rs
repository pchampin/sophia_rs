//! Serde implementation for LanguageTag

use std::borrow::Borrow;

use serde::{
    Deserialize, Serialize,
    de::{Error, Unexpected},
};

use crate::LanguageTag;

impl<'a, T: Borrow<str> + From<&'a str>> Deserialize<'a> for LanguageTag<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        let inner: &'a str = <&'a str>::deserialize(deserializer)?;
        Self::new(inner.into())
            .map_err(|err| D::Error::invalid_value(Unexpected::Str(&err.0), &"valid Language Tag"))
    }
}

impl<T: Borrow<str>> Serialize for LanguageTag<T> {
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
        tag: LanguageTag<String>,
    }

    #[derive(Serialize, Deserialize)]
    struct MyUncheckedTable {
        tag: &'static str,
    }

    #[test]
    fn valid_tag() {
        let data = MyUncheckedTable { tag: "fr-FR" };
        let toml_str = toml::to_string(&data).unwrap();
        let data2 = toml::from_str::<MyTable>(&toml_str).unwrap();
        assert_eq!(&data2.tag, &data2.tag);
    }

    #[test]
    fn invalid_tag() {
        let data = MyUncheckedTable { tag: "hello world" };
        let toml_str = toml::to_string(&data).unwrap();
        let data2 = toml::from_str::<MyTable>(&toml_str);
        assert!(data2.is_err());
    }
}
