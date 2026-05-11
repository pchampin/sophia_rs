//! Serde implementation for LanguageTag

use std::borrow::Borrow;

use serde::{
    Deserialize, Serialize,
    de::{Error, Unexpected},
};

use crate::LanguageTag;

impl<'de, T: Borrow<str> + Deserialize<'de>> Deserialize<'de> for LanguageTag<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let inner = T::deserialize(deserializer)?;
        Self::new(inner)
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
    fn valid_tag_str() {
        let data = MyUncheckedTable { tag: "fr-FR" };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_str::<MyTable>(&json_str).unwrap();
        assert_eq!(&data2.tag, &data2.tag);
    }

    #[test]
    fn valid_tag_reader() {
        let data = MyUncheckedTable { tag: "fr-FR" };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes()).unwrap();
        assert_eq!(&data2.tag, &data2.tag);
    }

    #[test]
    fn invalid_tag_str() {
        let data = MyUncheckedTable { tag: "hello world" };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_str::<MyTable>(&json_str);
        assert!(data2.is_err());
    }

    #[test]
    fn invalid_tag_reader() {
        let data = MyUncheckedTable { tag: "hello world" };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes());
        assert!(data2.is_err());
    }
}
