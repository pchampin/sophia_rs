use super::{Iri, IriRef, Result};
use serde::{
    Deserialize, Serialize,
    de::{Error, Unexpected},
};
use std::borrow::Borrow;

impl<'de, T: Borrow<str> + Deserialize<'de>> Deserialize<'de> for Iri<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let inner = T::deserialize(deserializer)?;
        Self::new(inner)
            .map_err(|err| D::Error::invalid_value(Unexpected::Str(&err.0), &"valid IRI"))
    }
}

impl<T: Borrow<str>> Serialize for Iri<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

impl<'a, T: Borrow<str> + Deserialize<'a>> Deserialize<'a> for IriRef<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        let inner: T = T::deserialize(deserializer)?;
        Self::new(inner)
            .map_err(|err| D::Error::invalid_value(Unexpected::Str(&err.0), &"valid IRI reference"))
    }
}

impl<T: Borrow<str>> Serialize for IriRef<T> {
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
        iri: Option<Iri<String>>,
        iriref: Option<IriRef<String>>,
    }

    #[derive(Serialize, Deserialize)]
    struct MyUncheckedTable {
        iri: Option<&'static str>,
        iriref: Option<&'static str>,
    }

    #[test]
    fn valid_iri_str() {
        let data = MyUncheckedTable {
            iri: Some("http://example.org/"),
            iriref: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_str::<MyTable>(&json_str).unwrap();
        assert_eq!(data.iri.unwrap(), data2.iri.unwrap().unwrap(),);
    }

    #[test]
    fn valid_iri_reader() {
        let data = MyUncheckedTable {
            iri: Some("http://example.org/"),
            iriref: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes()).unwrap();
        assert_eq!(data.iri.unwrap(), data2.iri.unwrap().unwrap(),);
    }

    #[test]
    fn invalid_iri_str() {
        let data = MyUncheckedTable {
            iri: Some("#foo"),
            iriref: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_str::<MyTable>(&json_str);
        assert!(data2.is_err());
    }

    #[test]
    fn invalid_iri_reader() {
        let data = MyUncheckedTable {
            iri: Some("#foo"),
            iriref: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes());
        assert!(data2.is_err());
    }

    #[test]
    fn valid_iriref_str() {
        let data = MyUncheckedTable {
            iriref: Some("#foo"),
            iri: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_str::<MyTable>(&json_str).unwrap();
        assert_eq!(data.iriref.unwrap(), data2.iriref.unwrap().unwrap(),);
    }

    #[test]
    fn valid_iriref_reader() {
        let data = MyUncheckedTable {
            iriref: Some("#foo"),
            iri: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes()).unwrap();
        assert_eq!(data.iriref.unwrap(), data2.iriref.unwrap().unwrap(),);
    }

    #[test]
    fn invalid_iriref_str() {
        let data = MyUncheckedTable {
            iriref: Some("a b"),
            iri: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_str::<MyTable>(&json_str);
        assert!(data2.is_err());
    }

    #[test]
    fn invalid_iriref_reader() {
        let data = MyUncheckedTable {
            iriref: Some("a b"),
            iri: None,
        };
        let json_str = serde_json::to_string(&data).unwrap();
        let data2 = serde_json::from_reader::<_, MyTable>(json_str.as_bytes());
        assert!(data2.is_err());
    }
}
