use super::{Iri, IriRef, Result};
use serde::{
    Deserialize, Serialize,
    de::{Error, Unexpected},
};
use std::borrow::Borrow;

impl<'a, T: Borrow<str> + Deserialize<'a>> Deserialize<'a> for Iri<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        let inner: T = T::deserialize(deserializer)?;
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
        iri: Option<String>,
        iriref: Option<String>,
    }

    #[test]
    fn valid_iri() {
        let data = MyUncheckedTable {
            iri: Some("http://example.org/".into()),
            iriref: None,
        };
        let toml_str = toml::to_string(&data).unwrap();
        let data2 = toml::from_str::<MyTable>(&toml_str).unwrap();
        assert_eq!(data.iri.unwrap(), data2.iri.unwrap().unwrap(),);
    }

    #[test]
    fn invalid_iri() {
        let data = MyUncheckedTable {
            iri: Some("#foo".into()),
            iriref: None,
        };
        let toml_str = toml::to_string(&data).unwrap();
        let data2 = toml::from_str::<MyTable>(&toml_str);
        assert!(data2.is_err());
    }

    #[test]
    fn valid_iriref() {
        let data = MyUncheckedTable {
            iriref: Some("#foo".into()),
            iri: None,
        };
        let toml_str = toml::to_string(&data).unwrap();
        let data2 = toml::from_str::<MyTable>(&toml_str).unwrap();
        assert_eq!(data.iriref.unwrap(), data2.iriref.unwrap().unwrap(),);
    }

    #[test]
    fn invalid_iriref() {
        let data = MyUncheckedTable {
            iriref: Some("a b".into()),
            iri: None,
        };
        let toml_str = toml::to_string(&data).unwrap();
        let data2 = toml::from_str::<MyTable>(&toml_str);
        assert!(data2.is_err());
    }
}
