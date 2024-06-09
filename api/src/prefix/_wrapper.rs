use super::*;
use std::borrow::Borrow;

sophia_iri::wrap! { Prefix borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `PN_PREFIX?` rule in Turtle/SPARQL.
    pub fn new(prefix: T) -> Result<Self, InvalidPrefix> {
        if is_valid_prefix(prefix.borrow()) {
            Ok(Prefix(prefix))
        } else {
            Err(InvalidPrefix(prefix.borrow().to_string()))
        }
    }
}

impl<T: Borrow<str>> IsPrefix for Prefix<T> {}

#[cfg(feature = "serde")]
mod _serde {
    use super::*;
    use serde::{
        de::{Error, Unexpected},
        Deserialize, Serialize,
    };
    use std::borrow::Borrow;

    impl<'a, T: Borrow<str> + Deserialize<'a>> Deserialize<'a> for Prefix<T> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'a>,
        {
            let inner: T = T::deserialize(deserializer)?;
            Prefix::new(inner)
                .map_err(|err| D::Error::invalid_value(Unexpected::Str(&err.0), &"valid Prefix"))
        }
    }

    impl<T: Borrow<str>> Serialize for Prefix<T> {
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
            prefix: Prefix<String>,
        }

        #[derive(Serialize, Deserialize)]
        struct MyUncheckedTable {
            prefix: String,
        }

        #[test]
        fn valid_prefix() {
            let data = MyUncheckedTable {
                prefix: "foo".into(),
            };
            let toml_str = toml::to_string(&data).unwrap();
            let data2 = toml::from_str::<MyTable>(&toml_str).unwrap();
            assert_eq!(data.prefix, data2.prefix.unwrap());
        }

        #[test]
        fn invalid_prefix() {
            let data = MyUncheckedTable {
                prefix: "f o".into(),
            };
            let toml_str = toml::to_string(&data).unwrap();
            let data2 = toml::from_str::<MyTable>(&toml_str);
            assert!(data2.is_err());
        }
    }
}
