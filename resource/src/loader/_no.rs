use super::*;
use sophia_iri::Iri;
use std::borrow::Borrow;
use std::fmt::Debug;

/// A resource loader loading no resource.
#[derive(Clone, Copy, Debug, Default)]
pub struct NoLoader();

impl Loader for NoLoader {
    fn get<T: Borrow<str>>(&self, iri: Iri<T>) -> Result<(Vec<u8>, String), LoaderError> {
        Err(LoaderError::UnsupportedIri(
            iri.map_unchecked(|t| t.borrow().to_owned().into()),
            "NoLoader can not load any IRI".into(),
        ))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[test]
    fn noloader_fails() {
        let ldr = NoLoader();
        assert!(matches!(
            ldr.get(F1R1),
            Err(LoaderError::UnsupportedIri(_, _)),
        ));
    }
}
