//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! It provides a resource-centric API on top of the [Sophia] API.
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/
#![deny(missing_docs)]

pub mod loader;
pub mod resource;

pub use loader::{Loader, LoaderError, LocalLoader, NoLoader};
pub use resource::{Resource, ResourceError, TypedResource};

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::{
        prelude::{Graph, Term},
        term::SimpleTerm,
        MownStr,
    };
    use sophia_iri::Iri;

    pub const NS: Iri<&str> = Iri::new_unchecked_const("http://example.org/");
    pub const F1: Iri<&str> = Iri::new_unchecked_const("http://example.org/file1.ttl");
    pub const F1R1: Iri<&str> = Iri::new_unchecked_const("http://example.org/file1.ttl#res1");
    pub const F1R2: Iri<&str> = Iri::new_unchecked_const("http://example.org/file1.ttl#res2");
    pub const F1R3: Iri<&str> = Iri::new_unchecked_const("http://example.org/file1.ttl#res3");
    pub const F2: Iri<&str> = Iri::new_unchecked_const("http://example.org/file2.ttl");
    pub const F2R1: Iri<&str> = Iri::new_unchecked_const("http://example.org/file2.ttl#res1");
    pub const F2R2: Iri<&str> = Iri::new_unchecked_const("http://example.org/file2.ttl#res2");
    pub const FAIL: Iri<&str> = Iri::new_unchecked_const("http://example.org/not_there");
    pub const SUBDIR: Iri<&str> = Iri::new_unchecked_const("http://example.org/subdir");
    // test with no extension (conneg emulation)
    pub const F1X: Iri<&str> = Iri::new_unchecked_const("http://example.org/file1");
    pub const F1XR1: Iri<&str> = Iri::new_unchecked_const("http://example.org/file1#res1");

    pub const EX_ID: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#id");
    pub const EX_LIST: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#list");
    pub const EX_FOREIGN1: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#foreign1");
    pub const EX_FOREIGN2: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#foreign2");
    pub const EX_NEXT: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#next");
    pub const EX_RELATED: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#related");
    pub const EX_UNREACHABLE: Iri<&str> =
        Iri::new_unchecked_const("http://example.org/ns#unreachable");
    pub const EX_UNUSED: Iri<&str> = Iri::new_unchecked_const("http://example.org/ns#unused");

    /// Number of triples in F1
    pub const F1_LEN: usize = 20;
    /// Number of triples in F2
    pub const F2_LEN: usize = 2;

    pub type MyGraph = Vec<[SimpleTerm<'static>; 3]>;
    pub type TestResult = Result<(), Box<dyn std::error::Error>>;

    pub fn make_loader() -> LocalLoader {
        let ns = NS.map_unchecked(MownStr::from);
        LocalLoader::new(vec![(
            ns,
            std::path::Path::new("test").canonicalize().unwrap().into(),
        )])
        .unwrap()
    }

    /// Test impl of TypedResource
    pub struct WithId(Resource<MyGraph, LocalLoader>);

    impl TryFrom<Resource<MyGraph, LocalLoader>> for WithId {
        type Error = ResourceError<<MyGraph as Graph>::Error>;

        fn try_from(value: Resource<MyGraph, LocalLoader>) -> Result<Self, Self::Error> {
            if value.get_any_term(&EX_ID)?.is_none() {
                Err(ResourceError::NoValueFor {
                    id: value.id().into_term(),
                    predicate: EX_ID.into_term(),
                })
            } else {
                Ok(Self(value))
            }
        }
    }

    impl std::ops::Deref for WithId {
        type Target = Resource<MyGraph, LocalLoader>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }
}
