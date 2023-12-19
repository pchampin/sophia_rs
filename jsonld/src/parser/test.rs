use sophia_api::{prelude::QuadParser, quad::Spog, source::QuadSource};
use sophia_term::ArcTerm;
use sophia_turtle::parser::nq;

use crate::{
    loader::{FsLoader, NoLoader, StaticLoader},
    JsonLdOptions, JsonLdParser,
};

// Check whether JsonLdParser<NoLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[test]
fn check_no_loader() {
    let options = JsonLdOptions::new().with_default_document_loader::<NoLoader>();
    let p = JsonLdParser::new_with_options(options);
    let got: TestDataset = p
        .parse_str(r#"{"@id": "tag:foo", "tag:bar": "BAZ"}"#)
        .collect_quads()
        .unwrap();
    let exp: TestDataset = nq::parse_str(r#"<tag:foo> <tag:bar> "BAZ"."#)
        .collect_quads()
        .unwrap();
    assert_eq!(got, exp);
}

// Check whether JsonLdParser<FsLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[test]
fn check_fs_loader() {
    let options = JsonLdOptions::new().with_default_document_loader::<FsLoader>();
    let p = JsonLdParser::new_with_options(options);
    let got: TestDataset = p
        .parse_str(r#"{"@id": "tag:foo", "tag:bar": "BAZ"}"#)
        .collect_quads()
        .unwrap();
    let exp: TestDataset = nq::parse_str(r#"<tag:foo> <tag:bar> "BAZ"."#)
        .collect_quads()
        .unwrap();
    assert_eq!(got, exp);
}

// Check whether JsonLdParser<StaticLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[test]
fn check_static_loader() {
    let options = JsonLdOptions::new().with_default_document_loader::<StaticLoader<_, _>>();
    let p = JsonLdParser::new_with_options(options);
    let got: TestDataset = p
        .parse_str(r#"{"@id": "tag:foo", "tag:bar": "BAZ"}"#)
        .collect_quads()
        .unwrap();
    let exp: TestDataset = nq::parse_str(r#"<tag:foo> <tag:bar> "BAZ"."#)
        .collect_quads()
        .unwrap();
    assert_eq!(got, exp);
}

// Check whether JsonLdParser<HttpLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[cfg(feature = "http_client")]
#[test]
fn check_http_loader() {
    use crate::loader::HttpLoader;

    let options = JsonLdOptions::new().with_default_document_loader::<HttpLoader>();
    let p = JsonLdParser::new_with_options(options);
    let got: TestDataset = p
        .parse_str(r#"{"@id": "tag:foo", "tag:bar": "BAZ"}"#)
        .collect_quads()
        .unwrap();
    let exp: TestDataset = nq::parse_str(r#"<tag:foo> <tag:bar> "BAZ"."#)
        .collect_quads()
        .unwrap();
    assert_eq!(got, exp);
}

#[cfg(feature = "file_url")]
// Check whether JsonLdParser<FileUrlLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[test]
fn check_file_url_loader() {
    use crate::loader::FileUrlLoader;

    let options = JsonLdOptions::new().with_default_document_loader::<FileUrlLoader>();
    let p = JsonLdParser::new_with_options(options);
    let got: TestDataset = p
        .parse_str(r#"{"@id": "tag:foo", "tag:bar": "BAZ"}"#)
        .collect_quads()
        .unwrap();
    let exp: TestDataset = nq::parse_str(r#"<tag:foo> <tag:bar> "BAZ"."#)
        .collect_quads()
        .unwrap();
    assert_eq!(got, exp);
}

// Check whether JsonLdParser<ChainLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[test]
fn check_chain_loader() {
    let options = JsonLdOptions::new().with_document_loader_closure(|| {
        crate::loader::ChainLoader::new(StaticLoader::default(), FsLoader::default())
    });
    let p = JsonLdParser::new_with_options(options);
    let got: TestDataset = p
        .parse_str(r#"{"@id": "tag:foo", "tag:bar": "BAZ"}"#)
        .collect_quads()
        .unwrap();
    let exp: TestDataset = nq::parse_str(r#"<tag:foo> <tag:bar> "BAZ"."#)
        .collect_quads()
        .unwrap();
    assert_eq!(got, exp);
}

type TestDataset = Vec<Spog<ArcTerm>>;
