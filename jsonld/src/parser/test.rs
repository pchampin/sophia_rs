use sophia_api::{prelude::QuadParser, quad::Spog, source::QuadSource};
use sophia_term::ArcTerm;
use sophia_turtle::parser::nq;

use crate::{JsonLdOptions, JsonLdParser};

// Check whether JsonLdParser<NoLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[test]
fn check_no_loader() {
    let loader = crate::loader::NoLoader::default();
    let options = JsonLdOptions::new().with_document_loader(loader);
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
    let loader = crate::loader::FsLoader::default();
    let options = JsonLdOptions::new().with_document_loader(loader);
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
    let loader = crate::loader::StaticLoader::default();
    let options = JsonLdOptions::new().with_document_loader(loader);
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

// Check whether JsonLdParser<SurfLoader> correctly implements QuadParser
// (i.e. it has the correct trait bounds).
// NB: the goal is NOT to check the loader itself -- we actually don't use it.
#[cfg(feature = "surf_loader")]
#[test]
fn check_surf_loader() {
    let loader = crate::loader::SurfLoader::new(0);
    let options = JsonLdOptions::new().with_document_loader(loader);
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
    let loader = crate::loader::ChainLoader::new(
        crate::loader::StaticLoader::default(),
        crate::loader::FsLoader::default(),
    );
    let options = JsonLdOptions::new().with_document_loader(loader);
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
