use crate::test_util::*;
use std::path::Path;

#[test]
fn w3c_test_suite() {
    let mpath = Path::new("..")
        .join("json-ld-api")
        .join("tests")
        .join("fromRdf-manifest.jsonld");
    let manifest = Manifest::new(&mpath);
    let (failed, skipped, passed) = manifest.perform_all_tests(true);
    assert_eq!(0, failed, "{}/{}", failed, failed + skipped + passed);
}
