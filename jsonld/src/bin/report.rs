//! Produce EARL report for the JSON-LD test suite
//!
//! To run, this requires that the submodule json-ld-api be checked out.

use chrono::Utc;
use sophia_jsonld::test_util::*;
use sophia_term::TTerm;
use std::path::Path;

const MAKER: &str = "http://champin.net/#pa";

pub fn main() {
    header(MAKER, &Utc::now().to_rfc3339());

    let mpath = Path::new("..")
        .join("json-ld-api")
        .join("tests")
        .join("fromRdf-manifest.jsonld");
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let manifest = Manifest::new(&mpath);
    for t in manifest.tests() {
        let outcome = match t.perform(false) {
            TestResult::Pass => {
                passed += 1;
                "passed"
            }
            TestResult::Skip => {
                skipped += 1;
                "untested"
            }
            TestResult::Fail => {
                failed += 1;
                "failed"
            }
        };
        assertion(MAKER, &Utc::now().to_rfc3339(), &t.iri().value(), outcome);
    }

    let res = if failed == 0 { "ok" } else { "KO" };
    println!(
        "# test result; {}. {} passed; {} failed; {} skipped",
        res, passed, failed, skipped
    );
}

fn header(maker: &str, datetime: &str) {
    let doap_path = Path::new("..").join("sophia_doap.ttl");
    println!(
        "{}",
        std::fs::read_to_string(doap_path).expect("doap file not found or unreadable")
    );

    println!(
        r#"<> foaf:primaryTopic <https://github.com/pchampin/sophia_rs>;
dc:issued "{1}"^^xsd:dateTime;
foaf:maker <{0}>.

<{0}> a earl:Assertor;
foaf:title "Implementor" ."#,
        maker, datetime
    );
}

fn assertion(maker: &str, datetime: &str, test_iri: &str, outcome: &str) {
    println!(
        r#"[ a earl:Assertion;
    earl:assertedBy <{0}>;
    earl:subject <https://github.com/pchampin/sophia_rs>;
    earl:test <{2}>;
    earl:result [
      a earl:TestResult;
      earl:outcome earl:{3};
      dc:date "{1}"^^xsd:dateTime];
    earl:mode earl:automatic ] .
"#,
        maker, datetime, test_iri, outcome
    )
}
