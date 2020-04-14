//! Utility types and functions for testing JSON-LD

use crate::config::*;
use crate::error::*;
use crate::serializer::Jsonifier;
use json::JsonValue;
use sophia::dataset::MutableDataset;
use sophia::serializer::QuadSerializer;
use sophia::triple::stream::SinkError;
use sophia_term::iri::Iri;
use sophia_term::BoxTerm;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

pub struct Manifest {
    base: PathBuf,
    json: JsonValue,
}

impl Manifest {
    pub fn new(path: &Path) -> Self {
        if !path.exists() || !path.is_file() {
            panic!("manifest not found or not a file: {}", path.display());
        }
        let base = path.parent().unwrap().into();
        let data = fs::read(path).expect("manifest could not be read");
        let data = String::from_utf8_lossy(&data);
        let json = json::parse(&data).expect("manifest could not be parsed");
        Self { base, json }
    }

    pub fn base_iri(&self) -> &str {
        self.json["baseIri"].as_str().unwrap()
    }

    pub fn tests<'s>(&'s self) -> impl Iterator<Item = Test<'s>> + 's {
        self.json["sequence"]
            .members()
            .map(move |val| Test::new(self, val))
    }

    /// Perform all tests in this manifest,
    /// returning the number of failed / skipped / passed
    pub fn perform_all_tests(&self, verbose: bool) -> (usize, usize, usize) {
        let (mut failed, mut skipped, mut passed) = (0, 0, 0);
        for t in self.tests() {
            match t.perform(verbose) {
                TestResult::Fail => failed += 1,
                TestResult::Skip => skipped += 1,
                TestResult::Pass => passed += 1,
            }
        }
        (failed, skipped, passed)
    }
}

pub struct Test<'a> {
    manifest: &'a Manifest,
    json: &'a JsonValue,
}

impl<'a> Test<'a> {
    pub fn new(manifest: &'a Manifest, json: &'a JsonValue) -> Self {
        Self { manifest, json }
    }

    pub fn id(&self) -> &str {
        self.json["@id"].as_str().unwrap()
    }

    pub fn iri(&self) -> Iri<&str> {
        Iri::new_suffixed(self.manifest.base_iri(), self.id()).unwrap()
    }

    pub fn positive(&self) -> bool {
        self.json["@type"].contains("jld:PositiveEvaluationTest")
    }

    pub fn input(&self) -> String {
        let mut path = self.manifest.base.clone();
        for part in self.json["input"].as_str().unwrap().split('/') {
            path.push(part);
        }
        fs::read_to_string(path).expect("test input could not be read")
    }

    pub fn input_dataset(&self) -> HashSet<([BoxTerm; 3], Option<BoxTerm>)> {
        let nq = self.input();
        let mut quads = sophia::parser::nq::parse_str(&nq);
        let mut dataset = HashSet::new();
        MutableDataset::insert_all(&mut dataset, &mut quads)
            .expect("test input could not be parsed");
        dataset
    }

    pub fn expected(&self) -> String {
        let mut path = self.manifest.base.clone();
        for part in self.json["expect"].as_str().unwrap().split('/') {
            path.push(part);
        }
        fs::read_to_string(path).expect("test expect could not be read")
    }

    pub fn expected_json(&self) -> JsonValue {
        let src = self.expected();
        json::parse(&src).expect("test expect could not be parsed")
    }

    pub fn expected_error(&self) -> &str {
        self.json["expectErrorCode"].as_str().unwrap()
    }

    pub fn config(&self) -> JsonLdConfig {
        let mut config = JsonLdConfig::new();
        for (key, val) in self.json["option"].entries() {
            match key {
                "rdfDirection" => {
                    config.rdf_direction = val.as_str().map(|val| match val {
                        "i18n-datatype" => RdfDirectionMode::I18nDatatype,
                        "compound-literal" => RdfDirectionMode::CompoundLiteral,
                        _ => panic!("Unknown rdfDirection {}", val),
                    });
                }
                "specVersion" => {
                    let version = match val.as_str().unwrap() {
                        "json-ld-1.0" => JsonLdSpecVersion::JsonLd10,
                        "json-ld-1.1" => JsonLdSpecVersion::JsonLd11,
                        _ => panic!("Unknown specVersion {}", val),
                    };
                    config.spec_version = version;
                }
                "useNativeTypes" => {
                    config.use_native_types = val.as_bool().unwrap();
                }
                "useRdfType" => {
                    config.use_rdf_type = val.as_bool().unwrap();
                }
                _ => panic!("Unknown option {}", key),
            }
        }
        config
    }

    pub fn skip(&self) -> bool {
        self.json["option"]["specVersion"] == "json-ld-1.0"
    }

    pub fn perform(&self, verbose: bool) -> TestResult {
        if self.json["@type"].contains("jld:FromRDFTest") {
            perform_from_rdf(self, verbose)
        } else {
            if verbose {
                println!("testing {} .....SKIP (unrecognized)", self.id());
            }
            TestResult::Skip
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum TestResult {
    Fail,
    Skip,
    Pass,
}

pub fn perform_from_rdf(test: &Test, verbose: bool) -> TestResult {
    if verbose {
        print!("testing {} .....\t", test.id());
    }
    //println!("=== -----------------------------------------------------------");
    let mut ser = Jsonifier::new_jsonifier_with_config(test.config());
    if test.positive() {
        let exp = test.expected_json();
        match ser.serialize_dataset(&test.input_dataset()) {
            Ok(got) if jsonld_cmp(&got.as_json(), &exp, "") => {
                if verbose {
                    println!("PASS");
                }
                TestResult::Pass
            }
            Err(SinkError(JsonLdError::UnsupportedVersion(_))) => {
                if verbose {
                    println!("SKIP");
                }
                TestResult::Skip
            }
            res => {
                if verbose {
                    println!("FAIL\n    {}", log_res(&res));
                }
                TestResult::Fail
            }
        }
    } else {
        let exp = test.expected_error();
        match ser.serialize_dataset(&test.input_dataset()) {
            Err(SinkError(JsonLdError::UnsupportedVersion(_))) => {
                if verbose {
                    println!("SKIP");
                }
                TestResult::Skip
            }
            Err(SinkError(e)) if format!("{}", e).starts_with(&exp) => {
                if verbose {
                    println!("PASS");
                }
                TestResult::Pass
            }
            res => {
                if verbose {
                    println!("FAIL\n  {:?}", res.map(|_| ()));
                }
                TestResult::Fail
            }
        }
    }
}

pub fn jsonld_cmp(v1: &JsonValue, v2: &JsonValue, orig: &str) -> bool {
    use JsonValue::*;
    match (v1, v2) {
        (Object(o1), Object(o2)) => {
            if o1.len() != o2.len() {
                return false;
            }
            for (key, val) in o1.iter() {
                match o2.get(key) {
                    None => {
                        return false;
                    }
                    Some(val2) => {
                        if !jsonld_cmp(val, val2, key) {
                            return false;
                        }
                    }
                }
            }
            true
        }
        (Array(a1), Array(a2)) => {
            if orig == "@list" {
                a1 == a2
            } else {
                if a1.len() != a2.len() {
                    return false;
                }
                for i in a1.iter() {
                    if a2.iter().find(|j| jsonld_cmp(i, j, "")).is_none() {
                        return false;
                    }
                }
                true
            }
        }
        (v1, v2) => v1 == v2,
    }
}

pub fn log_res<E: std::fmt::Debug>(res: &Result<&mut Jsonifier, E>) -> String {
    match res {
        Ok(got) => json::stringify_pretty(got.as_json().clone(), 4),
        Err(e) => format!("{:?}", e),
    }
}
