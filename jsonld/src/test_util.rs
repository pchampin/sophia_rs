//! Utility types and functions for testing JSON-LD

#![allow(missing_docs)]

use crate::config::*;
use crate::error::*;
use crate::parser::JsonLdParser;
use crate::serializer::Jsonifier;
use json::JsonValue;
use sophia_api::dataset::isomorphic_datasets;
use sophia_api::dataset::MutableDataset;
use sophia_api::parser::QuadParser;
use sophia_api::serializer::QuadSerializer;
use sophia_api::serializer::Stringifier;
use sophia_api::triple::stream::SinkError;
use sophia_api::triple::stream::StreamError::SourceError;
use sophia_iri::resolve::Resolve;
use sophia_term::iri::Iri;
use sophia_term::BoxTerm;
use sophia_turtle::serializer::nq::NqSerializer;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

pub struct Manifest {
    base: PathBuf,
    iri: Iri<Box<str>>,
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
        let iri = Iri::new_suffixed(
            json["baseIri"].as_str().expect("no baseIri"),
            json["@context"][1]["@base"].as_str().expect("no @base"),
        )
        .expect("could not resolve manifest IRI");
        Self { base, iri, json }
    }

    pub fn iri(&self) -> &Iri<Box<str>> {
        &self.iri
    }

    pub fn tests(&self) -> impl Iterator<Item = Test> {
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

    pub fn iri(&self) -> Iri<Box<str>> {
        let test_iri = Iri::<&str>::new(self.id()).expect("test has invalid IRI");
        let mut buffer = String::new();
        self.manifest
            .iri()
            .parse_components(&mut buffer)
            .resolve(&test_iri)
            .map_into()
    }

    pub fn positive(&self) -> bool {
        self.json["@type"].contains("jld:PositiveEvaluationTest")
    }

    pub fn negative(&self) -> bool {
        self.json["@type"].contains("jld:NegativeEvaluationTest")
    }

    pub fn input_raw(&self) -> &str {
        self.json["input"].as_str().unwrap()
    }

    pub fn input_path(&self) -> PathBuf {
        let mut path = self.manifest.base.clone();
        for part in self.json["input"].as_str().unwrap().split('/') {
            path.push(part);
        }
        path
    }

    pub fn input(&self) -> String {
        fs::read_to_string(self.input_path()).expect("test input could not be read")
    }

    pub fn input_dataset(&self) -> HashSet<([BoxTerm; 3], Option<BoxTerm>)> {
        let nq = self.input();
        let quads = sophia_turtle::parser::nq::parse_str(&nq);
        let mut dataset = HashSet::new();
        MutableDataset::insert_all(&mut dataset, quads)
            .expect("test input could not be parsed as N-Quads");
        dataset
    }

    pub fn input_json(&self) -> JsonValue {
        let src = self.input();
        json::parse(&src).expect("test input could not be parsed as JSON")
    }

    pub fn expected(&self) -> String {
        let mut path = self.manifest.base.clone();
        for part in self.json["expect"].as_str().unwrap().split('/') {
            path.push(part);
        }
        fs::read_to_string(path).expect("test expect could not be read")
    }

    pub fn expected_dataset(&self) -> HashSet<([BoxTerm; 3], Option<BoxTerm>)> {
        let nq = self.expected();
        let mut dataset = HashSet::new();
        if self.json["requires"] == "GeneralizedRdf" {
            let quads = sophia_turtle::parser::gtrig::parse_str(&nq);
            MutableDataset::insert_all(&mut dataset, quads)
                .expect("test expect could not be parsed as N-Quads");
        } else {
            let quads = sophia_turtle::parser::nq::parse_str(&nq);
            MutableDataset::insert_all(&mut dataset, quads)
                .expect("test expect could not be parsed as N-Quads");
        }
        dataset
    }

    pub fn expected_json(&self) -> JsonValue {
        let src = self.expected();
        json::parse(&src).expect("test expect could not be parsed as JSON")
    }

    pub fn expected_error(&self) -> &str {
        self.json["expectErrorCode"].as_str().unwrap()
    }

    pub fn config(&self) -> JsonLdConfig {
        let mut config = JsonLdConfig::new();
        for (key, val) in self.json["option"].entries() {
            match key {
                "base" => {
                    config.base = Some(val.as_str().expect("base must be a string").into());
                }
                "expandContext" => {
                    config.expand_context =
                        Some(val.as_str().expect("expandContext must be a string").into());
                }
                "processingMode" => {
                    config.processing_mode = JsonLdSpecVersion::new(val.as_str().unwrap())
                        .expect("unrecognized processingMode");
                }
                "produceGeneralizedRdf" => {
                    // TODO handle this properly
                    // (currently, Sophia always produce generalized RDF)
                }
                "rdfDirection" => {
                    config.rdf_direction = val.as_str().map(|val| match val {
                        "i18n-datatype" => RdfDirectionMode::I18nDatatype,
                        "compound-literal" => RdfDirectionMode::CompoundLiteral,
                        _ => panic!("Unknown rdfDirection {}", val),
                    });
                }
                "useNativeTypes" => {
                    config.use_native_types = val.as_bool().unwrap();
                }
                "useRdfType" => {
                    config.use_rdf_type = val.as_bool().unwrap();
                }
                "useJCS" | "specVersion" => {} // not part of the JSON-LD options
                _ => panic!("Unknown option {}", key),
            }
        }
        config
    }

    pub fn spec_version(&self) -> JsonLdSpecVersion {
        self.json["option"]["specVersion"]
            .as_str()
            .and_then(JsonLdSpecVersion::new)
            .unwrap_or_default()
    }

    pub fn use_jcs(&self) -> bool {
        self.json["option"]["useJCS"].as_bool().unwrap_or(false)
    }

    pub fn skip(&self) -> bool {
        self.json["option"]["specVersion"] == "json-ld-1.0"
    }

    pub fn perform(&self, verbose: bool) -> TestResult {
        if self.spec_version() != JsonLdSpecVersion::JsonLd11
            || self.config().processing_mode != JsonLdSpecVersion::JsonLd11
        {
            if verbose {
                println!("testing {} .....    SKIP (JSON-LD 1.0)", self.id());
            }
            TestResult::Skip
        } else if self.json["@type"].contains("jld:FromRDFTest") {
            perform_from_rdf(self, verbose)
        } else if self.json["@type"].contains("jld:ToRDFTest") {
            perform_to_rdf(self, verbose)
        } else {
            if verbose {
                println!("testing {} .....    SKIP (unrecognized)", self.id());
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
            Ok(got) if jsonld_cmp(got.as_json(), &exp, "") => {
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

pub fn perform_to_rdf(test: &Test, verbose: bool) -> TestResult {
    if verbose {
        print!("testing {} .....\t", test.id());
    }
    //println!("=== -----------------------------------------------------------");
    let input_res = expand(test);
    let par = JsonLdParser::new_with_config(test.config());
    if test.positive() {
        if test.use_jcs() {
            // TODO eventually pass these tests when JCS is implemented
            if verbose {
                println!("SKIP (JCS is not yet implemented)");
            }
            return TestResult::Skip;
        }
        let input = input_res.expect("expand should have succeeded");
        let exp = test.expected_dataset();
        let quads = par.parse(input.as_bytes());
        let mut got: HashSet<([BoxTerm; 3], Option<BoxTerm>)> = HashSet::new();
        match MutableDataset::insert_all(&mut got, quads) {
            Ok(_) if isomorphic_datasets(&got, &exp).unwrap() => {
                if verbose {
                    println!("PASS");
                }
                TestResult::Pass
            }
            Err(SourceError(JsonLdError::UnsupportedVersion(_))) => {
                if verbose {
                    println!("SKIP");
                }
                TestResult::Skip
            }
            Ok(_) => {
                if verbose {
                    let mut stringifier = NqSerializer::new_stringifier();
                    stringifier.serialize_dataset(&got).unwrap();
                    let lines: Vec<_> = stringifier.as_str().split("\n").collect();
                    let nq = lines.join("\n    ");
                    println!("FAIL\n    {}", nq);
                }
                TestResult::Fail
            }
            Err(err) => {
                if verbose {
                    println!("FAIL\n    {}", err);
                }
                TestResult::Fail
            }
        }
    } else if test.negative() {
        let input = match input_res {
            Err(_) => {
                // the failure is probably the expected result for this test
                // (anyway, we do not intend to test the underlying expand implementation)
                if verbose {
                    println!("SKIP (expand failed)");
                }
                return TestResult::Skip;
            }
            Ok(input) => input,
        };
        let exp = test.expected_error();
        let quads = par.parse(input.as_bytes());
        let mut got: HashSet<([BoxTerm; 3], Option<BoxTerm>)> = HashSet::new();
        match MutableDataset::insert_all(&mut got, quads) {
            Err(SourceError(JsonLdError::UnsupportedVersion(_))) => {
                if verbose {
                    println!("SKIP");
                }
                TestResult::Skip
            }
            Err(SourceError(e)) if format!("{}", e).starts_with(&exp) => {
                if verbose {
                    println!("PASS");
                }
                TestResult::Pass
            }
            res => {
                if verbose {
                    println!("FAIL\n    got: {:?}\n    exp: {}", res.map(|_| ()), exp);
                }
                TestResult::Fail
            }
        }
    } else {
        if verbose {
            println!("SKIP (not an evaluation test)");
        }
        TestResult::Skip
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
                    if !a2.iter().any(|j| jsonld_cmp(i, j, "")) {
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

pub fn expand(test: &Test) -> Result<String, AnyErr> {
    let file = std::fs::File::open(test.input_path()).unwrap();
    // see https://gist.github.com/pchampin/9416e423efe9925adaee536a3abdaf4c
    let mut cmd = std::process::Command::new("pyld");
    let config = test.config();
    let ctx = &config.expand_context;
    cmd.arg("expand").stdin(file);
    if let Some(base) = &config.base {
        cmd.arg("-b").arg(base);
    } else if ctx.is_none() {
        cmd.arg("-b").arg(format!(
            "https://w3c.github.io/json-ld-api/tests/{}",
            test.input_raw()
        ));
    }
    if let Some(ctx) = ctx {
        let mut path = test.manifest.base.clone();
        for part in ctx.split('/') {
            path.push(part);
        }
        let path = std::env::current_dir()?.join(path);
        cmd.arg("-c").arg(path.as_os_str());
    }
    let out = cmd.output()?.stdout;
    // It would be cleaner to check the spawned process exist status,
    // to determine if it failed or not.
    // But testing for an empty output is easier to implement,
    // and does the trick.
    if out.is_empty() {
        // expansion failed
        Err(AnyErr)
    } else {
        Ok(String::from_utf8(out)?)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct AnyErr;

impl<T: std::error::Error> From<T> for AnyErr {
    fn from(_: T) -> Self {
        AnyErr
    }
}
