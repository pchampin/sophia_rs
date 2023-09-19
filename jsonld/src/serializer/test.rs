use json_syntax::{Parse, Print, Value};
use sophia_api::{
    prelude::MutableDataset,
    quad::Spog,
    serializer::QuadSerializer,
    term::{BnodeId, SimpleTerm, Term},
};
use sophia_iri::Iri;

use crate::{JsonLdOptions, Jsonifier};

#[test]
fn iri_triple() -> TestResult {
    let ns = sophia_api::ns::Namespace::new("https://example.org/ns/")?;
    let mut dataset: Vec<Spog<SimpleTerm>> = vec![];
    dataset.insert_quad::<Spog<SimpleTerm>>((
        [
            ns.get("s")?.into_term(),
            ns.get("p")?.into_term(),
            ns.get("o")?.into_term(),
        ],
        None,
    ))?;

    let got = Jsonifier::new_jsonifier()
        .serialize_dataset(&dataset)?
        .to_json();
    let exp = Value::parse_str(
        r#"[
            {
                "@id":"https://example.org/ns/s",
                "https://example.org/ns/p":[
                    {
                        "@id":"https://example.org/ns/o"
                    }
                ]
            }
        ]"#,
        |_span| (),
    )?
    .into_value();
    assert_eq!(got, exp, "{}", got.pretty_print());
    Ok(())
}

#[test]
fn mixed_triple() -> TestResult {
    let mut dataset: Vec<Spog<SimpleTerm>> = vec![];
    dataset
        .insert_quad::<Spog<SimpleTerm>>((
            [
                BnodeId::new("b1")?.into_term(),
                Iri::new("tag:p")?.into_term(),
                42.into_term(),
            ],
            None,
        ))
        .unwrap();

    let got = Jsonifier::new_jsonifier()
        .serialize_dataset(&dataset)?
        .to_json();
    let exp = json_syntax::Value::parse_str(
        r#"[
            {
                "@id":"_:b1",
                "tag:p":[
                    {
                        "@value":"42",
                        "@type": "http://www.w3.org/2001/XMLSchema#integer"
                    }
                ]
            }
        ]"#,
        |_span| (),
    )?
    .into_value();
    assert_eq!(got, exp, "{}", got.pretty_print());
    Ok(())
}

#[test]
fn native_value() -> TestResult {
    let mut dataset: Vec<Spog<SimpleTerm>> = vec![];
    dataset
        .insert_quad::<Spog<SimpleTerm>>((
            [
                BnodeId::new("b1")?.into_term(),
                Iri::new("tag:p")?.into_term(),
                42.into_term(),
            ],
            None,
        ))
        .unwrap();

    let options = JsonLdOptions::new().with_use_native_types(true);
    let got = Jsonifier::new_jsonifier_with_options(options)
        .serialize_dataset(&dataset)?
        .to_json();
    let exp = json_syntax::Value::parse_str(
        r#"[
            {
                "@id":"_:b1",
                "tag:p":[
                    { "@value": 42 }
                ]
            }
        ]"#,
        |_span| (),
    )?
    .into_value();
    assert_eq!(got, exp, "{}", got.pretty_print());
    Ok(())
}

pub type TestResult = Result<(), Box<dyn std::error::Error>>;
