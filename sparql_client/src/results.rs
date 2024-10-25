use super::Error;
use super::StaticTerm;
use serde::{Deserialize, Serialize};
use sophia_api::term::{BnodeId, LanguageTag, Term as _};
use sophia_iri::IriRef;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

mod xml_parser;

#[derive(Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
#[allow(clippy::module_name_repetitions)]
pub enum ResultsDocument {
    Boolean {
        head: BooleanHead,
        boolean: bool,
    },
    Bindings {
        #[serde(flatten)]
        doc: BindingsDocument,
    },
}

/// The result of a `SELECT` query as returned by [`SparqlClient`](super::SparqlClient).
#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct BindingsDocument {
    pub(super) head: BindingsHead,
    pub(super) results: Results,
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct BooleanHead {
    #[serde(default)]
    link: Vec<Box<str>>,
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct BindingsHead {
    pub(super) vars: Vec<Box<str>>,
    #[serde(default)]
    link: Vec<Box<str>>,
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct Results {
    pub(super) bindings: Vec<HashMap<Box<str>, Term>>,
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Term {
    Bnode { value: Box<str> },
    Literal(Literal),
    Uri { value: Box<str> },
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Literal {
    Datatype {
        value: Box<str>,
        datatype: Box<str>,
    },
    Lang {
        value: Box<str>,
        #[serde(rename = "xml:lang")]
        lang: Box<str>,
    },
    Simple {
        value: Box<str>,
    },
}

impl TryFrom<Term> for StaticTerm {
    type Error = Error;
    fn try_from(other: Term) -> Result<StaticTerm, Error> {
        use self::Literal::{Datatype, Lang, Simple};
        use Term::{Bnode, Literal, Uri};
        match other {
            Bnode { value } => Ok(BnodeId::new(value)?.into_term()),
            Literal(Simple { value }) => Ok(value.as_ref().into_term()),
            Literal(Datatype { value, datatype }) => Ok(StaticTerm::LiteralDatatype(
                value.into(),
                IriRef::new(datatype.into())?,
            )),
            Literal(Lang { value, lang }) => {
                Ok((value.as_ref() * LanguageTag::new(lang.as_ref())?).into_term())
            }
            Uri { value } => Ok(IriRef::new(value)?.into_term()),
        }
    }
}

impl BindingsDocument {
    pub(super) fn pop_binding(&mut self) -> Result<Vec<Option<StaticTerm>>, Error> {
        debug_assert!(!self.results.bindings.is_empty());
        let mut hm = self.results.bindings.drain(..1).next().unwrap();
        let mut v = Vec::<Option<StaticTerm>>::with_capacity(self.head.vars.len());
        for key in &self.head.vars {
            match hm.remove(key) {
                None => v.push(None),
                Some(term) => v.push(Some(term.try_into()?)),
            }
        }
        Ok(v)
    }
}

impl ResultsDocument {
    pub fn from_xml<T: std::io::BufRead>(data: T) -> Result<ResultsDocument, crate::Error> {
        xml_parser::parse_results_document(data)
    }
}

#[cfg(test)]
mod test_json {
    use super::*;

    #[test]
    fn uri() {
        let src = r#"{
            "type": "uri",
            "value": "tag:u"
        }"#;
        let got: Term = serde_json::from_str(src).unwrap();
        let exp = Term::Uri {
            value: "tag:u".into(),
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn literal_simple() {
        let src = r#"{
            "type": "literal",
            "value": "simple"
        }"#;
        let got: Term = serde_json::from_str(src).unwrap();
        let exp = Term::Literal(Literal::Simple {
            value: "simple".into(),
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn literal_datatype() {
        let src = r#"{
            "type": "literal",
            "value": "datatype",
            "datatype": "tag:d"
        }"#;
        let got: Term = serde_json::from_str(src).unwrap();
        let exp = Term::Literal(Literal::Datatype {
            value: "datatype".into(),
            datatype: "tag:d".into(),
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn literal_lang() {
        let src = r#"{
            "type": "literal",
            "value": "lang",
            "xml:lang": "en"
        }"#;
        let got: Term = serde_json::from_str(src).unwrap();
        let exp = Term::Literal(Literal::Lang {
            value: "lang".into(),
            lang: "en".into(),
        });
        assert_eq!(got, exp);
    }

    #[test]
    fn bnode() {
        let src = r#"{
            "type": "bnode",
            "value": "bnode"
        }"#;
        let got: Term = serde_json::from_str(src).unwrap();
        let exp = Term::Bnode {
            value: "bnode".into(),
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn empty_results() {
        let src = r#"{
            "bindings": []
        }"#;
        let got: Results = serde_json::from_str(src).unwrap();
        let exp = Results { bindings: vec![] };
        assert_eq!(got, exp);
    }

    #[test]
    fn len1_results() {
        let src = r#"{
            "bindings": [
                {
                    "a": {
                        "type": "uri",
                        "value": "tag:a0"
                    },
                    "b": {
                        "type": "literal",
                        "value": "simple"
                    },
                    "c": {
                        "type": "bnode",
                        "value": "bn0"
                    }
                }
            ]
        }"#;
        let got: Results = serde_json::from_str(src).unwrap();
        let exp = Results {
            bindings: vec![vec![
                (
                    "a".into(),
                    Term::Uri {
                        value: "tag:a0".into(),
                    },
                ),
                (
                    "b".into(),
                    Term::Literal(Literal::Simple {
                        value: "simple".into(),
                    }),
                ),
                (
                    "c".into(),
                    Term::Bnode {
                        value: "bn0".into(),
                    },
                ),
            ]
            .into_iter()
            .collect()],
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn bindings_head() {
        let src = r#"{
            "vars": ["a", "b", "c"]
        }"#;
        let got: BindingsHead = serde_json::from_str(src).unwrap();
        let exp = BindingsHead {
            vars: vec!["a".into(), "b".into(), "c".into()],
            link: vec![],
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn bindings_doc() {
        let src = r#"
        {
            "head": {
                "vars": ["a", "b", "c"]
            },
            "results": {
                "bindings": [
                    {
                        "a": {
                            "type": "uri",
                            "value": "tag:a0"
                        },
                        "b": {
                            "type": "literal",
                            "value": "simple"
                        },
                        "c": {
                            "type": "bnode",
                            "value": "bn0"
                        }
                    },
                    {
                        "c": {
                            "type": "literal",
                            "value": "datatype",
                            "datatype": "tag:d1"
                        },
                        "a": {
                            "type": "literal",
                            "value": "lang",
                            "xml:lang": "en"
                        }
                    }
                ]
            }
        }"#;
        let got: ResultsDocument = serde_json::from_str(src).unwrap();
        let exp = ResultsDocument::Bindings {
            doc: BindingsDocument {
                head: BindingsHead {
                    vars: vec!["a".into(), "b".into(), "c".into()],
                    link: vec![],
                },
                results: Results {
                    bindings: vec![
                        vec![
                            (
                                "a".into(),
                                Term::Uri {
                                    value: "tag:a0".into(),
                                },
                            ),
                            (
                                "b".into(),
                                Term::Literal(Literal::Simple {
                                    value: "simple".into(),
                                }),
                            ),
                            (
                                "c".into(),
                                Term::Bnode {
                                    value: "bn0".into(),
                                },
                            ),
                        ]
                        .into_iter()
                        .collect::<HashMap<Box<str>, Term>>(),
                        vec![
                            (
                                "c".into(),
                                Term::Literal(Literal::Datatype {
                                    value: "datatype".into(),
                                    datatype: "tag:d1".into(),
                                }),
                            ),
                            (
                                "a".into(),
                                Term::Literal(Literal::Lang {
                                    value: "lang".into(),
                                    lang: "en".into(),
                                }),
                            ),
                        ]
                        .into_iter()
                        .collect::<HashMap<Box<str>, Term>>(),
                    ],
                },
            },
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn boolean_doc() {
        let src = r#"
        {
            "head": {},
            "boolean": true
        }"#;
        let got: ResultsDocument = serde_json::from_str(src).unwrap();
        let exp = ResultsDocument::Boolean {
            head: BooleanHead { link: vec![] },
            boolean: true,
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn boolean_doc_with_link() {
        let src = r#"
        {
            "head": {
                "link": [ "https://example.org" ]
            },
            "boolean": false
        }"#;
        let got: ResultsDocument = serde_json::from_str(src).unwrap();
        let exp = ResultsDocument::Boolean {
            head: BooleanHead {
                link: vec!["https://example.org".into()],
            },
            boolean: false,
        };
        assert_eq!(got, exp);
    }
}
