//! Parse XML-formatted SPARQL results.
//!
//! We are not using Serde here because `xml_serde` has a number of limitations.

use std::{borrow::Cow, io::BufRead};

use quick_xml::{
    events::{
        BytesStart, BytesText,
        Event::{CData, Empty, End, Eof, Start, Text},
    },
    name::{Namespace, QName, ResolveResult},
    NsReader,
};

use super::{
    BindingsDocument, BindingsHead, BooleanHead, HashMap, Literal, Results, ResultsDocument, Term,
};
use crate::Error::{self, SparqlXml};

pub fn parse_results_document<R: BufRead>(data: R) -> Result<ResultsDocument, Error> {
    SparqlXmlParser::new(data).parse_results_document()
}

struct SparqlXmlParser<R: BufRead> {
    events: NsReader<R>,
    buf: Vec<u8>,
}

impl<R: BufRead> SparqlXmlParser<R> {
    fn new(data: R) -> Self {
        Self {
            events: NsReader::from_reader(data),
            buf: vec![],
        }
    }

    pub fn parse_results_document(&mut self) -> Result<ResultsDocument, Error> {
        self.next_start_expecting("sparql")?;
        self.next_start_expecting("head")?;
        let mut variables: Vec<Box<str>> = vec![];
        let mut links: Vec<Box<str>> = vec![];
        while let Some(elt) = self.next_empty()? {
            let (ns, local_name) = self.events.resolve_element(elt.name());
            if ns != ResolveResult::Bound(NS) {
                return Err(SparqlXml(format!(
                    "Unrecognized element in <head>: {local_name:?}"
                )));
            }
            match local_name.into_inner() {
                b"variable" => variables.push(self.get_attr(&elt, "name")?),
                b"link" => links.push(self.get_attr(&elt, "href")?),
                _ => return Err(SparqlXml(format!("Unknown element <{local_name:?}>"))),
            }
        }
        if variables.is_empty() {
            self.next_start_expecting("boolean")?;
            let boolean = match self.next_text()?.as_ref() {
                "true" => true,
                "false" => false,
                other => return Err(SparqlXml(format!("Unrecognized boolean value '{other}'"))),
            };
            Ok(ResultsDocument::Boolean {
                head: BooleanHead { link: links },
                boolean,
            })
        } else {
            Ok(ResultsDocument::Bindings {
                doc: BindingsDocument {
                    head: BindingsHead {
                        vars: variables,
                        link: links,
                    },
                    results: self.parse_bindings_results()?,
                },
            })
        }
    }

    fn parse_bindings_results(&mut self) -> Result<Results, Error> {
        self.next_start_expecting("results")?;
        let mut bindings: Vec<HashMap<Box<str>, Term>> = vec![];

        while self.next_start_expecting_maybe("result")?.is_some() {
            let mut result = HashMap::new();
            while let Some(binding) = self.next_start_expecting_maybe("binding")? {
                let name = self.get_attr(&binding, "name")?;
                let Some(elt) = self.next_start()? else {
                    return Err(SparqlXml(format!("No child in <binding name='{name}'>")));
                };
                let term = self.parse_term(&elt, &name)?;
                self.expect_closing(binding.name())?;
                result.insert(name, term);
            }
            bindings.push(result);
        }
        Ok(Results { bindings })
    }

    fn parse_term(&mut self, start: &BytesStart<'static>, name: &str) -> Result<Term, Error> {
        let (ns, local_name) = self.events.resolve_element(start.name());
        if ns != ResolveResult::Bound(NS) {
            return Err(SparqlXml(format!(
                "Unrecognized term in <binding name='{}'>: {:?}",
                name,
                start.name()
            )));
        }
        let value: Box<str> = self.next_text()?.into();
        match local_name.into_inner() {
            b"uri" => Ok(Term::Uri { value }),
            b"bnode" => Ok(Term::Bnode { value }),
            b"literal" => {
                if let Some(lang) = self.get_attr_maybe(start, "xml:lang")? {
                    Ok(Term::Literal(Literal::Lang { value, lang }))
                } else if let Some(datatype) = self.get_attr_maybe(start, "datatype")? {
                    Ok(Term::Literal(Literal::Datatype { value, datatype }))
                } else {
                    Ok(Term::Literal(Literal::Simple { value }))
                }
            }
            other => Err(SparqlXml(format!(
                "Unrecognized term in <binding name='{name}'>: {other:?}"
            ))),
        }
    }

    fn next_start_expecting(&mut self, local_name: &str) -> Result<BytesStart<'static>, Error> {
        match self.next_start_expecting_maybe(local_name)? {
            None => Err(SparqlXml(format!(
                "Expected <{local_name}>, found no element"
            ))),
            Some(elt) => Ok(elt),
        }
    }

    fn next_start_expecting_maybe(
        &mut self,
        local_name: &str,
    ) -> Result<Option<BytesStart<'static>>, Error> {
        if let Some(start) = self.next_start()? {
            if self.check_element(&start, local_name) {
                Ok(Some(start))
            } else {
                Err(SparqlXml(format!(
                    "Expected <{}>, found {:?}",
                    local_name,
                    start.name()
                )))
            }
        } else {
            Ok(None)
        }
    }

    fn next_start(&mut self) -> Result<Option<BytesStart<'static>>, Error> {
        loop {
            match self.events.read_event_into(&mut self.buf)? {
                Start(s) => return Ok(Some(s.into_owned())),
                End(_) | Eof => return Ok(None),
                _ => continue,
            }
        }
    }

    fn next_empty(&mut self) -> Result<Option<BytesStart<'static>>, Error> {
        loop {
            match self.events.read_event_into(&mut self.buf)? {
                Empty(s) => return Ok(Some(s.into_owned())),
                Start(s) => {
                    let s = s.into_owned();
                    self.expect_closing(s.name())?;
                    return Ok(Some(s));
                }
                End(_) | Eof => return Ok(None),
                _ => continue,
            }
        }
    }

    fn next_text(&mut self) -> Result<String, Error> {
        loop {
            match self.events.read_event_into(&mut self.buf)? {
                Text(e) => return Ok(txt(e)?.map_or_else(String::new, Cow::into_owned)),
                End(_) => return Ok(String::new()),
                Start(_) | Empty(_) => return Err(SparqlXml("Unexpected child".into())),
                CData(_) => return Err(SparqlXml("Unexpected CDATA".into())),
                _ => continue,
            }
        }
    }

    fn expect_closing(&mut self, name: QName<'_>) -> Result<(), Error> {
        loop {
            match self.events.read_event_into(&mut self.buf)? {
                End(e) if e.name() == name => return Ok(()),
                Start(_) | Empty(_) | CData(_) => {
                    return Err(SparqlXml(format!("Spurious content in {name:?}")))
                }
                Text(e) => {
                    if txt(e)?.is_some() {
                        return Err(SparqlXml(format!("Spurious text in {name:?}")));
                    }
                }
                _ => continue,
            }
        }
    }

    fn get_attr(&mut self, start: &BytesStart<'_>, key: &str) -> Result<Box<str>, Error> {
        self.get_attr_maybe(start, key)?
            .ok_or_else(|| SparqlXml(format!("Attribute '{key:?}' not found")))
    }

    fn get_attr_maybe(
        &mut self,
        start: &BytesStart<'_>,
        key: &str,
    ) -> Result<Option<Box<str>>, Error> {
        let key = QName(key.as_bytes());
        for res in start.attributes() {
            let attr = res.map_err(quick_xml::Error::from)?;
            if attr.key == key {
                return Ok(Some(
                    attr.decode_and_unescape_value(self.events.decoder())?
                        .into(),
                ));
            }
        }
        Ok(None)
    }

    fn check_element(&mut self, start: &BytesStart<'_>, local_name: &str) -> bool {
        let (ns, local) = self.events.resolve_element(start.name());
        local.as_ref() == local_name.as_bytes() && ns == ResolveResult::Bound(NS)
    }
}

fn txt(mut bytes_text: BytesText<'_>) -> Result<Option<Cow<'_, str>>, Error> {
    bytes_text.inplace_trim_start();
    if bytes_text.inplace_trim_end() {
        Ok(None)
    } else {
        Ok(Some(bytes_text.unescape()?))
    }
}

const NS: Namespace = Namespace(b"http://www.w3.org/2005/sparql-results#");

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bindings_doc() {
        let src = std::io::Cursor::new(
            r#"<?xml version="1.0"?>
            <sparql xmlns="http://www.w3.org/2005/sparql-results#">
              <head>
                <variable name="a"/>
                <variable name="b"/>
                <variable name="c"/>
              </head>
              <results>
                <result>
                  <binding name="a">
                    <uri>tag:a0</uri>
                  </binding>
                  <binding name="b">
                    <literal>simple</literal>
                  </binding>
                  <binding name="c">
                    <bnode>bn0</bnode>
                  </binding>
                </result>
                <result>
                  <binding name="c">
                    <literal datatype="tag:d1">datatype</literal>
                  </binding>
                  <binding name="a">
                    <literal xml:lang="en">lang</literal>
                  </binding>
                </result>
              </results>
            </sparql>
        "#,
        );
        let got = ResultsDocument::from_xml(src).unwrap();
        let exp = ResultsDocument::Bindings {
            doc: BindingsDocument {
                head: BindingsHead {
                    vars: vec!["a".into(), "b".into(), "c".into()],
                    link: vec![],
                },
                // results: Results {
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
        let src = std::io::Cursor::new(
            r#"<?xml version="1.0"?>
            <sparql xmlns="http://www.w3.org/2005/sparql-results#">
              <head></head>
              <boolean>true</boolean>
            </sparql>
        "#,
        );
        let got = ResultsDocument::from_xml(src).unwrap();
        let exp = ResultsDocument::Boolean {
            head: BooleanHead { link: vec![] },
            boolean: true,
        };
        assert_eq!(got, exp);
    }

    #[test]
    fn boolean_doc_with_link() {
        let src = std::io::Cursor::new(
            r#"<?xml version="1.0"?>
            <sparql xmlns="http://www.w3.org/2005/sparql-results#">
              <head>
                <link href="https://example.org" />
              </head>
              <boolean>false</boolean>
            </sparql>
        "#,
        );
        let got = ResultsDocument::from_xml(src).unwrap();
        let exp = ResultsDocument::Boolean {
            head: BooleanHead {
                link: vec!["https://example.org".into()],
            },
            boolean: false,
        };
        assert_eq!(got, exp);
    }
}
