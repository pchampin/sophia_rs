//! Serializer for the [TriG] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [TriG]: https://www.w3.org/TR/trig/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use super::rio_common::rio_format_quads;
use super::turtle::{prettify, write_prefixes, write_term};
use crate::dataset::{Dataset, MutableDataset};
use crate::dataset::indexed::IndexedDataset;
use crate::dataset::inmem::FastDataset;
use rio_turtle::TriGFormatter;
use sophia_api::quad::stream::{SinkError, SourceError, StreamResult, QuadSource};
use sophia_api::quad::Quad;
use sophia_api::serializer::*;
use sophia_api::term::{TermKind::BlankNode, TTerm};
use sophia_term::{RcTerm};
use std::collections::{HashMap, HashSet};
use std::io;

/// The TriG serializer uses the same configuration type as the Turtle parser.
pub type TrigConfig = super::turtle::TurtleConfig;

/// TriG serialization uses the same underlying struct as Turtle serialization.
pub type TrigSerializer<W> = super::turtle::TurtleSerializer<W>;

impl<W> QuadSerializer for TrigSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_quads<QS>(
        &mut self,
        mut source: QS,
    ) -> StreamResult<&mut Self, QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        if self.config.pretty {
            let mut dataset = FastDataset::new();
            let mut graph_names = HashSet::new();
            let mut bnode_graph = HashMap::<RcTerm, Option<RcTerm>>::new();
            let mut anon_blacklist = HashSet::new();
            source
                .for_each_quad(|q| {
                    dataset
                        .insert(q.s(), q.p(), q.o(), q.g())
                        .unwrap();
                    // build graph_names and anon_blacklist
                    let gn = q.g().map(|t| get_rcterm(t, &dataset) );
                    for t in [q.s(), q.p(), q.o()] {
                        if t.kind() != BlankNode {
                            continue;
                        } 
                        let t = get_rcterm(t, &dataset);
                        if anon_blacklist.contains(&t) {
                            continue;
                        }
                        match bnode_graph.get(&t) {
                            None => {
                                if graph_names.contains(&t) {
                                    anon_blacklist.insert(t);
                                } else {
                                    bnode_graph.insert(t, gn.clone());
                                }
                            }
                            Some(gn2) if &gn != gn2 => {
                                anon_blacklist.insert(t);
                            }
                            _ => ()
                        }            
                    }
                    if let Some(term) = gn {
                        if term.kind() == BlankNode && bnode_graph.get(&term).is_some() {
                            anon_blacklist.insert(term.clone());
                        }
                        graph_names.insert(term);
                    }
                })
                .map_err(SourceError)?;

            write_prefixes(&mut self.write, &self.config.prefix_map[..]).map_err(SinkError)?;
            prettify(dataset.graph(None), &mut self.write, &self.config, &anon_blacklist, "")
                .map_err(SinkError)?;
            for gn in &graph_names {
                let allow_anon = !anon_blacklist.contains(gn);
                self.write.write_all(b"GRAPH ").map_err(SinkError)?;
                write_term(&mut self.write, gn, &self.config, allow_anon).map_err(SinkError)?;
                self.write.write_all(b" {").map_err(SinkError)?;
                prettify(dataset.graph(Some(gn)), &mut self.write, &self.config, &anon_blacklist, self.config.indentation()).map_err(SinkError)?;
                self.write.write_all(b"}").map_err(SinkError)?;
            }
            self.write.flush().map_err(SinkError)?;
        } else {
            let mut tf = TriGFormatter::new(&mut self.write);
            rio_format_quads(&mut tf, source)?;
            tf.finish().map_err(SinkError)?;
        }
        Ok(self)
    }
}

/// For a term that we know belongs to dataset,
/// return a clone of the corresponding RcTerm used in dataset.
fn get_rcterm<T: TTerm + ?Sized>(term: &T, dataset: &FastDataset) -> RcTerm {
    let i = dataset.get_index(term).unwrap();
    let t = dataset.get_term(i).unwrap();
    t.clone()
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use crate::dataset::inmem::FastDataset;
    use super::*;
    use sophia_api::dataset::{Dataset, isomorphic_datasets};
    use sophia_term::*;
    use std::error::Error;

    const TESTS: &'static [&str] = &[
        "#empty trig",
        r#"# simple quads
            PREFIX : <http://example.org/ns/>
            :alice a :Person; :name "Alice"; :age 42.

            GRAPH :g {
                :bob a :Person, :Man; :nick "bob"@fr, "bobby"@en; :admin true.
            }
        "#,
        r#"# lists
            GRAPH <tag:g> { <tag:alice> <tag:likes> ( 1 2 ( 3 4 ) 5 6 ), ("a" "b"). }
        "#,
        r#"# subject lists
            GRAPH <tag:g> { (1 2 3) a <tag:List>. }
        "#,
        r#"# blank node graph name
            PREFIX : <http://example.org/ns/>
            #:lois :belives _:b.
            #GRAPH _:b1 { :clark a :Human }
        "#,
        r#"# list split over different graphs
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            _:a rdf:first 42; rdf:rest _:b.

            GRAPH [] {
                _:b rdf:first 43; rdf:rest ().
            }
        "#,
    ];

    #[test]
    fn roundtrip_not_pretty() -> Result<(), Box<dyn std::error::Error>> {
        for ttl in TESTS {
            println!("==========\n{}\n----------", ttl);
            let g1: FastDataset = crate::parser::trig::parse_str(ttl).collect_quads()?;

            let out = TrigSerializer::new_stringifier().serialize_quads(g1.quads())?.to_string();
            println!("{}", &out);

            let g2: FastDataset = crate::parser::trig::parse_str(&out).collect_quads()?;

            assert!(isomorphic_datasets(&g1, &g2)?);
        }
        Ok(())
    }

    #[test]
    fn roundtrip_pretty() -> Result<(), Box<dyn Error>> {
        for ttl in TESTS {
            println!("==========\n{}\n----------", ttl);
            let g1: FastDataset = crate::parser::trig::parse_str(ttl).collect_quads()?;

            let config = TrigConfig::new().with_pretty(true);
            let out = TrigSerializer::new_stringifier_with_config(config).serialize_quads(g1.quads())?.to_string();
            println!("{}", &out);

            let g2: FastDataset = crate::parser::trig::parse_str(&out).collect_quads()?;

            assert!(isomorphic_datasets(&g1, &g2)?);
        }
        Ok(())
    }
}
