use std::{collections::BTreeSet, iter::once, sync::Arc};

use rayon::iter::{
    IndexedParallelIterator, IntoParallelRefIterator, ParallelBridge, ParallelExtend,
    ParallelIterator,
};
use smallvec::SmallVec;
use sophia_api::{
    ns::{rdf, xsd},
    term::IriRef,
};

use crate::{
    _dedup::UsizeIteratorDedup, Inconsistency, InternalTerm, ReasonableGraph,
    d_entailment::Recognized, ruleset::RuleSet,
};

/// A [`RuleSet`] for [RDF semantics](https://www.w3.org/TR/rdf-semantics/#rdf_d_interpretations)
///
/// # Limitations
///
/// * `rdf:_2`, `rdf:_3`... should all have `rdf:type` `rdf:Property`;
///   only those present in the graph will effectively have it.
///
/// # Limitations regarding D-entailment
///
/// * inferences based on exhaustive enumeration of values is not supported,
///   such as
///   `:s :p true. :s :p false. :v a xsd:boolean.` ⊧ `:s :p :v.`
///
/// * inferences based on disjoint value spaces is not supported,
///   such as
///   `:v a xsd:integer. :v a xsd:boolean.` is inconsistent.
///
/// # Limitations for generalized graphs
///
/// The following limitations only impact generalized graphs,
/// as the unsupported entailments can not be expressed in strict RDF.
///
/// * under D-entailment, if datatype `:dt` is recognized,
///   any correctly typed literal `"lex"^^:dt` has `rdf:type :dt`.
///   This is currently only supported for literals that are otherwise used in the graph.
pub struct Rdf;

impl RuleSet for Rdf {
    fn prepare<D: Recognized>(graph: &mut ReasonableGraph<D, Self>) {
        prepare_rdf_vocab(graph);
        prepare_recognized_datatypes(graph);
        prepare_witnesses(graph);
    }
    fn saturate<D: Recognized>(graph: &mut ReasonableGraph<D, Self>) -> Result<(), Inconsistency> {
        let mut buf = vec![];
        buf.par_extend(rdf_types::<D, Self, false>(graph));
        graph.insert_all(&mut buf);

        check_recognized_datatypes(graph)
    }
}

pub(crate) fn prepare_rdf_vocab<D: Recognized, R: RuleSet>(graph: &mut ReasonableGraph<D, R>) {
    let res = graph.get_or_make_index(&rdf::type_);
    debug_assert_eq!(res.ok(), Some(RDF_TYPE));
    let res = graph.get_or_make_index(&rdf::reifies);
    debug_assert_eq!(res.ok(), Some(RDF_REIFIES));
    let res = graph.get_or_make_index(&rdf::subject);
    debug_assert_eq!(res.ok(), Some(RDF_SUBJECT));
    let res = graph.get_or_make_index(&rdf::predicate);
    debug_assert_eq!(res.ok(), Some(RDF_PREDICATE));
    let res = graph.get_or_make_index(&rdf::object);
    debug_assert_eq!(res.ok(), Some(RDF_OBJECT));
    let res = graph.get_or_make_index(&rdf::first);
    debug_assert_eq!(res.ok(), Some(RDF_FIRST));
    let res = graph.get_or_make_index(&rdf::rest);
    debug_assert_eq!(res.ok(), Some(RDF_REST));
    let res = graph.get_or_make_index(&rdf::value);
    debug_assert_eq!(res.ok(), Some(RDF_VALUE));
    let res = graph.get_or_make_index(&rdf::nil);
    debug_assert_eq!(res.ok(), Some(RDF_NIL));
    let res = graph.get_or_make_index(&rdf::List);
    debug_assert_eq!(res.ok(), Some(RDF_LIST));
    let res = graph.get_or_make_index(&rdf::Property);
    debug_assert_eq!(res.ok(), Some(RDF_PROPERTY));
    let res = graph.get_or_make_index(&IriRef::new_unchecked(format!("{}_1", rdf::PREFIX)));
    debug_assert_eq!(res.ok(), Some(RDF_1));
    // axioms
    graph.insert([RDF_TYPE, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_SUBJECT, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_PREDICATE, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_OBJECT, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_REIFIES, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_FIRST, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_REST, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_VALUE, RDF_TYPE, RDF_PROPERTY]);
    graph.insert([RDF_NIL, RDF_TYPE, RDF_LIST]);
    graph.insert([RDF_1, RDF_TYPE, RDF_PROPERTY]);
}

pub(crate) fn prepare_recognized_datatypes<D: Recognized, R: RuleSet>(
    graph: &mut ReasonableGraph<D, R>,
) {
    let dtmin = graph.i2t.len();
    graph.get_or_make_index(&rdf::langString).unwrap();
    debug_assert_eq!(graph.i2t.len(), dtmin + 1);
    graph.get_or_make_index(&rdf::dirLangString).unwrap();
    debug_assert_eq!(graph.i2t.len(), dtmin + 2);
    graph.get_or_make_index(&xsd::string).unwrap();
    debug_assert_eq!(graph.i2t.len(), dtmin + 3);
    D::datatypes().for_each(|iri| {
        graph.get_or_make_index(&iri).unwrap();
    });
    debug_assert_eq!(graph.i2t.len(), dtmin + 3 + D::datatypes().count());
    graph.rdt = dtmin..graph.i2t.len();
}

pub(crate) fn prepare_witnesses<D: Recognized, R: RuleSet>(graph: &mut ReasonableGraph<D, R>) {
    for (lex, dt) in D::witnesses() {
        let idt = graph.get_or_make_index(&dt).unwrap();
        let lit = Arc::new(InternalTerm::TypedLiteral(lex.into(), idt));
        graph.t2i.entry(lit).or_insert_with_key(|lit| {
            let ret = graph.i2t.len();
            graph.i2t.push(lit.clone());
            ret
        });
    }
}

pub(crate) fn rdf_types<D: Recognized, R: RuleSet, const RDFS: bool>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    let rdf_lang_string = graph.get_index(&rdf::langString).unwrap();
    let rdf_dir_lang_string = graph.get_index(&rdf::dirLangString).unwrap();
    let xsd_string = graph.get_index(&xsd::string).unwrap();
    graph
        .i2t
        .par_iter()
        .enumerate()
        .flat_map_iter(move |(i, t)| -> SmallVec<[[usize; 3]; 16]> {
            let mut v = match t.as_ref() {
                InternalTerm::TypedLiteral(lex, idt) => {
                    // add rdf:type {datatype} for all recognized datatypes
                    let InternalTerm::Iri(datatype) = graph.i2t[*idt].as_ref() else {
                        unreachable!()
                    };
                    match D::datatypes_for(lex, datatype.as_ref()) {
                        Some(v) => {
                            let mut v: SmallVec<_> = v
                                .into_iter()
                                .map(|dt| [i, RDF_TYPE, graph.get_index(&dt).unwrap()])
                                .collect();
                            v.push([i, RDF_TYPE, *idt]);
                            v
                        }
                        None => {
                            if *idt == xsd_string {
                                SmallVec::from_elem([i, RDF_TYPE, xsd_string], 1)
                            } else {
                                SmallVec::new()
                            }
                        }
                    }
                }
                InternalTerm::LangString(_, _, dir) => {
                    // add rdf:type {datatype} for language strings
                    if dir.is_some() {
                        SmallVec::from_elem([i, RDF_TYPE, rdf_dir_lang_string], 1)
                    } else {
                        SmallVec::from_elem([i, RDF_TYPE, rdf_lang_string], 1)
                    }
                }
                InternalTerm::TripleTerm([_, p, _]) => {
                    // add rdf:type rdf:Property to all predicate used in triple terms
                    let mut v = SmallVec::from_elem([*p, RDF_TYPE, RDF_PROPERTY], 1);
                    if RDFS {
                        v.push([i, RDF_TYPE, super::_rdfs::RDFS_PROPOSITION]);
                    }
                    v
                }
                _ => SmallVec::new(),
            };
            if RDFS {
                v.push([i, RDF_TYPE, super::_rdfs::RDFS_RESOURCE])
            }
            v
        })
        .chain(
            // add rdf:type rdf:Property to all predicate used in asserted triples
            graph
                .pos
                .iter()
                .map(|[p, ..]| *p)
                .dedup()
                .map(|p| [p, RDF_TYPE, RDF_PROPERTY])
                .par_bridge(),
        )
}

/// Checks that no resources has rdf:types D1 and D2 where D1 and D2 are disjoint recognized datatypes.
///
/// To determine which datatypes are disjoint,
/// we are inspecting the endorsed datatypes (via [`D::datatypes_for`]) of all [`D::witnesses`],
/// since the contract requires that any pair of overlapping datatypes must have a witness.
pub(crate) fn check_recognized_datatypes<D: Recognized, R: RuleSet>(
    graph: &mut ReasonableGraph<D, R>,
) -> Result<(), Inconsistency> {
    let disjoint = compute_disjoint_datatypes(graph);
    let instance_of_disjoint_datatypes = graph
        .pos
        .range([RDF_TYPE, graph.rdt.start, 0]..[RDF_TYPE, graph.rdt.end, 0])
        .par_bridge()
        .flat_map_iter(|[_, dt1, s]| {
            graph
                .spo
                .range([*s, RDF_TYPE, *dt1 + 1]..[*s, RDF_TYPE, graph.rdt.end])
                .filter_map(|[_, _, dt2]| disjoint.get(&(*dt1, *dt2)).copied())
        })
        .find_map_any(|(dt1, dt2)| {
            let InternalTerm::Iri(iri1) = graph.i2t[dt1].as_ref() else {
                unreachable!()
            };
            let InternalTerm::Iri(iri2) = graph.i2t[dt2].as_ref() else {
                unreachable!()
            };
            Some(format!(
                "instance of disjoint datatypes <{iri1}> and <{iri2}>"
            ))
        });
    if let Some(msg) = instance_of_disjoint_datatypes {
        Err(Inconsistency::Other(msg))
    } else {
        Ok(())
    }
}

fn compute_disjoint_datatypes<D: Recognized, R: RuleSet>(
    graph: &mut ReasonableGraph<D, R>,
) -> BTreeSet<(usize, usize)> {
    let mut disjoint: BTreeSet<_> = graph
        .rdt
        .clone()
        .flat_map(|i| ((i + 1)..graph.rdt.end).map(move |j| (i, j)))
        .collect();
    for w in D::witnesses() {
        let wdts: Vec<_> = once(w.1.clone())
            .chain(D::datatypes_for(&w.0, w.1.as_ref()).unwrap())
            .map(|t| graph.get_index(&t).unwrap())
            .collect();
        for overlapping in wdts.iter().copied().flat_map(|i| {
            wdts.iter()
                .copied()
                .filter_map(move |j| (i < j).then_some((i, j)))
        }) {
            disjoint.remove(&overlapping);
        }
    }
    disjoint
}

pub(crate) const RDF_TYPE: usize = 0;
pub(crate) const RDF_REIFIES: usize = 1;
pub(crate) const RDF_SUBJECT: usize = 2;
pub(crate) const RDF_PREDICATE: usize = 3;
pub(crate) const RDF_OBJECT: usize = 4;
pub(crate) const RDF_FIRST: usize = 5;
pub(crate) const RDF_REST: usize = 6;
pub(crate) const RDF_VALUE: usize = 7;
pub(crate) const RDF_NIL: usize = 8;
pub(crate) const RDF_LIST: usize = 9;
pub(crate) const RDF_PROPERTY: usize = 10;
pub(crate) const RDF_1: usize = 11;
