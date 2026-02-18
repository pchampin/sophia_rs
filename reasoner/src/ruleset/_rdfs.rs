use std::sync::{Arc, LazyLock};

use rayon::iter::{
    IndexedParallelIterator, IntoParallelRefIterator, ParallelBridge, ParallelExtend,
    ParallelIterator,
};
use regex::Regex;
use sophia_api::ns::{rdf, rdfs};

use crate::{
    _range_n::RangeN, Inconsistency, InternalTerm, ReasonableGraph, d_entailment::Recognized,
    ruleset::RuleSet,
};

use super::_rdf::*;

/// A [`RuleSet`] for [RDFS semantics](https://www.w3.org/TR/rdf-semantics/#rdfs-interpretations)
///
/// # Limitations
///
/// All those of [`Rdf`](super::Rdf), plus
///
/// * any IRI (or literal, in generalized RDF) should have `rdf:type` `rdfs:Resource`,
///   but this implementation only infers it for terms used in the graph
/// * any triple term should have `rdf:type` `rdfs:Proposition`,
///   but this implementation only infers it for triple terms used in the graph
///
/// ## Limitations regarding D-entailment
///
/// * range clash for literals are not detected; for example
///   `:p rdfs:range rdf:langString.  [] :p "foo".` is inconsistent,
///   but this implementation does not detect it.
pub struct Rdfs;

impl RuleSet for Rdfs {
    fn prepare<D: Recognized>(graph: &mut ReasonableGraph<D, Self>) {
        prepare_rdf_vocab(graph);
        prepare_rdfs_vocab(graph);
        prepare_recognized_datatypes(graph);
        prepare_witnesses(graph);
        prepare_proposition_witness(graph);

        let mut buf = vec![];
        buf.par_extend(rdfs1(graph));
        graph.insert_all(&mut buf);
    }
    fn saturate<D: Recognized>(graph: &mut ReasonableGraph<D, Self>) -> Result<(), Inconsistency> {
        let mut buf = vec![];

        buf.par_extend(rdf_types::<D, Self, true>(graph));
        graph.insert_all(&mut buf);

        buf.par_extend(rdfs_membership_properties(graph));
        graph.insert_all(&mut buf);

        let mut changed = true;
        while changed {
            changed = false;

            buf.par_extend(rdfs12(graph)); // rdfs:ContainerMembershipProperty → rdfs:member
            changed = changed || graph.insert_all(&mut buf);

            buf.par_extend(rdfs13(graph)); // rdfs:Datatype → rdfs:Literal
            changed = changed || graph.insert_all(&mut buf);

            buf.par_extend(rdfs2(graph)); // rdfs:domain
            changed = changed || graph.insert_all(&mut buf);

            buf.par_extend(rdfs3(graph)); // rdfs:range
            changed = changed || graph.insert_all(&mut buf);

            let mut spo = true;
            while spo {
                buf.par_extend(rdfs5(graph)); // transitive closure rdfs:subPropertyOf
                spo = graph.insert_all(&mut buf);
                changed = changed || spo;
            }

            buf.par_extend(rdfs6(graph)); // reflexive closure rdfs:subPropertyOf
            changed = changed || graph.insert_all(&mut buf);

            buf.par_extend(rdfs7(graph)); // rdfs:subPropertyOf
            changed = changed || graph.insert_all(&mut buf);

            let mut spo = true;
            while spo {
                buf.par_extend(rdfs11(graph)); // transitive closure rdfs:subClassOf
                spo = graph.insert_all(&mut buf);
                changed = changed || spo;
            }

            buf.par_extend(rdfs8_rdfs10(graph)); // classes subclassOf rdfs:Resource and themselves
            changed = changed || graph.insert_all(&mut buf);

            buf.par_extend(rdfs9(graph)); // rdfs:subClassOf
            changed = changed || graph.insert_all(&mut buf);
        }
        Ok(())
    }
}

pub(crate) fn prepare_rdfs_vocab<D: Recognized, R: RuleSet>(graph: &mut ReasonableGraph<D, R>) {
    let res = graph.get_or_make_index(&rdf::Alt);
    debug_assert_eq!(res.ok(), Some(RDF_ALT));
    let res = graph.get_or_make_index(&rdf::Bag);
    debug_assert_eq!(res.ok(), Some(RDF_BAG));
    let res = graph.get_or_make_index(&rdf::Seq);
    debug_assert_eq!(res.ok(), Some(RDF_SEQ));
    let res = graph.get_or_make_index(&rdf::Statement);
    debug_assert_eq!(res.ok(), Some(RDF_STATEMENT));

    let res = graph.get_or_make_index(&rdfs::domain);
    debug_assert_eq!(res.ok(), Some(RDFS_DOMAIN));
    let res = graph.get_or_make_index(&rdfs::range);
    debug_assert_eq!(res.ok(), Some(RDFS_RANGE));
    let res = graph.get_or_make_index(&rdfs::Resource);
    debug_assert_eq!(res.ok(), Some(RDFS_RESOURCE));
    let res = graph.get_or_make_index(&rdfs::Literal);
    debug_assert_eq!(res.ok(), Some(RDFS_LITERAL));
    let res = graph.get_or_make_index(&rdfs::Datatype);
    debug_assert_eq!(res.ok(), Some(RDFS_DATATYPE));
    let res = graph.get_or_make_index(&rdfs::Class);
    debug_assert_eq!(res.ok(), Some(RDFS_CLASS));
    let res = graph.get_or_make_index(&rdfs::subClassOf);
    debug_assert_eq!(res.ok(), Some(RDFS_SUB_CLASS_OF));
    let res = graph.get_or_make_index(&rdfs::subPropertyOf);
    debug_assert_eq!(res.ok(), Some(RDFS_SUB_PROPERTY_OF));
    let res = graph.get_or_make_index(&rdfs::Proposition);
    debug_assert_eq!(res.ok(), Some(RDFS_PROPOSITION));
    let res = graph.get_or_make_index(&rdfs::member);
    debug_assert_eq!(res.ok(), Some(RDFS_MEMBER));
    let res = graph.get_or_make_index(&rdfs::Container);
    debug_assert_eq!(res.ok(), Some(RDFS_CONTAINER));
    let res = graph.get_or_make_index(&rdfs::ContainerMembershipProperty);
    debug_assert_eq!(res.ok(), Some(RDFS_CONTAINER_MEMBERSHIP_PROPERTY));
    let res = graph.get_or_make_index(&rdfs::comment);
    debug_assert_eq!(res.ok(), Some(RDFS_COMMENT));
    let res = graph.get_or_make_index(&rdfs::seeAlso);
    debug_assert_eq!(res.ok(), Some(RDFS_SEE_ALSO));
    let res = graph.get_or_make_index(&rdfs::isDefinedBy);
    debug_assert_eq!(res.ok(), Some(RDFS_IS_DEFINED_BY));
    let res = graph.get_or_make_index(&rdfs::label);
    debug_assert_eq!(res.ok(), Some(RDFS_LABEL));
    // axioms
    graph.insert([RDF_TYPE, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDF_REIFIES, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDFS_DOMAIN, RDFS_DOMAIN, RDF_PROPERTY]);
    graph.insert([RDFS_RANGE, RDFS_DOMAIN, RDF_PROPERTY]);
    graph.insert([RDFS_SUB_PROPERTY_OF, RDFS_DOMAIN, RDF_PROPERTY]);
    graph.insert([RDFS_SUB_CLASS_OF, RDFS_DOMAIN, RDFS_CLASS]);
    graph.insert([RDF_SUBJECT, RDFS_DOMAIN, RDF_STATEMENT]);
    graph.insert([RDF_PREDICATE, RDFS_DOMAIN, RDF_STATEMENT]);
    graph.insert([RDF_OBJECT, RDFS_DOMAIN, RDF_STATEMENT]);
    graph.insert([RDFS_MEMBER, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDF_FIRST, RDFS_DOMAIN, RDF_LIST]);
    graph.insert([RDF_REST, RDFS_DOMAIN, RDF_LIST]);
    graph.insert([RDFS_SEE_ALSO, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDFS_IS_DEFINED_BY, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDFS_COMMENT, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDFS_LABEL, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDF_VALUE, RDFS_DOMAIN, RDFS_RESOURCE]);

    graph.insert([RDF_TYPE, RDFS_RANGE, RDFS_CLASS]);
    graph.insert([RDF_REIFIES, RDFS_RANGE, RDFS_PROPOSITION]);
    graph.insert([RDFS_DOMAIN, RDFS_RANGE, RDFS_CLASS]);
    graph.insert([RDFS_RANGE, RDFS_RANGE, RDFS_CLASS]);
    graph.insert([RDFS_SUB_PROPERTY_OF, RDFS_RANGE, RDF_PROPERTY]);
    graph.insert([RDFS_SUB_CLASS_OF, RDFS_RANGE, RDFS_CLASS]);
    graph.insert([RDF_SUBJECT, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDF_PREDICATE, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDF_OBJECT, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDFS_MEMBER, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDF_FIRST, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDF_REST, RDFS_RANGE, RDF_LIST]);
    graph.insert([RDFS_SEE_ALSO, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDFS_IS_DEFINED_BY, RDFS_RANGE, RDFS_RESOURCE]);
    graph.insert([RDFS_COMMENT, RDFS_RANGE, RDFS_LITERAL]);
    graph.insert([RDFS_LABEL, RDFS_RANGE, RDFS_LITERAL]);
    graph.insert([RDF_VALUE, RDFS_RANGE, RDFS_RESOURCE]);

    graph.insert([RDF_ALT, RDFS_SUB_CLASS_OF, RDFS_CONTAINER]);
    graph.insert([RDF_BAG, RDFS_SUB_CLASS_OF, RDFS_CONTAINER]);
    graph.insert([RDF_SEQ, RDFS_SUB_CLASS_OF, RDFS_CONTAINER]);
    graph.insert([
        RDFS_CONTAINER_MEMBERSHIP_PROPERTY,
        RDFS_SUB_CLASS_OF,
        RDF_PROPERTY,
    ]);

    graph.insert([RDFS_IS_DEFINED_BY, RDFS_SUB_PROPERTY_OF, RDFS_SEE_ALSO]);

    graph.insert([RDFS_DATATYPE, RDFS_SUB_CLASS_OF, RDFS_CLASS]);

    graph.insert([RDF_1, RDF_TYPE, RDFS_CONTAINER_MEMBERSHIP_PROPERTY]);
    graph.insert([RDF_1, RDFS_DOMAIN, RDFS_RESOURCE]);
    graph.insert([RDF_1, RDFS_RANGE, RDFS_RESOURCE]);
}

pub(crate) fn prepare_proposition_witness<D: Recognized, R: RuleSet>(
    graph: &mut ReasonableGraph<D, R>,
) {
    let tt = Arc::new(InternalTerm::TripleTerm([RDF_TYPE, RDF_TYPE, RDF_PROPERTY]));
    graph.t2i.entry(tt).or_insert_with_key(|lit| {
        let ret = graph.i2t.len();
        graph.i2t.push(lit.clone());
        ret
    });
}

/// add axiomatic triples for all used membership properties
pub(crate) fn rdfs_membership_properties<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    static RE: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new("^http://www.w3.org/1999/02/22-rdf-syntax-ns#_[0-9]+$").unwrap()
    });
    graph
        .i2t
        .par_iter()
        .enumerate()
        .filter_map(|(i, t)| {
            if let InternalTerm::Iri(iri) = t.as_ref()
                && RE.is_match(iri.as_str())
            {
                Some(i)
            } else {
                None
            }
        })
        .flat_map_iter(|i| {
            [
                [i, RDF_TYPE, RDFS_CONTAINER_MEMBERSHIP_PROPERTY],
                [i, RDFS_DOMAIN, RDFS_RESOURCE],
                [i, RDFS_RANGE, RDFS_RESOURCE],
            ]
        })
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs1
pub(crate) fn rdfs1<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    D::datatypes()
        .par_bridge()
        .map(|iriref| [graph.get_index(&iriref).unwrap(), RDF_TYPE, RDFS_DATATYPE])
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs2
pub(crate) fn rdfs2<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range1(RDFS_DOMAIN)
        .par_bridge()
        .flat_map_iter(|[_, c, p]| graph.pos.range1(*p).map(|[_, _, s]| [*s, RDF_TYPE, *c]))
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs3
pub(crate) fn rdfs3<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range1(RDFS_RANGE)
        .par_bridge()
        .flat_map_iter(|[_, c, p]| graph.pos.range1(*p).map(|[_, o, _]| [*o, RDF_TYPE, *c]))
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs5
pub(crate) fn rdfs5<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range1(RDFS_SUB_PROPERTY_OF)
        .par_bridge()
        .filter(|[_, p3, p2]| p2 != p3)
        .flat_map_iter(|[_, p3, p2]| {
            graph
                .pos
                .range2(RDFS_SUB_PROPERTY_OF, *p2)
                .filter_map(move |[_, _, p1]| {
                    (p1 != p2 && p1 != p3).then_some([*p1, RDFS_SUB_PROPERTY_OF, *p3])
                })
        })
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs6
pub(crate) fn rdfs6<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range2(RDF_TYPE, RDF_PROPERTY)
        .par_bridge()
        .map(|[_, _, s]| [*s, RDFS_SUB_PROPERTY_OF, *s])
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs7
pub(crate) fn rdfs7<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range1(RDFS_SUB_PROPERTY_OF)
        .par_bridge()
        .filter(|[_, p2, p1]| p1 != p2)
        .flat_map_iter(|[_, p2, p1]| graph.pos.range1(*p1).map(move |[_, o, s]| [*s, *p2, *o]))
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs8
/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs10
pub(crate) fn rdfs8_rdfs10<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range2(RDF_TYPE, RDFS_CLASS)
        .par_bridge()
        .flat_map_iter(|[_, _, c]| {
            [
                [*c, RDFS_SUB_CLASS_OF, *c],
                [*c, RDFS_SUB_CLASS_OF, RDFS_RESOURCE],
            ]
        })
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs9
pub(crate) fn rdfs9<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range1(RDFS_SUB_CLASS_OF)
        .par_bridge()
        .filter(|[_, c2, c1]| c1 != c2)
        .flat_map_iter(|[_, c2, c1]| {
            graph
                .pos
                .range2(RDF_TYPE, *c1)
                .map(move |[_, _, s]| [*s, RDF_TYPE, *c2])
        })
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs11
pub(crate) fn rdfs11<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range1(RDFS_SUB_CLASS_OF)
        .par_bridge()
        .filter(|[_, c3, c2]| c2 != c3)
        .flat_map_iter(|[_, c3, c2]| {
            graph
                .pos
                .range2(RDFS_SUB_CLASS_OF, *c2)
                .filter_map(move |[_, _, c1]| {
                    (c1 != c2 && c1 != c3).then_some([*c1, RDFS_SUB_CLASS_OF, *c3])
                })
        })
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs12
pub(crate) fn rdfs12<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range2(RDF_TYPE, RDFS_CONTAINER_MEMBERSHIP_PROPERTY)
        .par_bridge()
        .map(|[_, _, s]| [*s, RDFS_SUB_PROPERTY_OF, RDFS_MEMBER])
}

/// https://www.w3.org/TR/rdf12-semantics/#dfn-rdfs13
pub(crate) fn rdfs13<D: Recognized, R: RuleSet>(
    graph: &ReasonableGraph<D, R>,
) -> impl ParallelIterator<Item = [usize; 3]> {
    graph
        .pos
        .range2(RDF_TYPE, RDFS_DATATYPE)
        .par_bridge()
        .map(|[_, _, s]| [*s, RDFS_SUB_CLASS_OF, RDFS_LITERAL])
}

// terms in the RDF ns that only have a meaning in RDFS
pub(crate) const RDF_ALT: usize = 15;
pub(crate) const RDF_BAG: usize = 16;
pub(crate) const RDF_SEQ: usize = 17;
pub(crate) const RDF_STATEMENT: usize = 18;

pub(crate) const RDFS_DOMAIN: usize = 19;
pub(crate) const RDFS_RANGE: usize = 20;
pub(crate) const RDFS_RESOURCE: usize = 21;
pub(crate) const RDFS_LITERAL: usize = 22;
pub(crate) const RDFS_DATATYPE: usize = 23;
pub(crate) const RDFS_CLASS: usize = 24;
pub(crate) const RDFS_SUB_CLASS_OF: usize = 25;
pub(crate) const RDFS_SUB_PROPERTY_OF: usize = 26;
pub(crate) const RDFS_PROPOSITION: usize = 27;
pub(crate) const RDFS_MEMBER: usize = 28;
pub(crate) const RDFS_CONTAINER: usize = 29;
pub(crate) const RDFS_CONTAINER_MEMBERSHIP_PROPERTY: usize = 30;
pub(crate) const RDFS_COMMENT: usize = 31;
pub(crate) const RDFS_SEE_ALSO: usize = 32;
pub(crate) const RDFS_IS_DEFINED_BY: usize = 33;
pub(crate) const RDFS_LABEL: usize = 34;
