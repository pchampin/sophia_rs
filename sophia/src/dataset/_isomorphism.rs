//! This module implements check for isomorphic blank node equivalence of RDF
//! datasets.
//!
//! It is publicly exported to `sophia::dataset`.

use crate::dataset::{DTerm, DQuad, Dataset};
use crate::triple::stream::{
    SinkError, SinkResult as _, SourceError, SourceResult as _, StreamError, StreamResult,
};
use crate::quad::Quad;
use crate::graph::{hash_if_not_bn, match_ignore_bns, bn_mapper};
use sophia_term::{RefTerm, Term, TermData, same_graph_name};
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};

/// Maximal steps a graph is traversed for proofing isomorphism.
/// If this bound is exceeded the algorithm assumes that the graphs are not
/// isomorphic.
pub const MAX_DISTANCE: usize = 8;

/// The hasher used internally for checking isomorphism.
pub type IsoHasher = std::collections::hash_map::DefaultHasher;

#[derive(Debug, Clone, Copy)]
struct AlgorithmFailure;

impl fmt::Display for AlgorithmFailure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to execute the Algorithm")
    }
}

impl Error for AlgorithmFailure {}

/// Checks if both graphs are isomorphic blank node equal.
///
/// According to the [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism)
/// this means that a mapping for blank nodes in `g1` exists so that `g1 == g2`.
///
/// The used algorithm was originally implemented for [`Oxigraph`](https://github.com/Tpt/oxigraph)
/// and is extended for the generalized RDF model of `sophia`.
///
/// # Errors
///
/// Both graphs can fail traversing (and this is done several times),
/// accordingly, a `StreamError` returned where `SourceError`s originate from
/// `g1` and `SinkError`s originate from `g2`
///
/// # Performance
///
/// As this algorithm has to traverse each graph several times the algorithm
/// gets way more expensive with bigger numbers of triples. In the same way
/// the number of blank nodes contributes to the costs.
pub fn isomorphic_datasets<D1, D2>(d1: &D1, d2: &D2) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
{
    // quick return conditions
    // -----------------------
    // blank nodes in the respective datasets
    let bns1 = d1.bnodes().source_err()?;
    let bns2 = d2.bnodes().sink_err()?;

    if bns1.len() != bns2.len() {
        return Ok(false);
    }

    // check for same quads in both datasets
    // -------------------------------------
    // - regardless of blank nodes
    // - implicitly checks that d1 and d2 have the same length
    let d1_in_d2 = check_for_equal_quads_regardless_bns(d1, d2)?;
    let d2_in_d1 = check_for_equal_quads_regardless_bns(d2, d1).map_err(StreamError::reverse)?;

    if !(d1_in_d2 && d2_in_d1) {
        return Ok(false);
    }

    // Create hashes
    let bn_hashes1 = match calc_bn_hashes::<D1, IsoHasher>(d1) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SourceError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };
    let bn_hashes2 = match calc_bn_hashes::<D2, IsoHasher>(d2) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SinkError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };

    // Create mapping
    let mut bn_mapping = HashMap::new();
    for (hash, bns1) in bn_hashes1 {
        for bn1 in bns1 {
            let bn2 = match bn_hashes2.get(&hash) {
                Some(bn) => bn,
                None => return Ok(false), // No matching blank node in g2!
            };
            bn_mapping.insert(bn1, bn2);
        }
    }

    // Apply mapping
    isomorphic_datasets_with_mapping(d1, d2, bn_mapping)
}

/// Builds a `TermMatcher` by using the blank node mapping provided.
///
/// If the given term is a blank node the matcher will match all possible
/// mappings for that blank node, i.e. included redundant blank nodes. If the
/// given term is not a blank node the matcher will only match the given term.
///
/// This aligns with the description of bijection _M_ described in the
/// [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism).
fn bn_mapper_for_gname<'m, TD1, TD2>(
    mapping: &'m HashMap<Term<TD1>, &Vec<Term<TD2>>>,
    g: Option<&'m Term<TD1>>,
) -> Box<dyn 'm + Fn(Option<&RefTerm>) -> bool>
where
    TD1: TermData,
    TD2: TermData,
{
    if let Some(Term::BNode(_)) = g {
        Box::new(move |other: Option<&RefTerm>| {
            let mapped = match mapping.get(g.unwrap()) {
                Some(bns) => bns,
                None => return false,
            };

            mapped.iter().any(|t| same_graph_name(Some(t), other))
        }) as _
    } else {
        let g = g.clone();
        Box::new(move |other: Option<&RefTerm>| same_graph_name(g, other)) as _
    }
}

/// Checks for each triple in `g1` with at least one blank node if it is also
/// contained in `g2` if the blank node `mapping` is applied.
fn isomorphic_datasets_with_mapping<D1, D2>(
    d1: &D1,
    d2: &D2,
    mapping: HashMap<DTerm<D1>, &Vec<DTerm<D2>>>,
) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
{
    for q in d1.quads() {
        let q = q.source_err()?;

        if matches!(q.s(), Term::BNode(_))
            || matches!(q.p(), Term::BNode(_))
            || matches!(q.o(), Term::BNode(_))
            || matches!(q.g(), Option::Some(&Term::BNode(_)))
        {
            let ms = bn_mapper(&mapping, q.s());
            let mp = bn_mapper(&mapping, q.p());
            let mo = bn_mapper(&mapping, q.o());
            let mg = bn_mapper_for_gname(&mapping, q.g());

            if d2.quads_matching(&ms, &mp, &mo, &mg).next().is_none() {
                return Ok(false);
            }
        }
    }

    Ok(true)
}

fn match_gname_ignore_bns<'t, TD>(
    t: Option<&'t Term<TD>>,
) -> Box<dyn 't + Fn(Option<&RefTerm>) -> bool>
where
    TD: TermData,
{
    if let Some(Term::BNode(_)) = t {
        Box::new(move |_: Option<&RefTerm>| true) as _
    } else {
        let t = t.clone();
        Box::new(move |other: Option<&RefTerm>| same_graph_name(t, other)) as _
    }
}

/// Checks is each triple in `g1` is also in `g2` regardless of blank node
/// labels.
///
/// # Example
///
/// Assume there are two graphs `g1`:
///
/// ```text
/// _:s1 :p :o .
/// _:s1 :p 42 .
/// ```
///
/// and `g2`:
///
/// ```text
/// _:s2 :p :o .
/// _:s2 :p 21 .
/// ```
///
/// The first triple passes the check as it searches for a triples with
/// predicate `:p` and object `:o` in `g2`. The second triple fails.
///
/// However, this is still not enough to proof isomorphism.
fn check_for_equal_quads_regardless_bns<D1, D2>(
    d1: &D1,
    d2: &D2,
) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
{
    for q in d1.quads() {
        let q = q.source_err()?;

        let ms = match_ignore_bns(q.s());
        let mp = match_ignore_bns(q.p());
        let mo = match_ignore_bns(q.o());
        let mg = match_gname_ignore_bns(q.g());

        if d2.quads_matching(&ms, &mp, &mo, &mg).next().is_none() {
            return Ok(false);
        }
    }

    Ok(true)
}

/// Calculate a hash for each blank node.
///
/// The hash of a blank node in a graph with `distance == 0` is the hash of all
/// terms in the triples in which the blank node occurs. Should this not be
/// enough to create distinct hashes, one can increase the distance. Increasing
/// distance means that beginning from the blank nodes triples the graph is
/// traversed up and down to add further triples to the hash calculation.
///
/// Blank nodes are not included in calculating the hashes.
///
/// The hashes are distinct if every 'bucket', i.e. the `Vec` in the returning
/// `HashMap` has only one element.
///
/// An exception are redundant blank nodes. If the algorithm detects such nodes
/// they will share the same hash.
fn calc_bn_hashes<D, H>(d: &D) -> StreamResult<HashMap<u64, Vec<DTerm<D>>>, D::Error, AlgorithmFailure>
where
    D: Dataset,
    H: Hasher + Default,
{
    let mut res_map = HashMap::new();
    let mut unresolved_map = HashMap::new();

    for bn in d.bnodes().source_err()?.into_iter() {
        let (hash, upstream, downstream) = calc_bns_init_hash::<D, H>(&bn, d).source_err()?;
        unresolved_map
            .entry(hash)
            .or_insert_with(Vec::new)
            .push((bn, upstream, downstream));
    }

    let mut last_map = unresolved_map;
    unresolved_map = HashMap::new();

    let mut i = 0;

    while !last_map.is_empty() && i < MAX_DISTANCE {
        for (hash, bns) in last_map.into_iter() {
            if bns.len() == 1 {
                // Distinct hash.
                let (bn, _, _) = bns.into_iter().next().expect("len == 1");
                res_map.insert(hash, vec![bn]);
            } else if bns
                .iter()
                .all(|(_, upstream, downstream)| upstream.is_empty() && downstream.is_empty())
            {
                // Can no longer traverse graph to distinguish nodes, i.e. they must be redundant.
                let redundants = bns.into_iter().map(|(bn, _, _)| bn).collect();
                res_map.insert(hash, redundants);
            } else {
                // improve hash by further traversing.
                for (bn, upstream, downstream) in bns {
                    let (better_hash, upstream, downstream) =
                        improve_hash_by_increasing_distance::<H, D>(
                            hash,
                            &upstream,
                            &downstream,
                            d,
                        )
                        .source_err()?;
                    unresolved_map
                        .entry(better_hash)
                        .or_insert_with(Vec::new)
                        .push((bn, upstream, downstream));
                }
            }
        }

        last_map = unresolved_map;
        unresolved_map = HashMap::new();
        i += 1;
    }

    if i >= MAX_DISTANCE {
        return Err(AlgorithmFailure).sink_err();
    }

    Ok(res_map)
}

/// Calculate the blank node's initial hash in the graph, i.e. for distance 0.
///
/// Returns the initial hash, the upstream nodes and the downstream nodes.
#[allow(clippy::type_complexity)]
fn calc_bns_init_hash<D, H>(
    bn: &DTerm<D>,
    d: &D,
) -> Result<(u64, Vec<DTerm<D>>, Vec<DTerm<D>>), D::Error>
where
    D: Dataset,
    H: Hasher + Default,
{
    // for same hashing result we need to order the triples' hashes.
    let mut hashes = BTreeSet::new();

    let mut upstream = vec![];
    let mut downstream = vec![];

    for quad in d.quads() {
        let quad = quad?;
        if quad.s() == bn || quad.p() == bn || quad.o() == bn || quad.g() == Some(bn) {
            hashes.insert(hash_quad_without_bn::<H, DQuad<D>>(&quad));
            if quad.o() != bn {
                upstream.push(quad.o().clone())
            };
            if quad.s() != bn {
                downstream.push(quad.s().clone())
            };
        }
    }

    // hashing
    let mut hasher = H::default();
    hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

/// Improves an existing hash by further traversing the graph.
#[allow(clippy::type_complexity)]
fn improve_hash_by_increasing_distance<H, D>(
    hash: u64,
    upstream: &[DTerm<D>],
    downstream: &[DTerm<D>],
    d: &D,
) -> Result<(u64, Vec<DTerm<D>>, Vec<DTerm<D>>), D::Error>
where
    H: Hasher + Default,
    D: Dataset,
{
    // for same hashing result we need to order the triples' hashes.
    let mut hashes = BTreeSet::new();

    let upstream = traverse_from_s_to_o::<H, D>(upstream, d, &mut hashes)?;
    let downstream = traverse_from_o_to_s::<H, D>(downstream, d, &mut hashes)?;

    // hashing
    let mut hasher = H::default();
    // initialize with existing hash.
    hash.hash(&mut hasher);
    hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

fn hash_quad_without_bn<H, Q>(q: &Q) -> u64
where
    H: Hasher + Default,
    Q: Quad,
{
    let mut h = H::default();
    hash_if_not_bn(q.s(), &mut h);
    hash_if_not_bn(q.p(), &mut h);
    hash_if_not_bn(q.o(), &mut h);
    if let Some(g) = q.g() {
        hash_if_not_bn(g, &mut h)
    }
    h.finish()
}

/// Looks for triples where the given terms are objects.
/// Those triples' hashes are inserted into the list and a list of their
/// subjects is returned.
fn traverse_from_o_to_s<H, D>(
    objects: &[DTerm<D>],
    d: &D,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<DTerm<D>>, D::Error>
where
    H: Hasher + Default,
    D: Dataset,
{
    let mut subjects = vec![];
    for o in objects {
        for quad in d.quads_with_o(o) {
            let quad = quad?;
            hashes.insert(hash_quad_without_bn::<H, DQuad<D>>(&quad));
            subjects.push(quad.s().clone());
        }
    }
    Ok(subjects)
}

/// Looks for triples where the given terms are subjects.
/// Those triples' hashes are inserted into the list and a list of their
/// objects is returned.
fn traverse_from_s_to_o<H, D>(
    subjects: &[DTerm<D>],
    d: &D,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<DTerm<D>>, D::Error>
where
    H: Hasher + Default,
    D: Dataset,
{
    let mut objects = vec![];
    for s in subjects {
        for quad in d.quads_with_s(s) {
            let quad = quad?;
            hashes.insert(hash_quad_without_bn::<H, DQuad<D>>(&quad));
            objects.push(quad.o().clone());
        }
    }
    Ok(objects)
}

#[cfg(test)]
mod test {
    // use super::*;
    // use crate::graph::inmem::FastGraph;
    // use crate::parser::{nt, turtle};
    // use crate::triple::stream::TripleSource;

    // #[test]
    // fn simple() -> Result<(), Box<dyn Error>> {
    //     let g1 = r#"
    //         @prefix foaf: <http://xmlns.com/foaf/0.1/>.

    //         _:alice foaf:name "Alice";
    //                foaf:mbox <mailto:alice@work.example> ;
    //                foaf:knows _:bob .
    //         _:bob foaf:name "Bob".
    //     "#;
    //     let g2 = r#"
    //         @prefix foaf: <http://xmlns.com/foaf/0.1/>.

    //         _:a foaf:name "Alice";
    //                foaf:mbox <mailto:alice@work.example> ;
    //                foaf:knows _:b .
    //         _:b foaf:name "Bob".
    //     "#;
    //     let g3 = r#"
    //         @prefix foaf: <http://xmlns.com/foaf/0.1/>.

    //         _:a foaf:name "Alice";
    //                foaf:mbox <mailto:alice@work.example> ;
    //                foaf:knows _:b .
    //         _:c foaf:name "Bob".
    //     "#;
    //     let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
    //     let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;
    //     let g3: FastGraph = turtle::parse_str(g3).collect_triples()?;

    //     assert!(isomorphic_graphs(&g1, &g2)?);
    //     assert!(isomorphic_graphs(&g2, &g1)?);
    //     assert!(!isomorphic_graphs(&g1, &g3)?);
    //     assert!(!isomorphic_graphs(&g2, &g3)?);

    //     Ok(())
    // }

    // #[test]
    // fn different_parsers() -> Result<(), Box<dyn Error>> {
    //     let ttl = r#"
    //         @prefix foaf: <http://xmlns.com/foaf/0.1/>.

    //         [] foaf:name "Alice";
    //                foaf:mbox <mailto:alice@work.example> ;
    //                foaf:knows [foaf:name "Bob"] .
    //     "#;
    //     let nt = r#"
    //         _:alice <http://xmlns.com/foaf/0.1/name> "Alice".
    //         _:alice <http://xmlns.com/foaf/0.1/mbox> <mailto:alice@work.example>.
    //         _:alice <http://xmlns.com/foaf/0.1/knows> _:bob.
    //         _:bob <http://xmlns.com/foaf/0.1/name> "Bob".
    //     "#;
    //     let ttl: FastGraph = turtle::parse_str(ttl).collect_triples()?;
    //     let nt: FastGraph = nt::parse_str(nt).collect_triples()?;

    //     assert!(isomorphic_graphs(&nt, &ttl)?);
    //     assert!(isomorphic_graphs(&ttl, &nt)?);

    //     Ok(())
    // }

    // /// Every subject and object is a blank node with the a different predicate.
    // #[test]
    // fn heterogeneous_grid() -> Result<(), Box<dyn Error>> {
    //     let g1 = r#"
    //         @prefix : <http://example.org/>.

    //         _:a :p1 _:b, _:d .
    //         _:c :p2 _:b, _:f .
    //         _:e :p3 _:b, _:d, _:f, _:h .
    //         _:g :p4 _:d, _:h .
    //         _:i :p5 _:f, _:h .
    //     "#;
    //     let g2 = r#"
    //         @prefix : <http://example.org/>.

    //         _:a2 :p1 _:b2, _:d2 .
    //         _:c2 :p2 _:b2, _:f2 .
    //         _:e2 :p3 _:b2, _:d2, _:f2, _:h2 .
    //         _:g2 :p4 _:d2, _:h2 .
    //         _:i2 :p5 _:f2, _:h2 .
    //     "#;
    //     let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
    //     let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

    //     assert!(isomorphic_graphs(&g1, &g2)?);
    //     assert!(isomorphic_graphs(&g2, &g1)?);

    //     Ok(())
    // }

    // /// Every subject and object is a blank node with the same predicate.
    // /// Source of test: http://aidanhogan.com/docs/rdf-canonicalisation.pdf
    // #[test]
    // fn homogeneous_grid() -> Result<(), Box<dyn Error>> {
    //     let g1 = r#"
    //         @prefix : <http://example.org/>.

    //         _:a :p _:b, _:d .
    //         _:c :p _:b, _:f .
    //         _:e :p _:b, _:d, _:f, _:h .
    //         _:g :p _:d, _:h .
    //         _:i :p _:f, _:h .
    //     "#;
    //     let g2 = r#"
    //         @prefix : <http://example.org/>.

    //         _:a2 :p _:b2, _:d2 .
    //         _:c2 :p _:b2, _:f2 .
    //         _:e2 :p _:b2, _:d2, _:f2, _:h2 .
    //         _:g2 :p _:d2, _:h2 .
    //         _:i2 :p _:f2, _:h2 .
    //     "#;
    //     let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
    //     let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

    //     assert!(isomorphic_graphs(&g1, &g2)?);
    //     assert!(isomorphic_graphs(&g2, &g1)?);

    //     Ok(())
    // }

    // /// Like homogeneous grid but with redundant nodes removed in the second graph.
    // #[test]
    // fn truncated_grid() -> Result<(), Box<dyn Error>> {
    //     let g1 = r#"
    //         @prefix : <http://example.org/>.

    //         _:a :p _:b, _:d .
    //         _:c :p _:b, _:f .
    //         _:e :p _:b, _:d, _:f, _:h .
    //         _:g :p _:d, _:h .
    //         _:i :p _:f, _:h .
    //     "#;
    //     let g2 = r#"
    //         @prefix : <http://example.org/>.

    //         _:a2 :p _:b2 .
    //     "#;
    //     let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
    //     let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

    //     assert!(!isomorphic_graphs(&g1, &g2)?);
    //     assert!(!isomorphic_graphs(&g2, &g1)?);

    //     Ok(())
    // }
    // /// Source of test: http://aidanhogan.com/docs/rdf-canonicalisation.pdf
    // #[test]
    // fn spider_like() -> Result<(), Box<dyn Error>> {
    //     let g1 = r#"
    //         @prefix : <http://example.org/>.

    //         :Chile :cabinet _:b1, [
    //             :members 23
    //           ], [
    //             :members 23
    //           ], _:b4 ;
    //           :presidency _:a1, _:a2, _:a3, _:a4 .
            
    //         _:a1 :next _:a2 .
    //         _:a2 :next _:a3 ;
    //           :president :MBachelet .
    //         _:a3 :next _:a4.
    //         _:a4 :president :MBachelet .

    //         :MBachelet :spouse _:c .
    //     "#;
    //     let g2 = r#"
    //         @prefix : <http://example.org/>.

    //         :Chile :cabinet _:b12, [
    //             :members 23
    //         ], [
    //             :members 23
    //         ], _:b42 ;
    //         :presidency _:a12, _:a22, _:a32, _:a42 .
            
    //         _:a12 :next _:a22 .
    //         _:a22 :next _:a32 ;
    //         :president :MBachelet .
    //         _:a32 :next _:a42.
    //         _:a42 :president :MBachelet .

    //         :MBachelet :spouse _:c2 .
    //     "#;
    //     let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
    //     let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

    //     assert!(isomorphic_graphs(&g1, &g2)?);
    //     assert!(isomorphic_graphs(&g2, &g1)?);

    //     Ok(())
    // }
}
