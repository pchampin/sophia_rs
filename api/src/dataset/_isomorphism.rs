//! This module implements check for isomorphic blank node equivalence of RDF
//! datasets.
//!
//! Its public member are transparently re-exported by its [parent module](../index.html).

use crate::dataset::{DQuad, DTerm, Dataset};
use crate::graph::{hash_if_not_bn, match_ignore_bns};
use crate::quad::Quad;
use crate::term::matcher::AnyOrExactlyRef;
use crate::term::{TTerm, TermKind};
use crate::triple::stream::{
    SinkError, SinkResult as _, SourceError, SourceResult as _, StreamError, StreamResult,
};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// Maximal steps a dataset is traversed for proofing isomorphism.
/// If this bound is exceeded the algorithm assumes that the datasets are not
/// isomorphic.
pub const MAX_DISTANCE: usize = 8;

/// The hasher used internally for checking isomorphism.
pub type IsoHasher = std::collections::hash_map::DefaultHasher;

/// Checks if both datasets are isomorphic blank node equal.
///
/// According to the [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism)
/// this means that a mapping for blank nodes in `d1` exists so that `d1 == d2`.
///
/// The algorithm is inspired from a similar one in [`Oxigraph`](https://github.com/Tpt/oxigraph)
/// and is extended for the generalized RDF model of `sophia`.
///
/// # Performance
///
/// As this algorithm has to enumerates the quads of each dataset several times,
/// the algorithm gets more expensive with bigger numbers of quads.
/// In the same way the number of blank nodes contributes to the cost.
///
/// Note however that the algorithm uses some heuristics,
/// to avoid the combinatorial explosion of trying every possible bnode-pairing.
/// As a result, it is not 100% accurate (see below).
///
/// # Accuracy
///
/// If `d1` and `d2` are isomorphic, the function will always return `true`.
///
/// If they are not isomorphic, the function will generally return `false`,
/// but a few pathological cases may be falses positives
/// (*i.e.* recognized as isomorphic while they are not).
///
/// See [`isomorphic_graphs`](../graph/fn.isomorphic_graphs.html) for pathological examples.
pub fn isomorphic_datasets<D1, D2>(d1: &D1, d2: &D2) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
    DTerm<D1>: Clone + Eq + Hash,
    DTerm<D2>: Clone + Eq + Hash,
{
    // quick return conditions
    // -----------------------
    let (min1, max1) = d1.quads().size_hint();
    let (min2, max2) = d2.quads().size_hint();
    if let Some(max1) = max1 {
        if max1 < min2 {
            return Ok(false);
        }
    }
    if let Some(max2) = max2 {
        if max2 < min1 {
            return Ok(false);
        }
    }

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
    if !check_for_equal_quads_regardless_bns(d1, d2)? {
        return Ok(false);
    }
    if !check_for_equal_quads_regardless_bns(d2, d1).map_err(StreamError::reverse)? {
        return Ok(false);
    }

    // Create hashes
    let bn_hashes1 = match calc_bn_hashes::<D1, IsoHasher>(d1, bns1) {
        Ok(map) => map,
        Err(e) => return Err(SourceError(e)),
    };
    let bn_hashes2 = match calc_bn_hashes::<D2, IsoHasher>(d2, bns2) {
        Ok(map) => map,
        Err(e) => return Err(SinkError(e)),
    };

    // Check that, for each hash, there are the same number of bnodes in each graph.
    for (hash, bns1) in bn_hashes1 {
        let bns1_len = bns1.len();
        let bns2_len = bn_hashes2.get(&hash).map(|x| x.len()).unwrap_or(0);
        if bns1_len != bns2_len {
            return Ok(false); // Not the same number of "equivalent" bnodes
        }
    }
    Ok(true) // heuristically
}

fn match_gname_ignore_bns<T>(g: Option<&T>) -> AnyOrExactlyRef<Option<&T>>
where
    T: TTerm + ?Sized,
{
    if g.map(TTerm::kind) == Some(TermKind::BlankNode) {
        AnyOrExactlyRef::Any
    } else {
        AnyOrExactlyRef::Exactly(g)
    }
}

/// Checks if each quad in `d1` is also in `d2` regardless of blank node labels.
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
/// We first compute a hash based on all adjacent triples, ignoring bnodes.
///
/// If several blank nodes have the same hash,
/// we modify their hash with the hash of their adjacent blank nodes.
/// We repeat this step until either
/// - we reached a point where each blank node has a unique hash, or
/// - the last step didn't change the number of distinct hash.
/// At this point, if several blank nodes share the same hash,
/// they must be absolutely redundant.
fn calc_bn_hashes<D, H>(
    d: &D,
    bnodes: HashSet<DTerm<D>>,
) -> Result<HashMap<u64, Vec<DTerm<D>>>, D::Error>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut n2h = HashMap::new();
    let mut map = HashMap::new();

    let n_bnodes = bnodes.len();
    for bn in bnodes {
        let (hash, related) = calc_bns_init_hash::<D, H>(&bn, d)?;
        n2h.insert(bn.clone(), hash);
        map.entry(hash).or_insert_with(Vec::new).push((bn, related));
    }

    let mut len_old_map = 0;

    while map.len() < n_bnodes && map.len() != len_old_map {
        len_old_map = map.len();
        let last_map = map;
        map = HashMap::new();
        let last_n2h = n2h.clone();

        for (hash, bns) in last_map {
            if bns.len() == 1 {
                map.insert(hash, bns);
            } else {
                for (bn, related) in bns {
                    let mut hasher = H::default();
                    hash.hash(&mut hasher);

                    let mut modifiers = Vec::new();
                    for (role, other) in related.iter() {
                        modifiers.push((role, last_n2h[other]));
                    }
                    modifiers.sort_unstable(); // to ensure reproducibility
                    for (role, hash) in modifiers {
                        role.hash(&mut hasher);
                        hash.hash(&mut hasher);
                    }
                    let new_hash = hasher.finish();
                    *n2h.get_mut(&bn).unwrap() = new_hash;
                    map.entry(new_hash)
                        .or_insert_with(Vec::new)
                        .push((bn, related));
                }
            }
        }
        //dbg_map::<G, _>(&map);
    }
    let mut ret = HashMap::with_capacity(map.len());
    for (hash, bns) in map {
        let v = bns.into_iter().map(|(bn, _)| bn).collect();
        ret.insert(hash, v);
    }
    Ok(ret)
}

/// Calculate the blank node's initial hash in the graph, i.e. for distance 0.
///
/// Returns the initial hash, and a vec of related blank node
/// (associated with an opaque role identifier )
#[allow(clippy::type_complexity)]
fn calc_bns_init_hash<D, H>(bn: &DTerm<D>, d: &D) -> Result<(u64, Vec<(u8, DTerm<D>)>), D::Error>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut quad_hashes = Vec::new();
    let mut related = vec![];

    for quad in d.quads_with_s(bn) {
        let quad = quad?;
        quad_hashes.push(hash_quad_without_bn::<H, DQuad<D>>(&quad));
        let p = quad.p();
        if p.kind() == TermKind::BlankNode && p != bn {
            related.push((0, p.clone()));
        }
        let o = quad.o();
        if o.kind() == TermKind::BlankNode && o != bn {
            related.push((1, o.clone()));
        }
        if let Some(g) = quad.g() {
            if g.kind() == TermKind::BlankNode && g != bn {
                related.push((2, g.clone()));
            }
        }
    }
    for quad in d.quads_with_p(bn) {
        let quad = quad?;
        quad_hashes.push(hash_quad_without_bn::<H, DQuad<D>>(&quad));
        let s = quad.s();
        if s.kind() == TermKind::BlankNode && s != bn {
            related.push((3, s.clone()));
        }
        let o = quad.o();
        if o.kind() == TermKind::BlankNode && o != bn {
            related.push((4, o.clone()));
        }
        if let Some(g) = quad.g() {
            if g.kind() == TermKind::BlankNode && g != bn {
                related.push((5, g.clone()));
            }
        }
    }
    for quad in d.quads_with_o(bn) {
        let quad = quad?;
        quad_hashes.push(hash_quad_without_bn::<H, DQuad<D>>(&quad));
        let s = quad.s();
        if s.kind() == TermKind::BlankNode && s != bn {
            related.push((6, s.clone()));
        }
        let p = quad.p();
        if p.kind() == TermKind::BlankNode && p != bn {
            related.push((7, p.clone()));
        }
        if let Some(g) = quad.g() {
            if g.kind() == TermKind::BlankNode && g != bn {
                related.push((8, g.clone()));
            }
        }
    }
    for quad in d.quads_with_g(Some(bn)) {
        let quad = quad?;
        quad_hashes.push(hash_quad_without_bn::<H, DQuad<D>>(&quad));
        let s = quad.s();
        if s.kind() == TermKind::BlankNode && s != bn {
            related.push((9, s.clone()));
        }
        let p = quad.p();
        if p.kind() == TermKind::BlankNode && p != bn {
            related.push((10, p.clone()));
        }
        let o = quad.o();
        if o.kind() == TermKind::BlankNode && o != bn {
            related.push((11, o.clone()));
        }
    }

    quad_hashes.sort_unstable(); // to ensure reproducibility

    // hashing
    let mut hasher = H::default();
    quad_hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), related))
}

fn hash_quad_without_bn<H, Q>(q: &Q) -> u64
where
    H: Hasher + Default,
    Q: Quad,
{
    let mut h = H::default();
    hash_if_not_bn(q.s(), 0, &mut h);
    hash_if_not_bn(q.p(), 1, &mut h);
    hash_if_not_bn(q.o(), 2, &mut h);
    if let Some(g) = q.g() {
        hash_if_not_bn(g, 3, &mut h)
    }
    h.finish()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ns::xsd;
    use crate::term::test::TestTerm;
    use std::error::Error;

    type StaticTerm = TestTerm<&'static str>;

    #[test]
    fn simple() -> Result<(), Box<dyn Error>> {
        let foaf = "http://xmlns.com/foaf/0.1/";
        let foaf_knows = StaticTerm::iri2(foaf, "knows");
        let foaf_mbox = StaticTerm::iri2(foaf, "mbox");
        let foaf_name = StaticTerm::iri2(foaf, "name");
        let mbox_alice = StaticTerm::iri("mailto:alice@work.example");
        let lit_alice = StaticTerm::lit_dt("alice", xsd::string);
        let lit_bob = StaticTerm::lit_dt("bob", xsd::string);

        let make_dataset =
            |b1: &'static str, b2: &'static str| -> Vec<([StaticTerm; 3], Option<StaticTerm>)> {
                let b1 = StaticTerm::bnode(b1);
                let b2 = StaticTerm::bnode(b2);
                vec![
                    ([b1, foaf_name, lit_alice], None),
                    ([b1, foaf_mbox, mbox_alice], None),
                    ([b1, foaf_knows, b2], None),
                    ([b2, foaf_name, lit_bob], Some(b1)),
                ]
            };
        let d1 = make_dataset("alice", "bob");
        assert!(isomorphic_datasets(&d1, &d1)?);

        let d2 = make_dataset("a", "b");
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        let d3 = make_dataset("b", "a");
        assert!(isomorphic_datasets(&d2, &d3)?);
        assert!(isomorphic_datasets(&d1, &d3)?);

        let b1 = StaticTerm::bnode("alice");
        let d4 = vec![
            ([b1, foaf_name, lit_alice], None),
            ([b1, foaf_mbox, mbox_alice], None),
            ([b1, foaf_knows, StaticTerm::bnode("bob")], None),
            ([StaticTerm::bnode("bobby"), foaf_name, lit_bob], Some(b1)),
        ];
        assert!(!isomorphic_datasets(&d1, &d4)?);
        assert!(!isomorphic_datasets(&d4, &d1)?);

        Ok(())
    }

    fn make_chain(ids: &'static str) -> Vec<[StaticTerm; 4]> {
        let rel = StaticTerm::iri("tag:rel");
        let nodes: Vec<_> = (0..ids.len())
            .map(|i| StaticTerm::bnode(&ids[i..i + 1]))
            .collect();
        let mut dataset = Vec::with_capacity(ids.len() - 1);
        for i in 1..nodes.len() {
            dataset.push([nodes[i - 1], rel, nodes[i], nodes[i - 1]]);
        }
        dataset
    }

    #[test]
    fn chain() -> Result<(), Box<dyn Error>> {
        let d1 = make_chain("abcdefghij");
        assert!(isomorphic_datasets(&d1, &d1)?);
        let d2 = make_chain("jihgfedcba");
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        let d3 = make_chain("abcdefghijk");
        assert!(!isomorphic_datasets(&d1, &d3)?);
        Ok(())
    }

    #[test]
    fn cycle2() -> Result<(), Box<dyn Error>> {
        let d1 = make_chain("aba");
        assert!(isomorphic_datasets(&d1, &d1)?);
        let d2 = make_chain("ABA");
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);
        Ok(())
    }

    #[test]
    fn cycle_long() -> Result<(), Box<dyn Error>> {
        let d1 = make_chain("abcdefghia");
        assert!(isomorphic_datasets(&d1, &d1)?);
        let d2 = make_chain("jihgfedcbj");
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        let d3 = make_chain("abcdefghija");
        assert!(!isomorphic_datasets(&d1, &d3)?);
        Ok(())
    }

    #[test]
    #[ignore]
    fn cycle_pathological() -> Result<(), Box<dyn Error>> {
        // This case is tricky (and does not work with the current implementation).
        // Both graphs contain the same number of (blank nodes) and the same number of arcs.
        // All blank nodes are locally undistinguishable from each other:
        // - they have exactly 1 incoming arc and 1 outgoing arc,
        // - both linking them to a blank node that are themselves undistinguisgable.
        let mut d1 = make_chain("abca");
        let mut d1b = make_chain("defgd");
        d1.append(&mut d1b);

        let d2 = make_chain("abcdefga");
        assert!(!isomorphic_datasets(&d1, &d2)?);
        Ok(())
    }

    #[test]
    fn cycle_almost_pathological() -> Result<(), Box<dyn Error>> {
        // This is uses the same graphs as above (cycle_pathological),
        // but *one* of the blank nodes is distinguished by an additional property,
        // which breaks symmetry and allow the algorithm to give the correct answer.
        //
        // This illustrate why the pathological case is not too bad:
        // in real data, *most* be nodes will be distinguisgable like that.
        let typ = StaticTerm::iri("tag:type");
        let dist = StaticTerm::iri("tag:Distinguished");

        let mut d1 = make_chain("abca");
        let mut d1b = make_chain("defgd");
        d1.append(&mut d1b);
        d1.push([d1[0][0], typ, dist, d1[0][0]]);

        let mut d2 = make_chain("abcdefga");
        d2.push([d2[0][0], typ, dist, d2[0][0]]);
        assert!(!isomorphic_datasets(&d1, &d2)?);
        Ok(())
    }

    fn make_clique(ids: &'static str) -> Vec<[StaticTerm; 4]> {
        let rel = StaticTerm::iri("tag:rel");
        let nodes: Vec<_> = (0..ids.len())
            .map(|i| StaticTerm::bnode(&ids[i..i + 1]))
            .collect();
        let mut dataset = Vec::with_capacity(ids.len() * ids.len());
        for n1 in nodes.iter() {
            for n2 in nodes.iter() {
                dataset.push([*n1, rel, *n2, *n1]);
            }
        }
        dataset
    }

    #[test]
    fn clique() -> Result<(), Box<dyn Error>> {
        let d1 = make_clique("abcde");
        assert!(isomorphic_datasets(&d1, &d1)?);

        let d2 = make_clique("ABCDE");
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        let d3 = make_clique("abcd");
        assert!(!isomorphic_datasets(&d1, &d3)?);
        Ok(())
    }

    fn make_tree(ids: &'static str) -> Vec<[StaticTerm; 4]> {
        let rel = StaticTerm::iri("tag:rel");
        let nodes: Vec<_> = (0..ids.len())
            .map(|i| StaticTerm::bnode(&ids[i..i + 1]))
            .collect();
        let mut dataset = Vec::with_capacity(ids.len() * ids.len());
        let mut i = 0;
        while 2 * i < nodes.len() {
            dataset.push([nodes[i], rel, nodes[2 * i], nodes[i]]);
            if 2 * i + 1 < nodes.len() {
                dataset.push([nodes[i], rel, nodes[2 * i + 1], nodes[i]]);
            }
            i += 1;
        }
        dataset
    }

    #[test]
    fn tree() -> Result<(), Box<dyn Error>> {
        let d1 = make_tree("abcdefghij");
        assert!(isomorphic_datasets(&d1, &d1)?);

        let d2 = make_tree("ABCDEFGHIJ");
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        let d3 = make_tree("abcdefghijk");
        assert!(!isomorphic_datasets(&d1, &d3)?);
        Ok(())
    }

    #[test]
    fn predicate_and_gname() -> Result<(), Box<dyn Error>> {
        let rel = StaticTerm::iri("tag:rel");
        let b1 = StaticTerm::bnode("b1");
        let b2 = StaticTerm::bnode("b2");
        let b3 = StaticTerm::bnode("b3");
        let b4 = StaticTerm::bnode("b4");

        let d1 = vec![[b1, rel, b2, b3], [b2, rel, b3, b4], [rel, b1, b4, b3]];
        assert!(isomorphic_datasets(&d1, &d1)?);

        let d2 = vec![[b2, rel, b3, b4], [b3, rel, b4, b1], [rel, b2, b1, b4]];
        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        let d3 = vec![[b1, rel, b2, b3], [b2, rel, b3, b4], [rel, b2, b4, b3]];
        //                                                        ^^
        assert!(!isomorphic_datasets(&d2, &d3)?);
        assert!(!isomorphic_datasets(&d1, &d3)?);

        let d4 = vec![[b1, rel, b2, b3], [b2, rel, b3, b4], [rel, b1, b4, b2]];
        //                                                                ^^
        assert!(!isomorphic_datasets(&d2, &d4)?);
        assert!(!isomorphic_datasets(&d1, &d4)?);

        Ok(())
    }
}
