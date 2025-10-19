//! I provide Sophia-C14N, a canonicalization algorithm for RDF Datasets,
//! which unlike [`RDFC-1.0`](crate::rdfc10) supports RDF 1.2 and Sophia's generalized RDF.
//!
//! Note that this canonicalization algorithm
//! + is not based on any official standard, and as such, may be modified in future versions of Sophia;
//! + will however always return the same result as RDFC-1.0 for any dataset supported by RDFC-1.0;
//! + is complete for RDF 1.2 (any compliant dataset can be canonicalized),
//!   but *not* for *generalized* RDF 1.2:
//!   more precisely, it will reject datasets having triple terms as named graphs.

use std::{collections::BTreeMap, io, rc::Rc};

use base64::{Engine, prelude::BASE64_URL_SAFE_NO_PAD};
use sophia_api::{
    dataset::{DTerm, SetDataset},
    quad::{Quad, Spog},
    term::{BnodeId, IriRef, Term},
};

pub use crate::rdfc10::C14nIdMap;
use crate::{
    _c14n_term::C14nTerm,
    _cnq::nq,
    C14nError,
    hash::{HashFunction, Sha256, Sha384},
    rdfc10::{
        DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT, normalize_with_inner, relabel_with_inner,
    },
};

/// Write into `w` a canonical N-quads representation of `d`, where
/// + blank nodes are canonically [relabelled](relabel) with
///   - the [SHA-256](Sha256) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`];
/// + quads are sorted in codepoint order.
///
/// See also [`normalize_with`].
pub fn normalize<D: SetDataset, W: io::Write>(d: &D, w: W) -> Result<(), C14nError<D::Error>> {
    normalize_with::<Sha256, D, W>(d, w, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Write into `w` a canonical N-quads representation of `d`, where
/// + blank nodes are canonically [relabelled](relabel_sha384) with
///   - the [SHA-384](Sha384) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`];
/// + quads are sorted in codepoint order.
///
/// See also [`normalize_with`].
pub fn normalize_sha384<D: SetDataset, W: io::Write>(
    d: &D,
    w: W,
) -> Result<(), C14nError<D::Error>> {
    normalize_with::<Sha384, D, W>(d, w, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Write into `w` a canonical N-quads representation of `d`, where
/// + blank nodes are canonically [relabelled](relabel_with) with
///   - the [hash function](HashFunction) `H`,
///   - the given `depth_factor`,
///   - the given `permutation_limit`;
/// + quads are sorted in codepoint order;
/// + if the generic parameter `S` (strict) is false,
///   a (non-standard) generalized version of RDFC1.0 is used,
///   that supports RDF 1.2 and Sophia's generalized RDF.
///
/// See also [`normalize`].
pub fn normalize_with<H: HashFunction, D: SetDataset, W: io::Write>(
    d: &D,
    w: W,
    depth_factor: f32,
    permutation_limit: usize,
) -> Result<(), C14nError<D::Error>> {
    let (quads, _) = relabel_with::<H, D>(d, depth_factor, permutation_limit)?;
    normalize_with_inner::<C14nTerm<DTerm<'_, D>>, D::Error, W, false>(quads, w)
}

/// Return a [`Dataset`](sophia_api::dataset::Dataset) isomorphic to `d`,
/// paired with a mapping from original blank node labels to canonical ones.
///
/// This calls [`relabel_with`] with
///   - the [SHA-256](Sha256) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`].
///
/// Implements <https://www.w3.org/TR/rdf-canon/#canon-algorithm>
///
/// See also [`normalize`].
pub fn relabel<D: SetDataset>(d: &D) -> Result<(C14nQuads<'_, D>, C14nIdMap), C14nError<D::Error>> {
    relabel_with::<Sha256, D>(d, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Return a [`Dataset`](sophia_api::dataset::Dataset) isomorphic to `d`,
/// paired with a mapping from original blank node labels to canonical ones.
///
/// This calls [`relabel_with`] with
///   - the [SHA-384](Sha384) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`].
///
/// Implements <https://www.w3.org/TR/rdf-canon/#canon-algorithm>
///
/// See also [`normalize`].
pub fn relabel_sha384<D: SetDataset>(
    d: &D,
) -> Result<(C14nQuads<'_, D>, C14nIdMap), C14nError<D::Error>> {
    relabel_with::<Sha384, D>(d, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Return a [`Dataset`](sophia_api::dataset::Dataset) isomorphic to `d`,
/// paired with a mapping from original blank node labels to canonical ones.
///
/// The generic parameter `H` determines which [hash function](HashFunction)
/// the algorithm should use internally
/// (RDFC-1.0 uses [SHA-256](Sha256) by default).
///
/// The parameters `depth_factor` and `permutation_limit`
/// are used to stop the algorithm if the computation becomes too complex,
/// in order to secure it against [dataset poisoning](https://www.w3.org/TR/rdf-canon/#dataset-poisoning).
/// The default values ([`DEFAULT_DEPTH_FACTOR`]) and [`DEFAULT_PERMUTATION_LIMIT`])
/// are expected to work with any "realistic" dataset.
///
/// More preciselity:
/// * the algorithm will not recurse more deeply than`depth_factor`*N,
///   where N is the total number of blank nodes in the dataset;
/// * the algorithm will not try to disambiguate more than
///   `permutation_limit` undistinguishable blank nodes
///   (blank nodes with the same immediate neighbourhood).
///
/// Implements <https://www.w3.org/TR/rdf-canon/#canon-algorithm>;
/// if the generic parameter `S` (strict) is false,
/// that algorithm is extended to supports RDF 1.2 and Sophia's generalized RDF.
///
/// See also [`relabel`], [`normalize_with`].
pub fn relabel_with<'a, H: HashFunction, D: SetDataset>(
    d: &'a D,
    depth_factor: f32,
    permutation_limit: usize,
) -> Result<(C14nQuads<'a, D>, C14nIdMap), C14nError<D::Error>> {
    let d_quads = d.quads();
    let mut quads: Vec<Spog<C14nTerm<DTerm<'a, D>>>> = Vec::with_capacity(d_quads.size_hint().0);
    let mut aliases: Aliases<'a, H, D> = BTreeMap::new();

    for q in d_quads {
        let (spo, g) = q?.to_spog();
        let spo = spo.map(|t| escape_triple_terms::<H, D>(t, &mut aliases));
        if g.as_ref().map(Term::is_triple).unwrap_or(false) {
            return Err(C14nError::Unsupported(
                "Generalized RDFC-1.0 does not support triple terms as graph names.".into(),
            ));
        }
        quads.push((spo, g.map(C14nTerm::Other)));
    }
    let aaa: IriRef<Rc<str>> = IriRef::new_unchecked("aaa:basic-encoded".into());
    if !aliases.is_empty() {
        quads.push((
            [
                C14nTerm::Aaa(aaa.clone()),
                C14nTerm::Aaa(aaa.clone()),
                C14nTerm::Aaa(aaa.clone()),
            ],
            None,
        ))
    }
    for (id, is_bnode, tt) in aliases.into_values() {
        let gn = if is_bnode {
            let bnid = BnodeId::new_unchecked(id);
            quads.push((
                [
                    C14nTerm::Aaa(aaa.clone()),
                    C14nTerm::Aaa(aaa.clone()),
                    C14nTerm::Blank(bnid.clone()),
                ],
                None,
            ));
            C14nTerm::Blank(bnid)
        } else {
            C14nTerm::Aaa(IriRef::new_unchecked(id))
        };
        quads.push((tt, Some(gn)));
    }
    relabel_with_inner::<'a, H, C14nTerm<DTerm<'a, D>>, D::Error, false>(
        quads,
        depth_factor,
        permutation_limit,
    )
}

fn escape_triple_terms<'a, H: HashFunction, D: SetDataset>(
    t: DTerm<'a, D>,
    aliases: &mut Aliases<'a, H, D>,
) -> C14nTerm<DTerm<'a, D>> {
    if t.is_triple() {
        let triple = t.to_triple().unwrap();
        let triple = triple.map(|t| escape_triple_terms::<H, D>(t, aliases));
        let hash = {
            let mut input = H::initialize();
            for t in &triple {
                nq(t, &mut input.as_write()).unwrap();
                input.update(b" ");
            }
            input.finalize()
        };
        let (id, is_bnode, _) = aliases.entry(hash).or_insert_with_key(|hash| {
            let is_bnode = triple.iter().any(Term::is_blank_node);
            let mut buffer = String::new();
            buffer.push_str(if is_bnode { "b" } else { "aaa:" });
            BASE64_URL_SAFE_NO_PAD.encode_string(hash, &mut buffer);
            (buffer.into(), is_bnode, triple)
        });
        match is_bnode {
            true => C14nTerm::Blank(BnodeId::new_unchecked(id.clone())),
            false => C14nTerm::Aaa(IriRef::new_unchecked(id.clone())),
        }
    } else {
        C14nTerm::Other(t)
    }
}

type Aliases<'a, H, D> = BTreeMap<<H as HashFunction>::Output, (Rc<str>, bool, C14nTriple<'a, D>)>;

type C14nTriple<'a, D> = [C14nTerm<DTerm<'a, D>>; 3];

/// An impl of [`Dataset`](sophia_api::dataset::Dataset)
/// that contains canonical labels for blank nodes.
pub type C14nQuads<'a, D> = Vec<Spog<C14nTerm<C14nTerm<DTerm<'a, D>>>>>;

#[cfg(test)]
mod test {
    use sophia_api::{prelude::MutableDataset, term::SimpleTerm};

    use super::*;
    use crate::rdfc10::test::{ez_quad, ez_quads, ez_term};

    #[test]
    fn generalized_atoms() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "<x:s> <x:p> <x:o> <x:g> .",
            "'s' 'p' 'o' 'g' .",
            "_:s _:p _:o _:g .",
            "?s ?p ?o ?g .",
        ]);
        let exp = r#""s" "p" "o" "g" .
<x:s> <x:p> <x:o> <x:g> .
?s ?p ?o ?g .
_:c14n0 _:c14n1 _:c14n3 _:c14n2 .
"#;
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    #[test]
    fn rdf12() {
        crate::test_setup();

        let dataset = ez_quads(&["_:r <x:reifies> <<(_:s\t<x:p>\t'o'@en--ltr)>> ."]);
        let exp = r#"<aaa:basic-encoded> <aaa:basic-encoded> <aaa:basic-encoded> .
<aaa:basic-encoded> <aaa:basic-encoded> _:c14n2 .
_:c14n0 <x:reifies> _:c14n2 .
_:c14n1 <x:p> "o'@en"@en--ltr _:c14n2 .
"#;
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    pub fn c14n_nquads<D: SetDataset>(d: &D) -> Result<String, C14nError<D::Error>> {
        let mut output = Vec::<u8>::new();
        normalize(d, &mut output)?;
        Ok(unsafe { String::from_utf8_unchecked(output) })
    }

    #[test]
    fn generalized_rdf12() {
        crate::test_setup();

        let mut dataset = ez_quads(&[
            "_:b1 <x:p1> <<(<x:s>\t<x:p>\t<x:o>)>> .",
            "_:b2 <x:p2> <<(_:s\t_:p\t_:o)>> .",
            "_:b3 <x:p3> <<('s'\t'p'\t'o')>> .",
            "_:b4 <x:p4> <<(?s\t?p\t?o)>> .",
        ]);
        MutableDataset::insert(
            &mut dataset,
            ez_term("_:b5"),
            ez_term("<x:p5>"),
            SimpleTerm::Triple(Box::new(
                ez_quad("<<(<x:s>\t<x:p>\t<x:o>)>> <<(_:s\t_:p\t_:o)>> <<('s'\t'p'\t'o')>> .").0,
            )),
            None as Option<i32>,
        )
        .unwrap();
        let exp = r#""s" "p" "o" <aaa:cREOCpsydLSqBK4M7AM-OVVH-02ZmXlJ-8RucuCvILc> .
<aaa:1GMFI6F3I3vhuhyKDXg7i9TeLkd_iSRdSDvlytoJydg> _:c14n2 <aaa:cREOCpsydLSqBK4M7AM-OVVH-02ZmXlJ-8RucuCvILc> _:c14n6 .
<aaa:basic-encoded> <aaa:basic-encoded> <aaa:basic-encoded> .
<aaa:basic-encoded> <aaa:basic-encoded> _:c14n2 .
<aaa:basic-encoded> <aaa:basic-encoded> _:c14n6 .
<x:s> <x:p> <x:o> <aaa:1GMFI6F3I3vhuhyKDXg7i9TeLkd_iSRdSDvlytoJydg> .
?s ?p ?o <aaa:Bl4nh4zSgXVt4nrMblVksYHnzfFbn8A6hVlorPwgyGw> .
_:c14n0 <x:p4> <aaa:Bl4nh4zSgXVt4nrMblVksYHnzfFbn8A6hVlorPwgyGw> .
_:c14n1 _:c14n4 _:c14n9 _:c14n2 .
_:c14n3 <x:p1> <aaa:1GMFI6F3I3vhuhyKDXg7i9TeLkd_iSRdSDvlytoJydg> .
_:c14n5 <x:p5> _:c14n6 .
_:c14n7 <x:p3> <aaa:cREOCpsydLSqBK4M7AM-OVVH-02ZmXlJ-8RucuCvILc> .
_:c14n8 <x:p2> _:c14n2 .
"#;
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }
}
