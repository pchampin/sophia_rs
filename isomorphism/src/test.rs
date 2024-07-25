use super::*;
use sophia_api::ns::xsd;
use sophia_api::term::{assert_consistent_term_impl, BnodeId, IriRef, Term, TermKind};
use sophia_api::MownStr;
use sophia_api::Error;

const FOAF_KNOWS: MyTerm = MyTerm::Iri("http://xmlns.com/foaf/0.1/knows");
const FOAF_MBOX: MyTerm = MyTerm::Iri("http://xmlns.com/foaf/0.1/mbox");
const FOAF_NAME: MyTerm = MyTerm::Iri("http://xmlns.com/foaf/0.1/name");
const MBOX_ALICE: MyTerm = MyTerm::Iri("mailto:alice@work.example");
const LIT_ALICE: MyTerm = MyTerm::String("alice");
const LIT_BOB: MyTerm = MyTerm::String("bob");

#[test]
fn no_bnode() -> Result<(), Box<dyn Error>> {
    let make_dataset = |i1: &'static str, i2: &'static str| -> Vec<([MyTerm; 3], Option<MyTerm>)> {
        let i1 = MyTerm::Iri(i1);
        let i2 = MyTerm::Iri(i2);
        vec![
            ([i1, FOAF_NAME, LIT_ALICE], None),
            ([i1, FOAF_MBOX, MBOX_ALICE], None),
            ([i1, FOAF_KNOWS, i2], None),
            ([i2, FOAF_NAME, LIT_BOB], Some(i1)),
        ]
    };
    let d1 = make_dataset("#alice", "#bob");
    assert!(isomorphic_datasets(&d1, &d1)?);

    let d2 = make_dataset("#a", "#b");
    assert!(!isomorphic_datasets(&d1, &d2)?);
    assert!(!isomorphic_datasets(&d2, &d1)?);

    let d3 = d1[1..].to_vec();
    assert!(!isomorphic_datasets(&d1, &d3)?);
    assert!(!isomorphic_datasets(&d3, &d1)?);

    let mut d4 = d1.clone();
    d4[3].0[0] = MyTerm::Iri("#bobby");
    assert!(!isomorphic_datasets(&d1, &d4)?);
    assert!(!isomorphic_datasets(&d4, &d1)?);

    let mut d5 = d1.clone();
    d5[3].1 = Some(MyTerm::Iri("#bob"));
    assert!(!isomorphic_datasets(&d1, &d5)?);
    assert!(!isomorphic_datasets(&d5, &d1)?);
    Ok(())
}

#[test]
fn simple() -> Result<(), Box<dyn Error>> {
    let make_dataset = |b1: &'static str, b2: &'static str| -> Vec<([MyTerm; 3], Option<MyTerm>)> {
        let b1 = MyTerm::Bnode(b1);
        let b2 = MyTerm::Bnode(b2);
        vec![
            ([b1, FOAF_NAME, LIT_ALICE], None),
            ([b1, FOAF_MBOX, MBOX_ALICE], None),
            ([b1, FOAF_KNOWS, b2], None),
            ([b2, FOAF_NAME, LIT_BOB], Some(b1)),
        ]
    };
    let d1 = make_dataset("alice", "bob");
    assert!(isomorphic_datasets(&d1, &d1)?);

    let d2 = make_dataset("a", "b");
    assert!(isomorphic_datasets(&d1, &d2)?);
    assert!(isomorphic_datasets(&d2, &d1)?);

    let d3 = d1[1..].to_vec();
    assert!(!isomorphic_datasets(&d1, &d3)?);
    assert!(!isomorphic_datasets(&d3, &d1)?);

    let mut d4 = d1.clone();
    d4[3].0[0] = MyTerm::Bnode("bobby");
    assert!(!isomorphic_datasets(&d1, &d4)?);
    assert!(!isomorphic_datasets(&d4, &d1)?);

    let mut d5 = d1.clone();
    d5[3].1 = Some(MyTerm::Bnode("bob"));
    assert!(!isomorphic_datasets(&d1, &d5)?);
    assert!(!isomorphic_datasets(&d5, &d1)?);

    Ok(())
}

#[test]
fn no_bnode_quoted_triple() -> Result<(), Box<dyn Error>> {
    const A: MyTerm = MyTerm::Iri("#a");
    const B: MyTerm = MyTerm::Iri("#b");
    const C: MyTerm = MyTerm::Iri("#c");
    const D: MyTerm = MyTerm::Iri("#d");
    const E: MyTerm = MyTerm::Iri("#e");

    let d1 = vec![
        ([A, B, C], None),
        ([MyTerm::Triple("#a #b #c"), D, E], Some(A)),
    ];
    assert!(isomorphic_datasets(&d1, &d1)?);

    let d2 = d1[1..].to_vec();
    assert!(!isomorphic_datasets(&d1, &d2)?);
    assert!(!isomorphic_datasets(&d2, &d1)?);

    let mut d3 = d1.clone();
    d3[1].1 = Some(B);
    assert!(!isomorphic_datasets(&d1, &d3)?);
    assert!(!isomorphic_datasets(&d3, &d1)?);

    let d4 = vec![
        ([A, B, C], None),
        ([A, B, MyTerm::Triple("#c #d #e")], Some(A)),
    ];
    assert!(!isomorphic_datasets(&d1, &d4)?);
    assert!(!isomorphic_datasets(&d4, &d1)?);
    Ok(())
}

#[test]
fn quoted_triple() -> Result<(), Box<dyn Error>> {
    const A: MyTerm = MyTerm::Bnode("a");
    const B: MyTerm = MyTerm::Bnode("b");
    const C: MyTerm = MyTerm::Bnode("c");
    const D: MyTerm = MyTerm::Bnode("d");
    const E: MyTerm = MyTerm::Bnode("e");

    let d1 = vec![
        ([A, B, C], None),
        ([MyTerm::Triple("a b c"), D, E], Some(A)),
    ];
    assert!(isomorphic_datasets(&d1, &d1)?);

    let d2 = d1[1..].to_vec();
    assert!(!isomorphic_datasets(&d1, &d2)?);
    assert!(!isomorphic_datasets(&d2, &d1)?);

    let mut d3 = d1.clone();
    d3[1].1 = Some(B);
    assert!(!isomorphic_datasets(&d1, &d3)?);
    assert!(!isomorphic_datasets(&d3, &d1)?);

    let d4 = vec![
        ([A, B, C], None),
        ([A, B, MyTerm::Triple("c d e")], Some(A)),
    ];
    assert!(!isomorphic_datasets(&d1, &d4)?);
    assert!(!isomorphic_datasets(&d4, &d1)?);
    Ok(())
}

fn make_chain(ids: &'static str) -> Vec<[MyTerm; 4]> {
    let rel = MyTerm::Iri("tag:rel");
    let nodes: Vec<_> = (0..ids.len())
        .map(|i| MyTerm::Bnode(&ids[i..i + 1]))
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
    let d2 = make_chain("EDCBAJIHGF");
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
    let d2 = make_chain("BAB");
    assert!(isomorphic_datasets(&d1, &d2)?);
    assert!(isomorphic_datasets(&d2, &d1)?);
    Ok(())
}

#[test]
fn cycle_long() -> Result<(), Box<dyn Error>> {
    let d1 = make_chain("abcdefghia");
    assert!(isomorphic_datasets(&d1, &d1)?);
    let d2 = make_chain("EBCDAIGHFE");
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
    let typ = MyTerm::Iri("tag:type");
    let dist = MyTerm::Iri("tag:Distinguished");

    let mut d1 = make_chain("abca");
    let mut d1b = make_chain("defgd");
    d1.append(&mut d1b);
    d1.push([d1[0][0], typ, dist, d1[0][0]]);

    let mut d2 = make_chain("abcdefga");
    d2.push([d2[0][0], typ, dist, d2[0][0]]);
    assert!(!isomorphic_datasets(&d1, &d2)?);
    Ok(())
}

fn make_clique(ids: &'static str) -> Vec<[MyTerm; 4]> {
    let rel = MyTerm::Iri("tag:rel");
    let nodes: Vec<_> = (0..ids.len())
        .map(|i| MyTerm::Bnode(&ids[i..i + 1]))
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

fn make_tree(ids: &'static str) -> Vec<[MyTerm; 4]> {
    let rel = MyTerm::Iri("tag:rel");
    let nodes: Vec<_> = (0..ids.len())
        .map(|i| MyTerm::Bnode(&ids[i..i + 1]))
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
    let rel = MyTerm::Iri("tag:rel");
    let b1 = MyTerm::Bnode("b1");
    let b2 = MyTerm::Bnode("b2");
    let b3 = MyTerm::Bnode("b3");
    let b4 = MyTerm::Bnode("b4");

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

#[derive(Clone, Copy, Debug)]
enum MyTerm {
    Iri(&'static str),
    Bnode(&'static str),
    String(&'static str),
    Number(i32),
    Triple(&'static str),
}

impl Term for MyTerm {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        match self {
            MyTerm::Iri(_) => TermKind::Iri,
            MyTerm::Bnode(_) => TermKind::BlankNode,
            MyTerm::String(_) => TermKind::Literal,
            MyTerm::Number(_) => TermKind::Literal,
            MyTerm::Triple(_) => TermKind::Triple,
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        if let MyTerm::Iri(iri) = *self {
            Some(IriRef::new_unchecked(iri.into()))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        if let MyTerm::Bnode(id) = *self {
            Some(BnodeId::new_unchecked(id.into()))
        } else {
            None
        }
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        if let MyTerm::String(val) = *self {
            Some(val.into())
        } else if let MyTerm::Number(n) = *self {
            Some(format!("{}", n).into())
        } else {
            None
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        if let MyTerm::String(_) = *self {
            xsd::string.iri()
        } else if let MyTerm::Number(_) = *self {
            xsd::integer.iri()
        } else {
            None
        }
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<MownStr>> {
        None
    }

    fn triple(&self) -> Option<[MyTerm; 3]> {
        if let MyTerm::Triple(spo) = *self {
            let spo: Vec<_> = spo
                .split(' ')
                .map(|t| {
                    if t.starts_with('#') {
                        MyTerm::Iri(t)
                    } else {
                        MyTerm::Bnode(t)
                    }
                })
                .collect();
            Some([spo[0], spo[1], spo[2]])
        } else {
            None
        }
    }

    fn to_triple(self) -> Option<[MyTerm; 3]> {
        self.triple()
    }
}

#[test]
fn my_term() {
    assert_consistent_term_impl(&MyTerm::Iri("tag:1"));
    assert_consistent_term_impl(&MyTerm::Bnode("b1"));
    assert_consistent_term_impl(&MyTerm::String("hello world"));
    assert_consistent_term_impl(&MyTerm::Number(42));
    assert_consistent_term_impl(&MyTerm::Triple("b1 p b2"));
}
