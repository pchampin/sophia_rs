//! Utility code for pretty-printing Turtle and `TriG`.
//!
//! Possible improvements:
//! 1. `PrettifiableDataset` should encapsulate some of the "indexes" built by Prettifier
//!    (labelled, `subject_types`, `named_graphs`)
//!    and build directly in `CollectibleDataset::from_quad_source()`.
//!
//! 2. Instead of writing directly to the output,
//!    generate a hierarchical structure,
//!    and decide on line breaks and indentation based on the overall structure,
//!    rather than a priori.

use crate::serializer::_common::{write_language_string, write_typed_literal};

use super::turtle::TurtleConfig;
use sophia_api::MownStr;
use sophia_api::dataset::Dataset;
use sophia_api::ns::rdf;
use sophia_api::prefix::PrefixMap;
use sophia_api::quad::{Gspo, Quad, Spog, iter_spog};
use sophia_api::term::matcher::Any;
use sophia_api::term::{GraphName, SimpleTerm, Term, TermKind};
use sophia_iri::IriRef;
use std::cmp::Ordering;
use std::collections::btree_map::Entry::{Occupied, Vacant};
use std::collections::{BTreeMap, BTreeSet};
use std::io::{self, Write};
use std::ops::Range;

pub type PrettifiableDataset<'a> = BTreeSet<Gspo<SimpleTerm<'a>>>;

/// Serialize `dataset` in pretty `TriG` on `write`, using the given `config`.
///
/// NB: if dataset only contains a default graph,
/// the resulting `TriG` will be valid Turtle.
pub fn prettify<W>(
    dataset: PrettifiableDataset<'_>,
    mut write: W,
    config: &TurtleConfig,
    base_indent: &str,
) -> io::Result<()>
where
    W: io::Write,
{
    assert!(base_indent.chars().all(char::is_whitespace));
    write_prefixes(&mut write, &config.prefix_map[..])?;

    let mut p = Prettifier::new(&dataset, &mut write, base_indent.into(), config);
    p.write_all()?;
    write.flush()?;
    Ok(())
}

/// write the prefix declarations of the given `prefix_map`, using SPARQL style.
fn write_prefixes<W, P>(mut write: W, prefix_map: &P) -> io::Result<()>
where
    W: io::Write,
    P: PrefixMap + ?Sized,
{
    for (pre, iri) in prefix_map.iter() {
        writeln!(&mut write, "PREFIX {}: <{}>", pre.as_str(), iri.as_str())?;
    }
    Ok(())
}

struct Prettifier<'a, W> {
    dataset: &'a PrettifiableDataset<'a>,
    write: W,
    indent: String,
    config: &'a TurtleConfig,
    labelled: BTreeSet<&'a SimpleTerm<'a>>,
    annotations: BTreeMap<Spog<&'a SimpleTerm<'a>>, Vec<(&'a SimpleTerm<'a>, bool)>>,
    subject_types: Vec<(
        GraphName<&'a SimpleTerm<'a>>,
        &'a SimpleTerm<'a>,
        SubjectType,
    )>,
    lists: BTreeMap<&'a SimpleTerm<'a>, Vec<&'a SimpleTerm<'a>>>,
    graph_range: Range<usize>,
}

type SubjectsWithType<'a> = [(
    GraphName<&'a SimpleTerm<'a>>,
    &'a SimpleTerm<'a>,
    SubjectType,
)];

impl<'a, W: Write> Prettifier<'a, W> {
    fn new(
        dataset: &'a PrettifiableDataset<'a>,
        write: W,
        indent: String,
        config: &'a TurtleConfig,
    ) -> Self {
        let mut labelled = build_labelled(dataset);
        let mut annotations = BTreeMap::new();
        let mut subject_types = build_subject_types(dataset, &mut labelled, &mut annotations);
        let lists = build_lists(dataset, &mut subject_types);
        let subject_types: Vec<_> = subject_types
            .into_iter()
            .map(|((g, s), st)| (g, s, st))
            .collect();
        // see how many subjects are in the default graph,
        // using the fact that the default graph (None) comes first in the sort order
        let upper = subject_types
            .iter()
            .take_while(|(g, _, _)| g.is_none())
            .count();
        let graph_range = 0..upper;

        Self {
            dataset,
            write,
            indent,
            config,
            labelled,
            annotations,
            subject_types,
            lists,
            graph_range,
        }
    }

    fn write_all(&mut self) -> io::Result<()> {
        if self.subject_types.is_empty() {
            return Ok(());
        }
        if self.graph_range.end > 0 {
            // default graph is not empty
            self.write_graph()?;
        }
        // then process named graphs
        while let Some(g) = self.next_graph() {
            self.write_newline()?;
            self.write_bytes(b"GRAPH ")?;
            self.write_term(g)?;
            self.write_bytes(b" {")?;
            self.indent();
            self.write_graph()?;
            self.unindent();
            self.write_bytes(b"}\n")?;
        }
        Ok(())
    }

    /// Pre-condition:
    /// self.subject_types[self.graph_range] is not empty;
    /// all its elements have the same graph name,
    /// and all subjects of that graph are contained in it.
    fn write_graph(&mut self) -> io::Result<()> {
        for i in self.graph_range.clone() {
            let (_, s, st) = &self.subject_types[i];
            match st {
                SubjectType::Root => self.write_tree(s, false)?,
                SubjectType::SingleReifier => self.write_tree(s, true)?,
                _ => continue,
            }
            self.subject_types[i].2 = SubjectType::Done;
        }
        Ok(())
    }

    fn write_tree(&mut self, root: &'a SimpleTerm<'a>, reifier: bool) -> io::Result<()> {
        self.write_newline()?;
        if reifier {
            self.write_reified(root)?;
        } else {
            self.write_term(root)?;
        }
        self.write_properties(root, reifier)?;
        self.write_bytes(b".\n")?;
        Ok(())
    }

    /// Precondition: root is a SingleReifier
    fn write_reified(&mut self, root: &'a SimpleTerm<'a>) -> io::Result<()> {
        let g = self.current_graph_name();
        let [s1, p1, o1] = self
            .dataset
            .quads_matching([root], [rdf::reifies], TermKind::Triple, [g])
            .next()
            .unwrap()
            .unwrap()
            .o()
            .triple()
            .unwrap();
        self.write_bytes(b"<< ")?;
        self.write_term(s1)?;
        self.write_bytes(b" ")?;
        self.write_term(p1)?;
        self.write_bytes(b" ")?;
        self.write_term(o1)?;
        if !root.is_blank_node() || self.labelled.contains(&root) {
            self.write_bytes(b" ~ ")?;
            self.write_term(root)?;
        }
        self.write_bytes(b" >>")?;
        Ok(())
    }

    fn write_properties(
        &mut self,
        subject: &'a SimpleTerm<'a>,
        skip_reifies: bool,
    ) -> io::Result<()> {
        let mut predicate = None;
        self.indent(); // to predicate-level
        let g = self.current_graph_name();
        let types: Vec<_> = self
            .dataset
            .quads_matching([subject], [rdf::type_], Any, [g])
            .map(Result::unwrap)
            .inspect(|q| {
                if predicate.is_none() {
                    predicate = Some(q.p());
                }
            })
            .map(|q| q.o())
            .collect();
        if !types.is_empty() {
            self.write_bytes(b" a ")?;
            self.indent(); // to object-level
            self.write_objects(subject, predicate.unwrap(), &types)?;
        }
        // NB: we know that PrettifiableDataset<'_> iterates triples grouped by predicate
        // (it uses a GSPO index)
        for t in self
            .dataset
            .quads_matching([subject], Any, Any, [g])
            .map(Result::unwrap)
        {
            let p = t.p();
            if rdf::type_ == p {
                continue;
            }
            if skip_reifies && rdf::reifies == p {
                continue;
            }
            if Some(p) != predicate {
                if predicate.is_some() {
                    self.write_bytes(b";")?;
                    self.unindent(); // back to predicate-level
                }
                predicate = Some(p);
                self.write_newline()?;
                self.write_term(p)?;
                self.write_bytes(b" ")?;
                self.indent(); // to object-level
            } else {
                self.write_bytes(b",")?;
                self.write_newline()?;
            }
            self.write_object(subject, predicate.unwrap(), t.o())?;
        }
        if predicate.is_some() {
            self.unindent(); // back to predicate-level
        }
        self.unindent(); // back to original level
        Ok(())
    }

    fn write_objects(
        &mut self,
        subject: &'a SimpleTerm<'a>,
        predicate: &'a SimpleTerm<'a>,
        objects: &[&'a SimpleTerm<'a>],
    ) -> io::Result<()> {
        self.write_object(subject, predicate, objects[0])?;
        for obj in &objects[1..] {
            self.write_bytes(b",")?;
            self.write_newline()?;
            self.write_object(subject, predicate, obj)?;
        }
        Ok(())
    }

    fn write_object(
        &mut self,
        subject: &'a SimpleTerm<'a>,
        predicate: &'a SimpleTerm<'a>,
        object: &'a SimpleTerm<'a>,
    ) -> io::Result<()> {
        if rdf::nil == object {
            self.write_bytes(b"()")?;
        } else {
            self.write_term(object)?;
        }
        let key = ([subject, predicate, object], self.current_graph_name());
        if let Some(reifiers) = self.annotations.remove(&key) {
            for (r, has_props) in reifiers {
                if !r.is_blank_node() || self.labelled.contains(r) || !has_props {
                    self.write_bytes(b" ~ ")?;
                    self.write_term(r)?;
                }
                if has_props {
                    self.write_bytes(b" {|")?;
                    self.write_properties(r, true)?;
                    self.write_bytes(b" |}")?;
                }
            }
        }
        Ok(())
    }

    fn write_term(&mut self, term: &'a SimpleTerm<'a>) -> io::Result<()> {
        use SimpleTerm::*;
        match term {
            Iri(iri_ref) => self.write_iri(iri_ref),
            BlankNode(_) => self.write_bnode(term),
            LiteralDatatype(lex, dt) => {
                write_typed_literal(lex, dt, &mut self.write, &self.config.prefix_map)
            }
            LiteralLanguage(lex, tag, dir) => {
                write_language_string(lex, tag, *dir, &mut self.write)
            }
            Triple(triple) => {
                self.write_bytes(b"<<( ")?;
                for t in &triple[..] {
                    self.write_term(t)?;
                    self.write_bytes(b" ")?;
                }
                self.write_bytes(b")>>")
            }
            Variable(var_name) => write!(&mut self.write, "?{}", var_name.as_str()),
        }
    }

    fn write_iri(&mut self, iri: &IriRef<MownStr>) -> io::Result<()> {
        super::_common::write_iri(iri, &mut self.write, &self.config.prefix_map)
    }

    fn write_bnode(&mut self, bn: &'a SimpleTerm<'a>) -> io::Result<()> {
        if let Some(items) = self.lists.remove(&bn) {
            self.write_bytes(b"(")?;
            self.indent();
            for item in items {
                self.write_newline()?;
                self.write_term(item)?;
            }
            self.unindent();
            self.write_newline()?;
            self.write_bytes(b")")?;
        } else if self.labelled.contains(&bn) {
            write!(self.write, "_:{}", bn.bnode_id().unwrap().as_str())?;
        } else if let Some(i) = self.find_st_index(bn) {
            let (_, s, st) = self.subject_types[i];
            match st {
                SubjectType::SubTree => {
                    self.write_bytes(b"[")?;
                    self.write_properties(s, false)?;
                    self.write_bytes(b"]")?;
                    self.subject_types[i].2 = SubjectType::Done;
                }
                SubjectType::Root | SubjectType::SingleReifier | SubjectType::Annotation => {
                    self.write_bytes(b"[]")?;
                }
                SubjectType::Done => {}
            }
        } else {
            self.write_bytes(b"[]")?;
        }
        Ok(())
    }

    fn write_newline(&mut self) -> io::Result<()> {
        self.write_bytes(b"\n")?;
        self.write.write_all(self.indent.as_bytes())
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> io::Result<()> {
        self.write.write_all(bytes)
    }

    fn indent(&mut self) {
        self.indent.push_str(self.config.indentation());
    }

    fn unindent(&mut self) {
        let ilen = self.config.indentation().len();
        self.indent.truncate(self.indent.len() - ilen);
    }

    fn next_graph(&mut self) -> Option<&'a SimpleTerm<'a>> {
        if self.graph_range.end >= self.subject_types.len() {
            None
        } else {
            let start = self.graph_range.end;
            let g1 = self.subject_types[start].0;
            let c = self.subject_types[start..]
                .iter()
                .take_while(|(g2, _, _)| g1 == *g2)
                .count();
            self.graph_range = start..(start + c);
            Some(g1.unwrap())
        }
    }

    fn current_graph_name(&self) -> GraphName<&'a SimpleTerm<'a>> {
        self.subject_types[self.graph_range.start].0
    }

    fn find_st_index<T: Term>(&self, term: T) -> Option<usize> {
        find_subject(term, &self.subject_types[self.graph_range.clone()])
            .map(|i| i + self.graph_range.start)
    }
}

/// blank nodes MUST be labelled (as opposed to described with square brackets) if
/// - they are used in several named graphs, or
/// - they are used several times as object, or
/// - they are used as predicate or `graph_name`, or
/// - they are used in a triple term, or
/// - they are involved in a blank node cycle.
///
/// NB: actually, blank nodes in triple terms can sometimes be written as ANON (`[]`),
/// but detecting this is pretty involved, for example:
/// if the blank node is used only once in the triple term AND
/// - if the triple term is used only once with an arbitrary predicate OR
/// - if the triple term is used several times, only with rdf:reifies AND the triple is also asserted
///   (then we can "factorize" the blank node using the annotation syntax).
///
/// There may be other corner cases...
/// This implementation currently does not support that.
fn build_labelled<'a>(d: &'a PrettifiableDataset) -> BTreeSet<&'a SimpleTerm<'a>> {
    let mut profiles = BTreeMap::new();
    for q in d.quads() {
        let q = q.unwrap();
        for (i, t) in iter_spog(q).enumerate() {
            match t.kind() {
                TermKind::BlankNode => {
                    profiles
                        .entry(t)
                        .and_modify(|profile: &mut BnodeProfile| {
                            if !profile.bad {
                                profile.add_named_graph(q.g());
                                profile.update_positions(i, &q.spog());
                            }
                        })
                        .or_insert_with(|| BnodeProfile {
                            bad: (i == 1 || i == 3),
                            named_graphs: [q.g()].into_iter().collect(),
                            out_degree: usize::from(i == 0),
                            predecessor: if i == 2 { Some(q.s()) } else { None },
                            visited: false,
                        });
                }
                TermKind::Triple => {
                    for a in t.atoms().filter(Term::is_blank_node) {
                        profiles
                            .entry(a)
                            .and_modify(|profile| profile.bad = true)
                            .or_insert_with(|| BnodeProfile {
                                bad: true,
                                named_graphs: Default::default(),
                                out_degree: 0,
                                predecessor: None,
                                visited: false,
                            });
                    }
                }
                _ => (),
            }
        }
    }
    // detect blank node cycles
    let keys: Vec<_> = profiles.keys().copied().collect();
    for key in keys {
        let profile = profiles.get_mut(&key).unwrap();
        if profile.bad || profile.visited {
            continue;
        }
        profile.visited = true;
        let mut current = profile.predecessor;
        while let Some(t) = current {
            if let Some(p) = profiles.get_mut(&t) {
                if t == key {
                    p.bad = true;
                    break;
                } else if p.bad || p.visited {
                    break;
                } else {
                    p.visited = true;
                    current = p.predecessor;
                }
            } else {
                break;
            }
        }
    }
    profiles
        .into_iter()
        .filter_map(|(key, profile)| profile.bad.then_some(key))
        .collect()
}

struct BnodeProfile<'a> {
    bad: bool,
    named_graphs: BTreeSet<GraphName<&'a SimpleTerm<'a>>>,
    out_degree: usize,
    predecessor: Option<&'a SimpleTerm<'a>>,
    visited: bool,
}

impl<'a> BnodeProfile<'a> {
    fn add_named_graph(&mut self, g: GraphName<&'a SimpleTerm<'a>>) {
        self.named_graphs.insert(g);
        if self.named_graphs.len() > 1 {
            self.bad = true;
        }
    }
    fn update_positions(&mut self, pos: usize, quad: &Spog<&'a SimpleTerm>) {
        if pos == 0 {
            self.out_degree += 1;
        } else if pos == 2 {
            if self.predecessor.is_none() {
                self.predecessor = Some(quad.s());
            } else {
                self.bad = true;
            }
        } else {
            debug_assert!(pos == 1 || pos == 3);
            self.bad = true;
        }
    }
}

/// For each pair (graph-name, subject), determine the subject type
fn build_subject_types<'a>(
    d: &'a PrettifiableDataset,
    labelled: &mut BTreeSet<&'a SimpleTerm<'a>>,
    annotations: &mut BTreeMap<Spog<&'a SimpleTerm<'a>>, Vec<(&'a SimpleTerm<'a>, bool)>>,
) -> BTreeMap<(GraphName<&'a SimpleTerm<'a>>, &'a SimpleTerm<'a>), SubjectType> {
    d.iter()
        .map(|q| (q.g(), q.s()))
        .dedup()
        .map(|(g, s)| {
            use TermKind::{BlankNode, Triple};
            let tts: Vec<_> = d
                .quads_matching([s], [rdf::reifies], Triple, [g])
                .take(2)
                .map(|r| r.unwrap().o().triple().unwrap())
                .collect();
            let st = if let [[s1, p1, o1]] = tts[..] {
                // s reifies exactly one triple term
                if d.quads_matching(Any, Any, [s], [g]).next().is_some() {
                    // if reifier is used in the object position, it must be labelled
                    labelled.insert(s);
                }
                if d.quads_matching([s1], [p1], [o1], [g]).next().is_some() {
                    let key = ([s1, p1, o1], g);
                    let has_props = d.quads_matching([s], Any, Any, [g]).take(2).count() == 2; // has at least 1 property besides rdf:reifies
                    annotations.entry(key).or_default().push((s, has_props));
                    // the reified triple is also asserted
                    SubjectType::Annotation
                } else {
                    // the reified triple is not asserted
                    SubjectType::SingleReifier
                }
            } else if s.kind() == BlankNode
                && !labelled.contains(&s)
                && d.quads_matching(Any, Any, [s], [g]).take(2).count() == 1
            {
                // bnodes that do not need to be labelled and have exactly one incoming arc can be serialized as subtrees
                SubjectType::SubTree
            } else {
                SubjectType::Root
            };
            ((g, s), st)
        })
        .collect()
}

/// Categorization of triple subjects
#[derive(Copy, Clone, Debug, PartialEq)]
enum SubjectType {
    /// A node that must be the root of a "tree"
    Root,
    /// A node that can be used as a subtree (square brackets with property list)
    SubTree,
    /// A reifier of a single and unasserted triple
    SingleReifier,
    /// A reifier of a single and asserted triple
    Annotation,
    /// A reifier of an asserted triple
    Done,
}

/// Find all well-formed lists in this dataset
fn build_lists<'a>(
    d: &'a PrettifiableDataset,
    subject_types: &mut BTreeMap<(GraphName<&'a SimpleTerm<'a>>, &'a SimpleTerm<'a>), SubjectType>,
) -> BTreeMap<&'a SimpleTerm<'a>, Vec<&'a SimpleTerm<'a>>> {
    let mut preds = BTreeMap::new();
    let mut seeds = vec![];
    use TermKind::BlankNode;
    for q in d.quads_matching(BlankNode, [rdf::rest], Any, Any) {
        let ([s, _, o], g) = q.unwrap().spog();
        if subject_types.get(&(g, s)) != Some(&SubjectType::SubTree) {
            continue;
        }
        if rdf::nil == o {
            if let Some(val) = list_item(s, d) {
                seeds.push(((g, s), vec![val]));
                subject_types.remove(&(g, s));
            }
        } else if o.is_blank_node() {
            match preds.entry(o) {
                Vacant(e) => {
                    e.insert(s);
                }
                Occupied(e) => {
                    e.remove();
                }
            }
        }
    }
    seeds
        .into_iter()
        .map(|((g, mut bn), mut items)| {
            loop {
                if let Some(pred) = preds.get(&bn).copied()
                    && let Some(val) = list_item(pred, d)
                {
                    bn = pred;
                    items.push(val);
                    subject_types.remove(&(g, pred));
                    continue;
                }
                break;
            }
            items.reverse();
            (bn, items)
        })
        .collect()
}

fn list_item<'a>(s: &'a SimpleTerm<'a>, d: &'a PrettifiableDataset) -> Option<&'a SimpleTerm<'a>> {
    let mut ret = None;
    for q in d.quads_matching([s], Any, Any, Any) {
        let q = q.unwrap();
        if rdf::rest == q.p() {
            continue;
        } else if rdf::first == q.p() && ret.is_none() {
            ret = Some(q.o());
        } else {
            return None;
        }
    }
    ret
}

fn find_subject<T: Term>(s: T, swt: &SubjectsWithType) -> Option<usize> {
    if swt.is_empty() {
        None
    } else {
        let m = swt.len() / 2;
        match Term::cmp(&swt[m].1, s.borrow_term()) {
            Ordering::Less => find_subject(s, &swt[m + 1..]).map(|i| i + m + 1),
            Ordering::Equal => Some(m),
            Ordering::Greater => find_subject(s, &swt[..m]),
        }
    }
}

// ---------------------------------------------------------------------------------
//                                      inners
// ---------------------------------------------------------------------------------

trait Dedup: Iterator + Sized {
    fn dedup(self) -> DedupIterator<Self> {
        DedupIterator {
            previous: None,
            inner: self,
        }
    }
}

impl<I: Iterator> Dedup for I {}

struct DedupIterator<I: Iterator> {
    previous: Option<I::Item>,
    inner: I,
}

impl<I: Iterator> Iterator for DedupIterator<I>
where
    I::Item: Clone + Eq,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let some_item = self.inner.next();
            #[allow(clippy::question_mark)]
            if some_item.is_none() {
                return None;
            }
            if some_item != self.previous {
                self.previous.clone_from(&some_item);
                return some_item;
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.inner.size_hint();
        (lower.max(1), upper)
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn dedup() {
        let v1 = [1, 1, 1, 2, 2, 1, 3, 3];
        let v2: Vec<_> = v1.into_iter().dedup().collect();
        assert_eq!(&v2, &[1, 2, 1, 3]);
    }

    #[test]
    fn relative_iri() -> Result<(), Box<dyn std::error::Error>> {
        let iri = IriRef::new_unchecked("");
        let graph = vec![[iri, iri, iri]];
        let config = TurtleConfig::new().with_pretty(true);
        use sophia_api::prelude::*;
        let pretty =
            crate::serializer::turtle::TurtleSerializer::new_stringifier_with_config(config)
                .serialize_triples(graph.triples())?
                .to_string();
        assert!(pretty.contains("<>"));
        // the goal is not to check the exact serialization,
        // but only that relative IRIs are supported even in debug mode
        Ok(())
    }
}
