//! Utility code for pretty-printing Turtle and TriG.
//!
//! Possible improvements:
//! 1. PrettifiableDataset should encapsulate some of the "indexes" built by Prettifier
//! (labelled, subject_types, named_graphs)
//! and build directly in CollectibleDataset::from_quad_source().
//!
//! 2. Instead of writing directly to the output,
//! generate a hierarchical structure,
//! and decide on line breaks and indentation based on the overall structure,
//! rather than a priori.

use super::turtle::TurtleConfig;
use regex::Regex;
use sophia_api::dataset::Dataset;
use sophia_api::ns::{rdf, xsd};
use sophia_api::prefix::PrefixMap;
use sophia_api::quad::{iter_spog, Gspo, Quad, Spog};
use sophia_api::term::matcher::Any;
use sophia_api::term::{GraphName, SimpleTerm, Term, TermKind};
use sophia_api::triple::Triple;
use sophia_api::MownStr;
use sophia_iri::{Iri, IriRef};
use std::cmp::Ordering;
use std::collections::btree_map::Entry::{Occupied, Vacant};
use std::collections::{BTreeMap, BTreeSet};
use std::io::{self, Write};
use std::ops::Range;

pub type PrettifiableDataset<'a> = BTreeSet<Gspo<SimpleTerm<'a>>>;

/// Serialize `dataset` in pretty TriG on `write`, using the given `config`.
///
/// NB: if dataset only contains a default graph,
/// the resulting TriG will be valid Turtle.
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

/// write the prefix declarations of the given prefix_map, using SPARQL style.
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
        let labelled = build_labelled(dataset);
        let mut subject_types = build_subject_types(dataset, &labelled);
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
    /// swt is not empty;
    /// all its elements have the same graph name,
    /// and all subjects of that graph are contained in it.
    fn write_graph(&mut self) -> io::Result<()> {
        for i in self.graph_range.clone() {
            let (_, s, st) = &self.subject_types[i];
            if *st != SubjectType::Root {
                continue;
            }
            self.write_tree(s)?;
            self.subject_types[i].2 = SubjectType::Done;
        }
        /*
        // some blank node cycles can cause all of them to be SubTree;
        // here we detect and break these cycles
        for i in self.graph_range.clone() {
            let (_, s, st) = &self.subject_types[i];
                if *st == SubjectType::Done {
                continue
            }
            assert!(*st == SubjectType::SubTree);
            self.write_tree(*s)?;
            self.subject_types[i].2 = SubjectType::Done;
        }
        */
        Ok(())
    }

    fn write_tree(&mut self, root: &'a SimpleTerm<'a>) -> io::Result<()> {
        self.write_newline()?;
        self.write_term(root)?;
        self.write_properties(root)?;
        self.write_bytes(b".\n")?;
        Ok(())
    }

    fn write_properties(&mut self, subject: &'a SimpleTerm<'a>) -> io::Result<()> {
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
        self.write_term(object)?;
        let tr = SimpleTerm::Triple(Box::new([
            subject.clone(),
            predicate.clone(),
            object.clone(),
        ]));
        if let Some(i) = self.find_st_index(tr) {
            let (_, s, st) = self.subject_types[i];
            if st == SubjectType::Annotation {
                self.write_bytes(b" {|")?;
                self.write_properties(s)?;
                self.write_bytes(b" |}")?;
                self.subject_types[i].2 = SubjectType::Done;
            }
        }
        Ok(())
    }

    fn write_term(&mut self, term: &'a SimpleTerm<'a>) -> io::Result<()> {
        use TermKind::*;
        match term.kind() {
            Iri => self.write_iri(&term.iri().unwrap()),
            BlankNode => self.write_bnode(term),
            Literal => self.write_literal(term),
            Variable => {
                write!(&mut self.write, "?{}", term.variable().unwrap().as_str())
            }
            Triple => {
                self.write_bytes(b"<< ")?;
                for t in term.triple().unwrap() {
                    self.write_term(t)?;
                    self.write_bytes(b" ")?;
                }
                self.write_bytes(b">>")
            }
        }
    }

    fn write_iri(&mut self, iri: &IriRef<MownStr>) -> io::Result<()> {
        if rdf::nil == iri {
            return self.write_bytes(b"()");
        }
        let Some(iri) = Iri::new(iri.as_str()).ok()  else {
            return write!(self.write, "<{}>", iri.as_str())
        };
        match self
            .config
            .prefix_map
            .get_checked_prefixed_pair(iri, |txt| PN_LOCAL.is_match(txt))
        {
            Some((pre, suf)) => {
                write!(self.write, "{}:{}", pre.as_str(), suf)
            }
            None => {
                write!(self.write, "<{}>", iri.as_str())
            }
        }
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
                    self.write_properties(s)?;
                    self.write_bytes(b"]")?;
                    self.subject_types[i].2 = SubjectType::Done;
                }
                SubjectType::Root => {
                    self.write_bytes(b"[]")?;
                }
                _ => {}
            }
        } else {
            self.write_bytes(b"[]")?;
        }
        Ok(())
    }

    fn write_literal(&mut self, lit: &'a SimpleTerm<'a>) -> io::Result<()> {
        debug_assert!(lit.kind() == TermKind::Literal);
        let datatype = lit.datatype().unwrap();
        let value = lit.lexical_form().unwrap();
        if xsd::integer == datatype && INTEGER.is_match(&value)
            || xsd::decimal == datatype && DECIMAL.is_match(&value)
            || xsd::double == datatype && DOUBLE.is_match(&value)
            || xsd::boolean == datatype && BOOLEAN.is_match(&value)
        {
            self.write_bytes(value.as_bytes())?;
        } else {
            self.write_bytes(b"\"")?;
            super::nt::quoted_string(&mut self.write, value.as_bytes())?;
            self.write_bytes(b"\"")?;
            if let Some(tag) = lit.language_tag() {
                write!(self.write, "@{}", tag.as_str())?;
            } else if xsd::string != datatype {
                self.write_bytes(b"^^")?;
                self.write_iri(&datatype)?;
            }
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
    }
}

/// blank nodes MUST be labelled (as opposed to described with square brackets) if
/// - they are used in several named graphs, or
/// - they are used several times as object, or
/// - they are used as predicate or graph_name, or
/// - they are used in a quoted triple, or
/// - they are involved in a blank node cycle.
///
/// NB1: actually, blank nodes that are the subject of a quoted triple that is also
/// asserted (in the same graph) are not forced to be labelled,
/// because the quoted triple can be "hidden" by an annotating the asserted one.
///
/// NB2: there would be other cases where a bnode in a quoted triples could,
/// theory, be written using the square brackets, but the added value is not worth
/// the trouble of identifying those cases.
fn build_labelled<'a>(d: &'a PrettifiableDataset) -> BTreeSet<&'a SimpleTerm<'a>> {
    let mut profiles = BTreeMap::new();
    for q in d.quads() {
        let q = q.unwrap();
        for (i, t) in iter_spog(q).enumerate() {
            match t.kind() {
                TermKind::BlankNode => match profiles.entry(t) {
                    Vacant(e) => {
                        e.insert(BnodeProfile {
                            bad: (i == 1 || i == 3),
                            named_graphs: [q.g()].into_iter().collect(),
                            out_degree: usize::from(i == 0),
                            predecessor: if i == 2 { Some(q.s()) } else { None },
                            visited: false,
                        });
                    }
                    Occupied(mut e) => {
                        let profile = e.get_mut();
                        if !profile.bad {
                            profile.add_named_graph(q.g());
                            profile.update_positions(i, &q);
                        }
                    }
                },
                TermKind::Triple => {
                    let mut atoms = t.atoms();
                    let [s, p, o] = t.triple().unwrap().spo();
                    if s.is_blank_node() && Dataset::contains(d, s, p, o, q.g()).unwrap() {
                        atoms.next(); // skip the subject blank nodes in atoms
                                      // and leave it to the "asserted" blank node to determine
                                      // if it must be labelled or not
                    }
                    for a in t.atoms().filter(Term::is_blank_node) {
                        match profiles.entry(a) {
                            Vacant(e) => {
                                e.insert(BnodeProfile {
                                    bad: true,
                                    named_graphs: Default::default(),
                                    out_degree: 0,
                                    predecessor: None,
                                    visited: false,
                                });
                            }
                            Occupied(mut e) => {
                                let profile = e.get_mut();
                                profile.bad = true;
                            }
                        }
                    }
                }
                _ => (),
            }
        }
    }
    // detect blank node cycles
    let keys: Vec<_> = profiles.keys().cloned().collect();
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
    labelled: &BTreeSet<&'a SimpleTerm<'a>>,
) -> BTreeMap<(GraphName<&'a SimpleTerm<'a>>, &'a SimpleTerm<'a>), SubjectType> {
    d.iter()
        .map(|q| (q.g(), q.s()))
        .dedup()
        .map(|(g, s)| {
            use TermKind::*;
            let st = match s.kind() {
                BlankNode => {
                    if !labelled.contains(&s)
                        && d.quads_matching(Any, Any, [s], [g]).take(2).count() == 1
                    {
                        SubjectType::SubTree
                    } else {
                        SubjectType::Root
                    }
                }
                Triple => {
                    let tr = s.triple().unwrap();
                    if rdf::first != tr.p()
                        && rdf::rest != tr.p()
                        && Dataset::contains(d, tr.s(), tr.p(), tr.o(), g).unwrap()
                    {
                        SubjectType::Annotation
                    } else {
                        SubjectType::Root
                    }
                }
                _ => SubjectType::Root,
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
    /// A quoted triple that is also asserted
    Annotation,
    /// A dummy subject type, to indicate that this subject has been serialized already
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
                if let Some(pred) = preds.get(&bn).copied() {
                    if let Some(val) = list_item(pred, d) {
                        bn = pred;
                        items.push(val);
                        subject_types.remove(&(g, pred));
                        continue;
                    }
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

lazy_static::lazy_static! {
    /// Match an absolute IRI reference.
    pub(crate) static ref PN_LOCAL: Regex = Regex::new(r"(?x)^
        #(PN_CHARS_U | ':' | [0-9] | PLX)
        (
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_:0-9]
            # | PLX
            | \\ [_~.!$&'()*+,;=/?\#@%-]
            | % [0-9A-Fa-f]{2}
        )
        # ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
        (
            (
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}.:-]
                | \\ [_~.!$&'()*+,;=/?\#@%-]
                | % [0-9A-Fa-f]{2}
            )*
            (
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}:-]
                | \\ [_~.!$&'()*+,;=/?\#@%-]
                | % [0-9A-Fa-f]{2}
            )
        )?
    $").unwrap();
    pub(crate) static ref INTEGER: Regex = Regex::new(r"^[+-]?[0-9]+$").unwrap();
    pub(crate) static ref DECIMAL: Regex = Regex::new(r"^[+-]?[0-9]*.[0-9]+$").unwrap();
    pub(crate) static ref DOUBLE: Regex = Regex::new(r"(?x)^
      [+-]? ( [0-9]+ ( . [0-9]* )? | . [0-9]+ ) [eE] [+-]? [0-9]+
    $").unwrap();
    pub(crate) static ref BOOLEAN: Regex = Regex::new(r"^(true|false)$").unwrap();
}

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
                self.previous = some_item.clone();
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
pub(crate) mod test {
    use super::*;

    #[test]
    fn dedup() {
        let v1 = [1, 1, 1, 2, 2, 1, 3, 3];
        let v2: Vec<_> = v1.into_iter().dedup().collect();
        assert_eq!(&v2, &[1, 2, 1, 3]);
    }

    #[test]
    fn pn_local() {
        for positive in [
            "a",
            "aBc",
            "éàïsophia_api::graph::",
            ":::",
            "123",
            "%20%21%22",
            "\\%\\?\\&",
        ] {
            assert!(PN_LOCAL.is_match(positive), "{}", positive);
        }
        for negative in [" ", ".a", "a."] {
            assert!(!PN_LOCAL.is_match(negative), "{}", negative);
        }
    }

    #[test]
    fn double() {
        for positive in [
            "3.14e0",
            "+3.14e0",
            "-3.14e0",
            "3.14e+0",
            "3.14e-0",
            "0000e0000",
            ".1E0",
            "1.e+3",
            "1E-3",
        ] {
            assert!(DOUBLE.is_match(positive), "{}", positive);
        }
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
