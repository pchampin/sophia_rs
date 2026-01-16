use std::{collections::HashMap, sync::Arc};

use sophia_api::{
    prelude::Dataset,
    term::{BnodeId, Term},
    triple::Triple,
};
use sophia_term::ArcTerm;
use uuid::{Uuid, fmt::Hyphenated};

use crate::{
    Bindings, ResultTerm, SparqlWrapperError,
    binding::{Binding, BindingsIter},
};

pub struct ConstructIter<'a, D: Dataset + ?Sized> {
    template: Vec<[ArcTerm; 3]>,
    bindings: BindingsIter<'a, D>,
    #[expect(clippy::type_complexity)]
    current: Option<Result<(Binding, usize), SparqlWrapperError<D::Error>>>,
    bnodes: HashMap<BnodeId<Arc<str>>, ArcTerm>,
}

impl<'a, D: Dataset + ?Sized> ConstructIter<'a, D> {
    pub fn new(template: Vec<[ArcTerm; 3]>, bindings: Bindings<'a, D>) -> Self {
        let mut bindings = bindings.iter;
        let current = bindings.next().map(|res| res.map(|b| (b, 0)));
        let bnodes = HashMap::new();
        Self {
            template,
            bindings,
            current,
            bnodes,
        }
    }

    fn populate(
        &mut self,
        triple_template: &[ArcTerm; 3],
        binding: &Binding,
    ) -> Option<[ResultTerm; 3]> {
        // TODO once array::try_map is stable, use it instead
        let [s, p, o] = triple_template;
        let s = self.substitute(s, binding)?;
        let p = self.substitute(p, binding)?;
        let o = self.substitute(o, binding)?;
        Some([s, p, o])
    }

    fn substitute(&mut self, term_template: &ArcTerm, binding: &Binding) -> Option<ResultTerm> {
        Some(match term_template {
            ArcTerm::BlankNode(bnode_id) => self
                .bnodes
                .entry(bnode_id.clone())
                .or_insert_with(fresh_bnode)
                .clone()
                .into(),
            ArcTerm::Triple(triple) => self.populate(triple, binding)?.into(),
            ArcTerm::Variable(var_name) => binding.v.get(var_name.as_str())?.clone(),
            _ => term_template.clone().into(),
        })
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for ConstructIter<'a, D> {
    type Item = Result<[ResultTerm; 3], SparqlWrapperError<D::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.current.take()?;
        match res {
            Err(err) => Some(Err(err)),
            Ok((binding, i)) if i >= self.template.len() => {
                self.current = self.bindings.next().map(|res| res.map(|b| (b, 0)));
                self.bnodes.clear();
                self.next()
            }
            Ok((binding, i)) => {
                let template = self.template[i].clone();
                let populated = self
                    .populate(&template, &binding)
                    .filter(|[s, p, o]| strict_triple(s.inner(), p.inner(), o.inner()));
                self.current = Some(Ok((binding, i + 1)));
                if let Some(t) = populated {
                    Some(Ok(t))
                } else {
                    self.next()
                }
            }
        }
    }
}

fn fresh_bnode() -> ArcTerm {
    let mut buf = vec![b'b'; Hyphenated::LENGTH + 1];
    Uuid::new_v4().hyphenated().encode_lower(&mut buf[1..]);
    let str = unsafe {
        // SAFETY: we now that buf contain only ASCII characters
        String::from_utf8_unchecked(buf)
    };
    ArcTerm::BlankNode(BnodeId::new_unchecked(Arc::from(str)))
}

fn strict_triple(s: &ArcTerm, p: &ArcTerm, o: &ArcTerm) -> bool {
    strict_subject(s) && p.is_iri() && strict_object(o)
}

fn strict_subject(t: &ArcTerm) -> bool {
    t.is_iri() || t.is_blank_node()
}
fn strict_object(t: &ArcTerm) -> bool {
    match t {
        ArcTerm::Variable(_) => false,
        ArcTerm::Triple(tr) => strict_triple(tr.s(), tr.p(), tr.o()),
        _ => true,
    }
}
