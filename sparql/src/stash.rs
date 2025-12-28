//! Extend [`ArcStrStash`] with useful methods

use std::sync::Arc;

use sophia_api::term::{BaseDirection, IriRef, LanguageTag, Term, VarName};
use sophia_term::{ArcStrStash, ArcTerm, GenericLiteral};
use spargebra::term::GroundTerm;

use crate::{term::ResultTerm, value::SparqlValue};

pub trait ArcStrStashExt {
    fn copy_result_term<T: Term>(&mut self, t: T) -> ResultTerm;
    fn copy_variable(&mut self, v: &spargebra::term::Variable) -> VarName<Arc<str>>;
    fn copy_ground_term(&mut self, gterm: &GroundTerm) -> ResultTerm;
}

impl ArcStrStashExt for ArcStrStash {
    fn copy_result_term<T: Term>(&mut self, t: T) -> ResultTerm {
        ResultTerm::from(self.copy_term(t))
    }

    fn copy_variable(&mut self, v: &spargebra::term::Variable) -> VarName<Arc<str>> {
        VarName::new_unchecked(self.copy_str(v.as_str()))
    }

    fn copy_ground_term(&mut self, gterm: &GroundTerm) -> ResultTerm {
        match gterm {
            GroundTerm::NamedNode(named_node) => {
                ArcTerm::Iri(IriRef::new_unchecked(self.copy_str(named_node.as_str()))).into()
            }
            GroundTerm::Literal(literal) => {
                let val = self.copy_str(literal.value());
                let lit = if let Some(tag) = literal.language() {
                    let tag = LanguageTag::new_unchecked(self.copy_str(tag));
                    let dir = literal.direction().map(|oxdir| match oxdir {
                        oxrdf::BaseDirection::Ltr => BaseDirection::Ltr,
                        oxrdf::BaseDirection::Rtl => BaseDirection::Rtl,
                    });
                    GenericLiteral::LanguageString(val, tag, dir)
                } else {
                    let dt = IriRef::new_unchecked(self.copy_str(literal.datatype().as_str()));
                    GenericLiteral::Typed(val, dt)
                };
                ArcTerm::Literal(lit).into()
            }
            GroundTerm::Triple(triple) => {
                let s = ArcTerm::Iri(IriRef::new_unchecked(
                    self.copy_str(triple.subject.as_str()),
                ))
                .into();
                let p = ArcTerm::Iri(IriRef::new_unchecked(
                    self.copy_str(triple.predicate.as_str()),
                ))
                .into();
                let o = self.copy_ground_term(&triple.object);
                [s, p, o].into()
            }
        }
    }
}

pub fn value_to_term<F: FnMut(&str) -> Arc<str>>(value: SparqlValue, factory: F) -> ResultTerm {
    let inner = value_ref_to_arcterm(&value, factory);
    ResultTerm::from_parts(inner, Some(value))
}

pub fn value_ref_to_arcterm<F: FnMut(&str) -> Arc<str>>(
    value: &SparqlValue,
    factory: F,
) -> ArcTerm {
    ArcTerm::Literal(if let SparqlValue::String(lex, Some((lang, dir))) = value {
        GenericLiteral::LanguageString(lex.clone(), lang.clone(), *dir)
    } else {
        GenericLiteral::Typed(value.lexical_form(factory), value.datatype())
    })
}
