//! Extend [`ArcStrStash`] with useful methods

use std::sync::Arc;

use sophia_api::term::{Term, VarName};
use sophia_term::{ArcStrStash, ArcTerm, GenericLiteral};

use crate::{term::ResultTerm, value::SparqlValue};

pub trait ArcStrStashExt {
    fn copy_result_term<T: Term>(&mut self, t: T) -> ResultTerm;
    fn value_to_term(&mut self, n: SparqlValue) -> ResultTerm;
    fn copy_variable(&mut self, v: &spargebra::term::Variable) -> VarName<Arc<str>>;
}

impl ArcStrStashExt for ArcStrStash {
    fn copy_result_term<T: Term>(&mut self, t: T) -> ResultTerm {
        ResultTerm::from(self.copy_term(t))
    }

    fn copy_variable(&mut self, v: &spargebra::term::Variable) -> VarName<Arc<str>> {
        VarName::new_unchecked(self.copy_str(v.as_str()))
    }

    fn value_to_term(&mut self, value: SparqlValue) -> ResultTerm {
        value_to_term(value, |txt| self.copy_str(txt))
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
    ArcTerm::Literal(if let SparqlValue::String(lex, Some(tag)) = value {
        GenericLiteral::LanguageString(lex.clone(), tag.clone())
    } else {
        GenericLiteral::Typed(value.lexical_form(factory), value.datatype())
    })
}
