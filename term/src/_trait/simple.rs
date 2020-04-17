//! A minimalistic implementation of trait `Term`
use crate::mown_str::MownStr;
use super::*;

/// Simple type implemeting Term
#[derive(Clone, Debug)]
pub struct SimpleTerm<'a> {
    inner: SimpleTermInner<'a>,
}

impl<'a> SimpleTerm<'a> {
    pub fn new_iri<T: Into<MownStr<'a>>>(value: T) -> SimpleTerm<'a> {
        // TODO check validity of IRI
        SimpleTerm{
            inner: Iri(value.into())
        }
    }

    pub fn new_iri_unchecked(value: MownStr<'a>) -> SimpleTerm<'a> {
        SimpleTerm{
            inner: Iri(value)
        }
    }

    pub fn new_literal_dt<T, U>(value: T, datatype: U) -> SimpleTerm<'a>
    where
        T: Into<MownStr<'a>>,
        U: Into<MownStr<'a>>,
    {
        // TODO check validity of datatype IRI
        SimpleTerm{
            inner: LitDt(value.into(), datatype.into())
        }
    }

    pub fn new_literal_dt_unchecked(value: MownStr<'a>, datatype: MownStr<'a>) -> SimpleTerm<'a> {
        SimpleTerm{
            inner: LitDt(value, datatype)
        }
    }

    pub fn new_literal_lang<T, U>(value: T, tag: U) -> SimpleTerm<'a>
    where
        T: Into<MownStr<'a>>,
        U: Into<MownStr<'a>>,
    {
        // TODO check validity of language tag
        SimpleTerm{
            inner: LitLang(value.into(), tag.into())
        }
    }

    pub fn new_literal_lang_unchecked(value: MownStr<'a>, tag: MownStr<'a>) -> SimpleTerm<'a> {
        SimpleTerm{
            inner: LitLang(value, tag)
        }
    }

    pub fn new_bnode<T: Into<MownStr<'a>>>(value: T) -> SimpleTerm<'a> {
        // TODO check validity of bnode ID
        SimpleTerm{
            inner: BNode(value.into())
        }
    }

    pub fn new_bnode_unchecked(value: MownStr<'a>) -> SimpleTerm<'a> {
        SimpleTerm{
            inner: BNode(value)
        }
    }

    pub fn new_variable<T: Into<MownStr<'a>>>(value: T) -> SimpleTerm<'a> {
        // TODO check validity of variable name
        SimpleTerm{
            inner: Variable(value.into())
        }
    }

    pub fn new_variable_unchecked(value: MownStr<'a>) -> SimpleTerm {
        SimpleTerm{
            inner: Variable(value)
        }
    }

}

#[derive(Clone, Debug)]
enum SimpleTermInner<'a> {
    Iri(MownStr<'a>),
    LitDt(MownStr<'a>, MownStr<'a>),
    LitLang(MownStr<'a>, MownStr<'a>),
    BNode(MownStr<'a>),
    Variable(MownStr<'a>),
}
use SimpleTermInner::*;

impl<'a> Term for SimpleTerm<'a> {
    fn kind(&self) -> TermKind {
        match self.inner {
            Iri(_) => TermKind::Iri,
            LitDt(..) => TermKind::Literal,
            LitLang(..) => TermKind::Literal,
            BNode(_) => TermKind::BNode,
            Variable(_) => TermKind::Variable,
        }
    }
    fn value(&self) -> MownStr {
        let v = match &self.inner {
            Iri(v) => v,
            LitDt(v, _) => v,
            LitLang(v, _) => v,
            BNode(v) => v,
            Variable(v) => v,
        };
        MownStr::Ref(v.as_ref())
    }
    fn datatype(&self) -> Option<MownStr> {
        if let LitDt(_, dt) = &self.inner {
            Some(MownStr::Ref(dt.as_ref()))
        } else {
            None
        }
    }
    fn language(&self) -> Option<MownStr> {
        if let LitLang(_, tag) = &self.inner {
            Some(MownStr::Ref(tag.as_ref()))
        } else {
            None
        }
    }
}

impl<'a, 'b> PartialEq<SimpleTerm<'a>> for SimpleTerm<'b> {
    fn eq(&self, other: &SimpleTerm<'a>) -> bool {
        match (&self.inner, &other.inner) {
            (Iri(v1), Iri(v2)) => v1 == v2,
            (LitDt(v1, dt1), LitDt(v2, dt2)) => v1 == v2 && dt1 == dt2,
            (LitLang(v1, tag1), LitLang(v2, tag2)) => v1 == v2 && tag1 == tag2,
            (BNode(v1), BNode(v2)) => v1 == v2,
            (Variable(v1), Variable(v2)) => v1 == v2,
            _ => false,
        }
    }
}

impl<'a> Eq for SimpleTerm<'a> {}