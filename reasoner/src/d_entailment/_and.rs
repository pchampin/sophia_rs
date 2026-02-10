use std::{borrow::Borrow, marker::PhantomData};

use smallvec::SmallVec;
use sophia_api::{MownStr, term::IriRef};

use crate::d_entailment::{IllTypedLiteral, LexDt, Recognized};

/// A [`Recognized`] recognizing all datatypes from R1 and R2.
#[derive(Clone, Copy, Default)]
pub struct And<R1, R2>(PhantomData<(R1, R2)>);

impl<R1: Recognized + 'static, R2: Recognized + 'static> Recognized for And<R1, R2> {
    fn try_normalize<T: Borrow<str>>(
        lex_dt: LexDt<T>,
    ) -> Result<Result<LexDt<Box<str>>, IllTypedLiteral>, LexDt<T>> {
        R1::try_normalize(lex_dt).or_else(R2::try_normalize)
    }

    fn datatypes() -> impl Iterator<Item = IriRef<MownStr<'static>>> + Send {
        R1::datatypes().chain(R2::datatypes())
    }

    fn witnesses() -> impl Iterator<Item = (MownStr<'static>, IriRef<MownStr<'static>>)> + Send {
        R1::witnesses().chain(R2::witnesses())
    }

    fn datatypes_for(
        lex: &str,
        datatype: IriRef<&str>,
    ) -> Option<SmallVec<[IriRef<MownStr<'static>>; 16]>> {
        match (
            R1::datatypes_for(lex, datatype),
            R2::datatypes_for(lex, datatype),
        ) {
            (None, None) => None,
            (Some(vec1), None) => Some(vec1),
            (None, Some(vec2)) => Some(vec2),
            (Some(mut vec1), Some(vec2)) => {
                vec1.extend(vec2);
                Some(vec1)
            }
        }
    }
}
