use std::{borrow::Borrow, iter::empty};

use smallvec::SmallVec;
use sophia_api::{MownStr, term::IriRef};

use crate::d_entailment::{IllTypedLiteral, LexDt, Recognized};

/// A default [`Recognized`] recognizing no datatype.
#[derive(Clone, Copy, Default)]
pub struct Nothing;

impl Recognized for Nothing {
    fn try_normalize<T: Borrow<str>>(
        lex_dt: LexDt<T>,
    ) -> Result<Result<LexDt<Box<str>>, IllTypedLiteral>, LexDt<T>> {
        Err(lex_dt)
    }

    fn datatypes() -> impl Iterator<Item = IriRef<MownStr<'static>>> + Send {
        empty()
    }

    fn witnesses() -> impl Iterator<Item = (MownStr<'static>, IriRef<MownStr<'static>>)> + Send {
        empty()
    }

    fn datatypes_for(
        _lex: &str,
        _datatype: IriRef<&str>,
    ) -> Option<SmallVec<[IriRef<MownStr<'static>>; 16]>> {
        None
    }
}
