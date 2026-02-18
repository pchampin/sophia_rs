//! I define the trait [`Recognized`], as well as a number of implementations of that trait.
use std::borrow::Borrow;

use smallvec::SmallVec;
use sophia_api::{
    MownStr,
    source::TripleSource,
    term::{IriRef, SimpleTerm},
    triple::Triple,
};

mod _nothing;
pub use _nothing::Nothing;
mod _and;
pub use _and::And;
mod _sparql;
pub use _sparql::Sparql;

mod _triple_source;
pub use _triple_source::{NormalizeError, NormalizeTriples};

/// A trait for dealing with recognized datatypes.
pub trait Recognized: Send + Sync {
    /// Normalize the typed literal.
    ///
    /// * The outer results indicates whether the datatype is recognized (`Ok`)
    ///   or not (`Err`, passing through the original lexical form).
    /// * The inner results indicates whether the lexical form is valid (`Ok`) or ill-typed (`Err`).
    ///
    /// Note that normalization may alter both the lexical value (turned into canonical form)
    /// and the datatype (in case several recognized datatypes have overlapping value spaces).
    fn try_normalize<T: Borrow<str>>(
        lex_dt: LexDt<T>,
    ) -> Result<Result<LexDt<Box<str>>, IllTypedLiteral>, LexDt<T>>;

    /// List all supported datatypes.
    fn datatypes() -> impl Iterator<Item = IriRef<MownStr<'static>>> + Send;

    /// List witnesses for all supported non-empty datatypes,
    /// as pairs (lexical_form, datatype)
    ///
    /// NB: one literal can be a witness for several supported datatypes,
    /// e.g. `xsd:decimal`, `xsd:integer`, `xsd:int` and others...
    ///
    /// Note however that all combinations of supported datatypes that share a value should have a witness.
    /// Imagine 3 datatypes :multipleOf2, :multipleOf3 and :prime,
    /// 2 (:multipleOf2 + :prime) and 3 (:multipleOf3 + :prime) are not sufficient,
    /// another witness should exist for :multipleOf2 and :multipleOf3 (e.g. 6).
    fn witnesses() -> impl Iterator<Item = (MownStr<'static>, IriRef<MownStr<'static>>)> + Send;

    /// List all additional datatypes for a given literal.
    ///
    /// Return
    /// * `None` if `datatype` is not recognized
    /// * `Some` empty vec if this literal has no other recognized datatype
    /// * `Some` non-empty vec if this literal is also a value of other recognized datatypes
    ///
    /// ## Precondition
    ///
    /// `lex` must be a canonical lexical form for the given datatype if it is recognized
    fn datatypes_for(
        lex: &str,
        datatype: IriRef<&str>,
    ) -> Option<SmallVec<[IriRef<MownStr<'static>>; 16]>>;

    /// Normalize the lexical form if the datatype is recognized, otherwise return it as is.
    fn normalize_or_fallback<T: Borrow<str> + From<Box<str>>>(
        lex_dt: LexDt<T>,
    ) -> Result<LexDt<T>, IllTypedLiteral> {
        Self::try_normalize(lex_dt)
            .map(|res| res.map(|(lex, dt)| (lex.into(), dt)))
            .unwrap_or_else(Ok)
    }

    /// Normalize the given [`SimpleTerm`]
    fn normalize_simple_term(t: SimpleTerm<'_>) -> Result<SimpleTerm<'_>, IllTypedLiteral> {
        Ok(match t {
            SimpleTerm::LiteralDatatype(lex, dt) => {
                let (lex, dt) = Self::normalize_or_fallback((lex, dt))?;
                SimpleTerm::LiteralDatatype(lex, dt)
            }
            SimpleTerm::Triple(tr) => {
                let [s, p, o] = tr.to_spo();
                let tr = [
                    Self::normalize_simple_term(s)?,
                    Self::normalize_simple_term(p)?,
                    Self::normalize_simple_term(o)?,
                ];
                SimpleTerm::Triple(Box::new(tr))
            }
            _ => t,
        })
    }

    /// Normalize all terms in a [`TripleSource`]
    fn normalize_triples<TS: TripleSource>(ts: TS) -> NormalizeTriples<Self, TS> {
        NormalizeTriples::new(ts)
    }
}

//

/// An error raised when an [ill-typed literal](https://www.w3.org/TR/rdf12-concepts/#dfn-ill-typed)
/// is encountered.
#[derive(Clone, Debug, thiserror::Error)]
#[error("Ill-typed literal {lex:?}^^<{datatype}>")]
pub struct IllTypedLiteral {
    lex: Box<str>,
    datatype: IriRef<Box<str>>,
}

impl IllTypedLiteral {
    /// Constructor
    pub fn new<T: Borrow<str>>(lex: T, datatype: IriRef<T>) -> Self {
        Self {
            lex: Box::from(lex.borrow()),
            datatype: datatype.as_ref().map_unchecked(Box::from),
        }
    }
}

//

/// A type alias for handling together the lexical value and datatype of a literal.
pub type LexDt<'a, T> = (T, IriRef<MownStr<'a>>);
