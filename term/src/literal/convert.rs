//! Public exported by parent-module `literal`.
//!

use super::*;
use crate::{term_to_string, Result, TermError};
use sophia_api::ns::xsd;
use std::error::Error;
use std::fmt;
use std::hash;
use std::str::FromStr;

/// A native datatype that maps to an RDF datatype.
pub trait DataType {
    /// Static IRI referencing the datatype of this Rust-type.
    fn iri() -> SimpleIri<'static>;

    /// Datatype of `self`.
    ///
    /// This can be used for `enum`s where each variant may has its own
    /// datatype.
    fn datatype(&self) -> SimpleIri<'static> {
        Self::iri()
    }
}

/// A native datatype that converts to an RDF literal.
pub trait AsLiteral: DataType {
    /// The term type to which this native type can convert.
    type Term: TTerm;
    /// Create an RDF literal, representing `self`.
    fn as_literal(&self) -> Self::Term;
}

/// A native datatype that can be constructed from an RDF literal.
pub trait TryConvertTerm: DataType + FromStr
where
    <Self as FromStr>::Err: Error + 'static,
{
    /// Try to convert any term to this native type.
    fn try_convert<T>(term: &T) -> Result<Self>
    where
        T: TTerm + ?Sized,
    {
        match term.datatype() {
            None => Err(TermError::UnsupportedKind(term_to_string(term))),
            Some(dt) => {
                if Self::iri() != dt {
                    Err(TermError::UnsupportedDatatype(term_to_string(term)))
                } else {
                    term.value_raw().0.parse::<Self>().map_err(|err| {
                        TermError::InvalidLexicalValue {
                            lex: term.value_raw().0.to_string(),
                            dt: term_to_string(&dt),
                            source: Box::new(err),
                        }
                    })
                }
            }
        }
    }
}

/// This trait is to [`TryConvertTerm`]
/// what `Into` is to `From`.
/// It is automatically implemented by any implementation of [`TTerm`].
///
/// [`TryConvertTerm`]: ./trait.TryConvertTerm.html
/// [`TTerm`]: ./trait.TTerm.html
pub trait ConvertibleTerm: TTerm {
    /// Try to convert this term into a native type
    fn try_converted<T>(&self) -> Result<T>
    where
        T: TryConvertTerm,
        <T as FromStr>::Err: Error + 'static;
}

impl<T> ConvertibleTerm for T
where
    T: TTerm + ?Sized,
{
    fn try_converted<U>(&self) -> Result<U>
    where
        U: TryConvertTerm,
        <U as FromStr>::Err: Error + 'static,
    {
        U::try_convert(self)
    }
}

/// An RDF literal representation of a native value.
#[derive(Clone, Debug)]
pub struct NativeLiteral<T, U = Box<str>>
where
    T: DataType + ?Sized,
    U: AsRef<str>,
{
    /// Lexical value
    pub(crate) lexval: U,
    /// Phantom data to link to the original datatype
    _phantom: std::marker::PhantomData<T>,
}

impl<T, U> NativeLiteral<T, U>
where
    T: DataType + ?Sized,
    U: AsRef<str>,
{
    /// Construct a SimpleLiterla.
    ///
    /// # Pre-conditions
    /// This constructor does not check that `lexval`
    /// is a valid lexical value for this literal's datatype.
    /// It is the user's responsibility to ensure that.
    fn new(lexval: U) -> Self {
        Self {
            lexval,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, U> TTerm for NativeLiteral<T, U>
where
    T: DataType + ?Sized,
    U: AsRef<str>,
{
    fn kind(&self) -> TermKind {
        TermKind::Literal
    }

    fn value_raw(&self) -> (&str, Option<&str>) {
        (&self.lexval.as_ref(), None)
    }

    fn datatype(&self) -> Option<SimpleIri> {
        Some(T::iri())
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<T, U> fmt::Display for NativeLiteral<T, U>
where
    T: DataType + ?Sized,
    U: AsRef<str>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        term_format(self, f)
    }
}

impl<T, U, V> PartialEq<V> for NativeLiteral<T, U>
where
    T: DataType + ?Sized,
    U: AsRef<str>,
    V: TTerm + ?Sized,
{
    fn eq(&self, other: &V) -> bool {
        term_eq(self, other)
    }
}

impl<T, U> hash::Hash for NativeLiteral<T, U>
where
    T: DataType + ?Sized,
    U: AsRef<str>,
{
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}

macro_rules! impl_datatype {
    ($ty:ty, $iri:expr) => {
        impl $crate::literal::convert::DataType for $ty {
            fn iri() -> sophia_api::term::SimpleIri<'static> {
                $iri
            }
        }
    };
}

impl_datatype!(u8, xsd::unsignedByte);
impl_datatype!(u16, xsd::unsignedShort);
impl_datatype!(u32, xsd::unsignedInt);
impl_datatype!(u64, xsd::unsignedLong);
impl_datatype!(i8, xsd::byte);
impl_datatype!(i16, xsd::short);
impl_datatype!(i32, xsd::int);
impl_datatype!(i64, xsd::long);
impl_datatype!(f32, xsd::float);
impl_datatype!(f64, xsd::double);
impl_datatype!(bool, xsd::boolean);
impl_datatype!(String, xsd::string);
impl_datatype!(str, xsd::string);
impl_datatype!(&str, xsd::string);

macro_rules! impl_as_literal {
    ($ty:ty) => {
        impl $crate::literal::convert::AsLiteral for $ty
        where
            Self: std::string::ToString,
        {
            type Term = $crate::literal::convert::NativeLiteral<Self>;
            fn as_literal(&self) -> Self::Term {
                $crate::literal::convert::NativeLiteral::new(self.to_string().into_boxed_str())
            }
        }
    };
}

impl_as_literal!(u8);
impl_as_literal!(u16);
impl_as_literal!(u32);
impl_as_literal!(u64);
impl_as_literal!(i8);
impl_as_literal!(i16);
impl_as_literal!(i32);
impl_as_literal!(i64);
impl_as_literal!(f32);
impl_as_literal!(f64);
impl_as_literal!(bool);
impl_as_literal!(String);

impl<'a> AsLiteral for &'a str {
    type Term = NativeLiteral<&'a str, &'a str>;
    fn as_literal(&self) -> Self::Term {
        NativeLiteral::new(*self)
    }
}

impl TryConvertTerm for u8 {}
impl TryConvertTerm for u16 {}
impl TryConvertTerm for u32 {}
impl TryConvertTerm for u64 {}
impl TryConvertTerm for i8 {}
impl TryConvertTerm for i16 {}
impl TryConvertTerm for i32 {}
impl TryConvertTerm for i64 {}
impl TryConvertTerm for f32 {}
impl TryConvertTerm for f64 {}
impl TryConvertTerm for bool {}
impl TryConvertTerm for String {}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum Failed {
        InvLexVal,
        NotALit,
        WrongDt,
        Unknown,
    }

    impl From<TermError> for Failed {
        fn from(other: TermError) -> Self {
            match other {
                TermError::InvalidLexicalValue { .. } => Failed::InvLexVal,
                TermError::UnsupportedKind(_) => Failed::NotALit,
                TermError::UnsupportedDatatype(_) => Failed::WrongDt,
                _ => Failed::Unknown,
            }
        }
    }

    #[test]
    fn borrow_str() {
        let _: NativeLiteral<&str, &str> = "test".as_literal();
        let string = "test2".to_string();
        let l1 = string.as_str().as_literal();
        let l2 = string.as_literal();
        assert_eq!(l1, l2);
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Ok(-10)                ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Err(Failed::InvLexVal) ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_i8(lex: &str) -> Result<i8, Failed> {
        let dt = Iri::<&str>::from(i8::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: i8 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Ok(-10)                ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Err(Failed::InvLexVal) ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_i16(lex: &str) -> Result<i16, Failed> {
        let dt = Iri::<&str>::from(i16::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: i16 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Ok(-10)                ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Ok(1000000)            ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_i32(lex: &str) -> Result<i32, Failed> {
        let dt = Iri::<&str>::from(i32::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: i32 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Ok(-10)                ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Ok(1000000)            ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_i64(lex: &str) -> Result<i64, Failed> {
        let dt = Iri::<&str>::from(i64::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: i64 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Err(Failed::InvLexVal) ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Err(Failed::InvLexVal) ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_u8(lex: &str) -> Result<u8, Failed> {
        let dt = Iri::<&str>::from(u8::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: u8 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Err(Failed::InvLexVal) ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Err(Failed::InvLexVal) ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_u16(lex: &str) -> Result<u16, Failed> {
        let dt = Iri::<&str>::from(u16::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: u16 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Err(Failed::InvLexVal) ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Ok(1000000)            ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_u32(lex: &str) -> Result<u32, Failed> {
        let dt = Iri::<&str>::from(u32::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: u32 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0)                  ; "zero")]
    #[test_case("-10"       => Err(Failed::InvLexVal) ; "minus ten")]
    #[test_case("10"        => Ok(10)                 ; "ten")]
    #[test_case("1000000"   => Ok(1000000)            ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_u64(lex: &str) -> Result<u64, Failed> {
        let dt = Iri::<&str>::from(u64::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: u64 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0.0)                ; "zero")]
    #[test_case("-10"       => Ok(-10.0)              ; "minus ten")]
    #[test_case("10"        => Ok(10.0)               ; "ten")]
    #[test_case("1000000"   => Ok(1000000.0)          ; "million")]
    #[test_case("314e-2"    => Ok(3.14)               ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_f32(lex: &str) -> Result<f32, Failed> {
        let dt = Iri::<&str>::from(f32::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: f32 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok(0.0)                ; "zero")]
    #[test_case("-10"       => Ok(-10.0)              ; "minus ten")]
    #[test_case("10"        => Ok(10.0)               ; "ten")]
    #[test_case("1000000"   => Ok(1000000.0)          ; "million")]
    #[test_case("314e-2"    => Ok(3.14)               ; "float")]
    #[test_case("true"      => Err(Failed::InvLexVal) ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_f64(lex: &str) -> Result<f64, Failed> {
        let dt = Iri::<&str>::from(f64::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: f64 = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Err(Failed::InvLexVal) ; "zero")]
    #[test_case("-10"       => Err(Failed::InvLexVal) ; "minus ten")]
    #[test_case("10"        => Err(Failed::InvLexVal) ; "ten")]
    #[test_case("1000000"   => Err(Failed::InvLexVal) ; "million")]
    #[test_case("314e-2"    => Err(Failed::InvLexVal) ; "float")]
    #[test_case("true"      => Ok(true)               ; "bool")]
    #[test_case("something" => Err(Failed::InvLexVal) ; "string")]
    fn convert_bool(lex: &str) -> Result<bool, Failed> {
        let dt = Iri::<&str>::from(bool::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: bool = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }

    #[test_case("0"         => Ok("0"        .to_string()) ; "zero")]
    #[test_case("-10"       => Ok("-10"      .to_string()) ; "minus ten")]
    #[test_case("10"        => Ok("10"       .to_string()) ; "ten")]
    #[test_case("1000000"   => Ok("1000000"  .to_string()) ; "million")]
    #[test_case("314e-2"    => Ok("314e-2"   .to_string()) ; "float")]
    #[test_case("true"      => Ok("true"     .to_string()) ; "bool")]
    #[test_case("something" => Ok("something".to_string()) ; "string")]
    fn convert_string(lex: &str) -> Result<String, Failed> {
        let dt = Iri::<&str>::from(String::iri());
        let lit = Literal::<&str>::new_dt(lex, dt);
        let native: String = lit.try_converted()?;
        let lit2 = native.as_literal();
        assert_eq!(lit.datatype(), lit2.datatype());
        Ok(native)
    }
}
