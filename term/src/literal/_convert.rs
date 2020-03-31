//! Public exported by parent-module `literal`.
//!

use super::*;
use crate::ns::xsd;
use crate::{Iri, Result, StaticTerm, TermData, TermError};
//use crate::triple::stream::{SinkError, SourceError, StreamResult};

/// Provide an IRI to specify the semantic datatype.
pub trait DataType {
    /// Static IRI referencing the datatype of this Rust-type.
    fn iri() -> &'static Iri<&'static str>;

    /// Datatype of `self`.
    ///
    /// This can be used for `enum`s where each variant may has its own
    /// datatype.
    fn datatype(&self) -> &'static Iri<&'static str> {
        Self::iri()
    }

    /// Same as `iri()` but returns a `Term`
    fn iri_as_term() -> &'static StaticTerm;

    /// Datatype of `self`.
    ///
    /// This can be used for `enum`s where each variant may has its own
    /// datatype.
    fn datatype_as_term(&self) -> &'static StaticTerm {
        Self::iri_as_term()
    }
}

/// Representation as an RDF literal.
pub trait AsLiteral<TD: TermData>: DataType {
    /// Create an RDF literal, representing `self`.
    fn as_literal(&self) -> Literal<TD>;

    /// Blanked implementation to directly get a term.
    fn as_term(&self) -> Term<TD> {
        self.as_literal().into()
    }
}

/// Create `self` from an RDF literal.
pub trait FromLiteral: Sized {
    /// Error raised if literal text is malformed.
    type Error: std::error::Error + 'static;

    /// Direct from a literal.
    fn from_literal<TD: TermData>(lit: &Literal<TD>) -> Result<Self, Self::Error>;

    /// Get a term that is actually a literal.
    ///
    /// # Error
    ///
    /// The default implementation throws `SourceError(UnexpectedKindOfTerm)`
    /// if `t` is not a literal.
    fn from_term<'a, TD>(t: &'a Term<TD>) -> Result<Self, TermError>
    where
        TD: 'a + TermData,
    {
        if let Term::Literal(lit) = t {
            Self::from_literal(lit).map_err(|err| TermError::InvalidLexicalValue {
                lex: lit.value().to_string(),
                dt: lit.dt().value().to_string(),
                source: Box::new(err),
            })
        } else {
            Err(TermError::UnexpectedKindOfTerm {
                term: t.to_string(),
                expect: "literal".to_string(),
            })
        }
    }
}

macro_rules! impl_dt_with_term {
    ($ty:ty, $iri:expr, $term:expr) => {
        impl $crate::literal::DataType for $ty {
            fn iri() -> &'static $crate::iri::Iri<&'static str> {
                &$iri
            }

            fn iri_as_term() -> &'static crate::Term<&'static str> {
                &$term
            }
        }
    };
}

impl_dt_with_term!(u8, xsd::iri::unsignedByte, xsd::unsignedByte);
impl_dt_with_term!(u16, xsd::iri::unsignedShort, xsd::unsignedShort);
impl_dt_with_term!(u32, xsd::iri::unsignedInt, xsd::unsignedInt);
impl_dt_with_term!(u64, xsd::iri::unsignedLong, xsd::unsignedLong);
impl_dt_with_term!(i8, xsd::iri::byte, xsd::byte);
impl_dt_with_term!(i16, xsd::iri::short, xsd::short);
impl_dt_with_term!(i32, xsd::iri::int, xsd::int);
impl_dt_with_term!(i64, xsd::iri::long, xsd::long);
impl_dt_with_term!(f32, xsd::iri::float, xsd::float);
impl_dt_with_term!(f64, xsd::iri::double, xsd::double);
impl_dt_with_term!(bool, xsd::iri::boolean, xsd::boolean);
impl_dt_with_term!(String, xsd::iri::string, xsd::string);
impl_dt_with_term!(str, xsd::iri::string, xsd::string);
impl_dt_with_term!(&str, xsd::iri::string, xsd::string);

macro_rules! impl_as_literal {
    ($ty:ty) => {
        impl<TD> $crate::literal::AsLiteral<TD> for $ty
        where
            Self: std::string::ToString,
            TD: $crate::TermData + From<String> + From<&'static str>,
        {
            fn as_literal(&self) -> Literal<TD> {
                $crate::literal::Literal::new_dt(self.to_string(), Self::iri().clone_into())
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

impl<'a, TD> AsLiteral<TD> for &'a str
where
    TD: TermData + From<&'a str>,
    Iri<TD>: From<Iri<&'a str>>,
{
    fn as_literal(&self) -> Literal<TD> {
        Literal::new_dt(*self, xsd::string.try_into().unwrap())
    }
}

macro_rules! impl_from_literal {
    ($ty:ty) => {
        impl $crate::literal::FromLiteral for $ty {
            type Error = <$ty as std::str::FromStr>::Err;

            fn from_literal<TD: TermData>(lit: &Literal<TD>) -> Result<Self, Self::Error> {
                lit.txt().as_ref().parse::<Self>()
            }
        }
    };
}

impl_from_literal!(u8);
impl_from_literal!(u16);
impl_from_literal!(u32);
impl_from_literal!(u64);
impl_from_literal!(i8);
impl_from_literal!(i16);
impl_from_literal!(i32);
impl_from_literal!(i64);
impl_from_literal!(f32);
impl_from_literal!(f64);
impl_from_literal!(bool);
impl_from_literal!(String);

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum Failed {
        Convert,
        WrongDt,
        TermNeqLiteral,
    }

    #[test]
    fn borrow_str() {
        let _: Literal<&'static str> = "test".as_literal();
        let string = "test2".to_string();
        let l1: Literal<&str> = string.as_str().as_literal();
        let l2: Literal<std::sync::Arc<str>> = string.as_literal();
        assert_eq!(l1, l2);
    }

    #[test_case("0" => Ok(0) ; "zero")]
    #[test_case("-10" => Ok(-10) ; "minus ten")]
    #[test_case("10" => Ok(10) ; "ten")]
    #[test_case("1000000" => Err(Failed::Convert) ; "million")]
    #[test_case("314e-2" => Err(Failed::Convert) ; "float")]
    #[test_case("true" => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_i8(i: &str) -> Result<i8, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = i8::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::byte != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0" => Ok(0) ; "zero")]
    #[test_case("-10" => Ok(-10) ; "minus ten")]
    #[test_case("10" => Ok(10) ; "ten")]
    #[test_case("1000000" => Err(Failed::Convert) ; "million")]
    #[test_case("314e-2" => Err(Failed::Convert) ; "float")]
    #[test_case("true" => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_i16(i: &str) -> Result<i16, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = i16::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::short != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0" => Ok(0) ; "zero")]
    #[test_case("-10" => Ok(-10) ; "minus ten")]
    #[test_case("10" => Ok(10) ; "ten")]
    #[test_case("1000000" => Ok(1_000_000) ; "million")]
    #[test_case("314e-2" => Err(Failed::Convert) ; "float")]
    #[test_case("true" => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_i32(i: &str) -> Result<i32, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = i32::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::int != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0" => Ok(0) ; "zero")]
    #[test_case("-10" => Ok(-10) ; "minus ten")]
    #[test_case("10" => Ok(10) ; "ten")]
    #[test_case("1000000" => Ok(1_000_000) ; "million")]
    #[test_case("314e-2" => Err(Failed::Convert) ; "float")]
    #[test_case("true" => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_i64(i: &str) -> Result<i64, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = i64::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::long != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0" => Ok(0) ; "zero")]
    #[test_case("-10" => Err(Failed::Convert) ; "minus ten")]
    #[test_case("10" => Ok(10) ; "ten")]
    #[test_case("1000000" => Err(Failed::Convert) ; "million")]
    #[test_case("314e-2" => Err(Failed::Convert) ; "float")]
    #[test_case("true" => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_u8(i: &str) -> Result<u8, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = u8::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::unsignedByte != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0" => Ok(0) ; "zero")]
    #[test_case("-10" => Err(Failed::Convert) ; "minus ten")]
    #[test_case("10" => Ok(10) ; "ten")]
    #[test_case("1000000" => Err(Failed::Convert) ; "million")]
    #[test_case("314e-2" => Err(Failed::Convert) ; "float")]
    #[test_case("true" => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_u16(i: &str) -> Result<u16, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = u16::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::unsignedShort != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0"         => Ok(0)                ; "zero")]
    #[test_case("-10"       => Err(Failed::Convert) ; "minus ten")]
    #[test_case("10"        => Ok(10)               ; "ten")]
    #[test_case("1000000"   => Ok(1_000_000)        ; "million")]
    #[test_case("314e-2"    => Err(Failed::Convert) ; "float")]
    #[test_case("true"      => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_u32(i: &str) -> Result<u32, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = u32::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::unsignedInt != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0"         => Ok(0)                ; "zero")]
    #[test_case("-10"       => Err(Failed::Convert) ; "minus ten")]
    #[test_case("10"        => Ok(10)               ; "ten")]
    #[test_case("1000000"   => Ok(1_000_000)        ; "million")]
    #[test_case("314e-2"    => Err(Failed::Convert) ; "float")]
    #[test_case("true"      => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_u64(i: &str) -> Result<u64, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = u64::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::unsignedLong != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0"         => Ok(0.0)              ; "zero")]
    #[test_case("-10"       => Ok(-10.0)            ; "minus ten")]
    #[test_case("10"        => Ok(10.0)             ; "ten")]
    #[test_case("1000000"   => Ok(1_000_000.0)      ; "million")]
    #[test_case("314e-2"    => Ok(3.14)             ; "float")]
    #[test_case("true"      => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_f32(i: &str) -> Result<f32, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = f32::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::float != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0"         => Ok(0.0)              ; "zero")]
    #[test_case("-10"       => Ok(-10.0)            ; "minus ten")]
    #[test_case("10"        => Ok(10.0)             ; "ten")]
    #[test_case("1000000"   => Ok(1_000_000.0)      ; "million")]
    #[test_case("314e-2"    => Ok(3.14)             ; "float")]
    #[test_case("true"      => Err(Failed::Convert) ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_f64(i: &str) -> Result<f64, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = f64::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::double != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0"         => Err(Failed::Convert) ; "zero")]
    #[test_case("-10"       => Err(Failed::Convert) ; "minus ten")]
    #[test_case("10"        => Err(Failed::Convert) ; "ten")]
    #[test_case("1000000"   => Err(Failed::Convert) ; "million")]
    #[test_case("314e-2"    => Err(Failed::Convert) ; "float")]
    #[test_case("true"      => Ok(true)             ; "bool")]
    #[test_case("something" => Err(Failed::Convert) ; "string")]
    fn convert_bool(i: &str) -> Result<bool, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = bool::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::boolean != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }

    #[test_case("0"         => Ok("0"        .to_string()) ; "zero")]
    #[test_case("-10"       => Ok("-10"      .to_string()) ; "minus ten")]
    #[test_case("10"        => Ok("10"       .to_string()) ; "ten")]
    #[test_case("1000000"   => Ok("1000000"  .to_string()) ; "million")]
    #[test_case("314e-2"    => Ok("314e-2"   .to_string()) ; "float")]
    #[test_case("true"      => Ok("true"     .to_string()) ; "bool")]
    #[test_case("something" => Ok("something".to_string()) ; "string")]
    fn convert_string(i: &str) -> Result<String, Failed> {
        let i: Literal<&str> = i.as_literal();
        let res = String::from_literal(&i).map_err(|e| {
            println!("Fail with error: {}", e);
            Failed::Convert
        })?;

        let lit: Literal<String> = res.as_literal();
        let t: Term<String> = res.as_term();
        if t != lit {
            return Err(Failed::TermNeqLiteral);
        }
        if xsd::string != lit.dt() {
            return Err(Failed::WrongDt);
        }

        Ok(res)
    }
}
