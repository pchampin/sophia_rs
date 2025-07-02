use std::sync::LazyLock;

use bigdecimal::{BigDecimal, FromPrimitive, One, Signed, ToPrimitive, Zero};
use num_bigint::BigInt;

#[derive(Clone, Debug)]
pub enum SparqlNumber {
    NativeInt(isize),
    BigInt(BigInt),
    Decimal(BigDecimal),
    Float(f32),
    Double(f64),
}

impl From<isize> for SparqlNumber {
    fn from(value: isize) -> Self {
        SparqlNumber::NativeInt(value)
    }
}

impl From<BigInt> for SparqlNumber {
    fn from(value: BigInt) -> Self {
        SparqlNumber::BigInt(value)
    }
}

impl From<BigDecimal> for SparqlNumber {
    fn from(value: BigDecimal) -> Self {
        SparqlNumber::Decimal(value)
    }
}

impl From<f32> for SparqlNumber {
    fn from(value: f32) -> Self {
        SparqlNumber::Float(value)
    }
}

impl From<f64> for SparqlNumber {
    fn from(value: f64) -> Self {
        SparqlNumber::Double(value)
    }
}

macro_rules! impl_sparqlnumber_integer_from {
    ($t: ty) => {
        impl From<$t> for SparqlNumber {
            fn from(value: $t) -> Self {
                #[allow(irrefutable_let_patterns)]
                if let Ok(val) = value.try_into() {
                    SparqlNumber::NativeInt(val)
                } else {
                    SparqlNumber::BigInt(value.into())
                }
            }
        }
    };
}
impl_sparqlnumber_integer_from!(i64);
impl_sparqlnumber_integer_from!(i32);
impl_sparqlnumber_integer_from!(i16);
impl_sparqlnumber_integer_from!(i8);
impl_sparqlnumber_integer_from!(u64);
impl_sparqlnumber_integer_from!(u32);
impl_sparqlnumber_integer_from!(u16);
impl_sparqlnumber_integer_from!(u8);

impl SparqlNumber {
    pub fn try_parse_integer(lex: &str) -> Option<Self> {
        if let Ok(val) = lex.parse::<isize>() {
            Some(val.into())
        } else if let Ok(val) = lex.parse::<BigInt>() {
            Some(val.into())
        } else {
            None
        }
    }

    pub fn try_parse<T: std::str::FromStr + Into<Self>>(lex: &str) -> Option<Self> {
        if let Ok(val) = lex.parse::<T>() {
            Some(val.into())
        } else {
            None
        }
    }

    pub fn check<F: FnOnce(&Self) -> bool>(self, predicate: F) -> Option<Self> {
        if predicate(&self) { Some(self) } else { None }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(inner) => inner.is_zero(),
            SparqlNumber::BigInt(inner) => inner.is_zero(),
            SparqlNumber::Decimal(inner) => inner.is_zero(),
            SparqlNumber::Float(inner) => inner.is_zero(),
            SparqlNumber::Double(inner) => inner.is_zero(),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(inner) => inner.is_positive(),
            SparqlNumber::BigInt(inner) => inner.is_positive(),
            SparqlNumber::Decimal(inner) => inner.is_positive(),
            SparqlNumber::Float(inner) => inner.is_positive(),
            SparqlNumber::Double(inner) => inner.is_positive(),
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(inner) => inner.is_negative(),
            SparqlNumber::BigInt(inner) => inner.is_negative(),
            SparqlNumber::Decimal(inner) => inner.is_negative(),
            SparqlNumber::Float(inner) => inner.is_negative(),
            SparqlNumber::Double(inner) => inner.is_negative(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(i) => !i.is_zero(),
            SparqlNumber::BigInt(i) => !i.is_zero(),
            SparqlNumber::Decimal(d) => !d.is_zero(),
            SparqlNumber::Double(d) => !d.is_zero() && !d.is_nan(),
            SparqlNumber::Float(f) => !f.is_zero() && !f.is_nan(),
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            SparqlNumber::NativeInt(inner) => inner.abs().into(),
            SparqlNumber::BigInt(inner) => inner.clone().into(),
            SparqlNumber::Decimal(inner) => inner.abs().into(),
            SparqlNumber::Float(inner) => inner.abs().into(),
            SparqlNumber::Double(inner) => inner.abs().into(),
        }
    }

    pub fn ceil(&self) -> Self {
        match self {
            SparqlNumber::NativeInt(inner) => (*inner).into(),
            SparqlNumber::BigInt(inner) => inner.clone().into(),
            SparqlNumber::Decimal(inner) => (inner.to_ref() + DEC_0_5.to_ref()).round(0).into(),
            SparqlNumber::Float(inner) => inner.ceil().into(),
            SparqlNumber::Double(inner) => inner.ceil().into(),
        }
    }

    pub fn floor(&self) -> Self {
        match self {
            SparqlNumber::NativeInt(inner) => (*inner).into(),
            SparqlNumber::BigInt(inner) => inner.clone().into(),
            SparqlNumber::Decimal(inner) => (inner.to_ref() - DEC_0_5.to_ref()).round(0).into(),
            SparqlNumber::Float(inner) => inner.floor().into(),
            SparqlNumber::Double(inner) => inner.floor().into(),
        }
    }

    pub fn round(&self) -> Self {
        match self {
            SparqlNumber::NativeInt(inner) => (*inner).into(),
            SparqlNumber::BigInt(inner) => inner.clone().into(),
            SparqlNumber::Decimal(inner) => inner.round(0).into(),
            SparqlNumber::Float(inner) => inner.round().into(),
            SparqlNumber::Double(inner) => inner.round().into(),
        }
    }

    /// Coerce to a decimal
    ///
    /// ## Precondition
    /// Will panic if called on anything but a `NativeInt` or `BigInt`.
    /// NB: Decimal must not be coerced to decimal, as this would cause a clone.
    pub(crate) fn coerce_to_decimal(&self) -> BigDecimal {
        match self {
            SparqlNumber::NativeInt(inner) => BigDecimal::from_isize(*inner).unwrap(),
            SparqlNumber::BigInt(inner) => inner.clone().into(),
            _ => panic!(),
        }
    }

    /// Coerce to a f32
    ///
    /// Note that the conversion may lose precision, or even result in NaN for some big integers and decimals.
    #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
    pub fn coerce_to_float(&self) -> f32 {
        match self {
            SparqlNumber::NativeInt(inner) => *inner as f32,
            SparqlNumber::BigInt(inner) => inner.to_f32().unwrap_or(f32::NAN),
            SparqlNumber::Decimal(inner) => inner.to_f32().unwrap_or(f32::NAN),
            SparqlNumber::Float(inner) => *inner,
            SparqlNumber::Double(inner) => *inner as f32,
        }
    }

    /// Coerce to a f64
    ///
    /// Note that the conversion may lose precision, or even result in NaN for some big integers and decimals.
    #[allow(clippy::cast_precision_loss)]
    pub fn coerce_to_double(&self) -> f64 {
        match self {
            SparqlNumber::NativeInt(inner) => *inner as f64,
            SparqlNumber::BigInt(inner) => inner.to_f64().unwrap_or(f64::NAN),
            SparqlNumber::Decimal(inner) => inner.to_f64().unwrap_or(f64::NAN),
            SparqlNumber::Float(inner) => f64::from(*inner),
            SparqlNumber::Double(inner) => *inner,
        }
    }

    fn coercing_operator<F1, F2, F3, F4, F5, OI, O>(
        &self,
        rhs: &Self,
        fint: F1,
        fbig: F2,
        fdec: F3,
        fflt: F4,
        fdbl: F5,
    ) -> Option<O>
    where
        F1: FnOnce(isize, isize) -> Option<OI>,
        F2: FnOnce(&BigInt, &BigInt) -> Option<O>,
        F3: FnOnce(&BigDecimal, &BigDecimal) -> Option<O>,
        F4: FnOnce(f32, f32) -> Option<O>,
        F5: FnOnce(f64, f64) -> Option<O>,
        O: From<OI>,
    {
        use SparqlNumber::*;
        match (self, rhs) {
            (Double(lhs), rhs) => fdbl(*lhs, rhs.coerce_to_double()),
            (lhs, Double(rhs)) => fdbl(lhs.coerce_to_double(), *rhs),
            //
            (Float(lhs), rhs) => fflt(*lhs, rhs.coerce_to_float()),
            (lhs, Float(rhs)) => fflt(lhs.coerce_to_float(), *rhs),
            //
            (Decimal(lhs), Decimal(rhs)) => fdec(lhs, rhs),
            (Decimal(lhs), rhs) => fdec(lhs, &rhs.coerce_to_decimal()),
            (lhs, Decimal(rhs)) => fdec(&lhs.coerce_to_decimal(), rhs),
            //
            (BigInt(lhs), BigInt(rhs)) => fbig(lhs, rhs),
            (NativeInt(lhs), BigInt(rhs)) => fbig(&(*lhs).into(), rhs),
            //
            (BigInt(lhs), NativeInt(rhs)) => fbig(lhs, &(*rhs).into()),
            (NativeInt(lhs), NativeInt(rhs)) => fint(*lhs, *rhs)
                .map(O::from)
                .or_else(|| fbig(&(*lhs).into(), &(*rhs).into())),
        }
    }
}

impl std::ops::Add for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn add(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            isize::checked_add,
            |x, y| Some((x + y).into()),
            |x, y| Some((x + y).into()),
            |x, y| Some((x + y).into()),
            |x, y| Some((x + y).into()),
        )
    }
}

impl std::ops::Sub for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn sub(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            isize::checked_sub,
            |x, y| Some((x - y).into()),
            |x, y| Some((x - y).into()),
            |x, y| Some((x - y).into()),
            |x, y| Some((x - y).into()),
        )
    }
}

impl std::ops::Mul for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn mul(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            isize::checked_mul,
            |x, y| Some((x * y).into()),
            |x, y| Some((x * y).into()),
            |x, y| Some((x * y).into()),
            |x, y| Some((x * y).into()),
        )
    }
}

impl std::ops::Div for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn div(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            |_, _| None as Option<i32>,
            |x, y| {
                (!y.is_zero())
                    .then(|| (BigDecimal::from(x.clone()) / BigDecimal::from(y.clone())).into())
            }, // TODO this can probably be achieved with less clones
            |x, y| (!y.is_zero()).then(|| (x / y).into()),
            |x, y| Some((x / y).into()),
            |x, y| Some((x / y).into()),
        )
    }
}

impl std::ops::Neg for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn neg(self) -> Self::Output {
        match self {
            SparqlNumber::NativeInt(inner) => Some((-inner).into()),
            SparqlNumber::BigInt(inner) => Some((-inner).into()),
            SparqlNumber::Decimal(inner) => Some((-inner).into()),
            SparqlNumber::Float(inner) => Some((-inner).into()),
            SparqlNumber::Double(inner) => Some((-inner).into()),
        }
    }
}

impl std::cmp::PartialEq for &'_ SparqlNumber {
    fn eq(&self, other: &Self) -> bool {
        self.coercing_operator(
            other,
            |x, y| Some(x == y),
            |x, y| Some(x == y),
            |x, y| Some(x == y),
            |x, y| Some(x == y),
            |x, y| Some(x == y),
        )
        .unwrap_or(false)
    }
}

impl std::cmp::PartialOrd for &'_ SparqlNumber {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.coercing_operator(
            other,
            |x, y| x.partial_cmp(&y),
            std::cmp::PartialOrd::partial_cmp,
            std::cmp::PartialOrd::partial_cmp,
            |x, y| x.partial_cmp(&y),
            |x, y| x.partial_cmp(&y),
        )
    }
}

static DEC_0_5: LazyLock<BigDecimal> = LazyLock::new(|| BigDecimal::one() / 2);
