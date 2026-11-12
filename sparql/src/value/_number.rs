use bigdecimal::{BigDecimal, FromPrimitive, RoundingMode, Signed, ToPrimitive, Zero};
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
        Self::NativeInt(value)
    }
}

impl From<BigInt> for SparqlNumber {
    fn from(value: BigInt) -> Self {
        Self::BigInt(value)
    }
}

impl From<BigDecimal> for SparqlNumber {
    fn from(value: BigDecimal) -> Self {
        Self::Decimal(value)
    }
}

impl From<f32> for SparqlNumber {
    fn from(value: f32) -> Self {
        Self::Float(value)
    }
}

impl From<f64> for SparqlNumber {
    fn from(value: f64) -> Self {
        Self::Double(value)
    }
}

macro_rules! impl_sparqlnumber_integer_from {
    ($t: ty) => {
        impl From<$t> for SparqlNumber {
            fn from(value: $t) -> Self {
                #[allow(irrefutable_let_patterns)]
                // because depending on the arguments of the macro,
                // the pattern bellow *may* be irrefutable
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
    #[must_use]
    pub fn try_parse_integer(lex: &str) -> Option<Self> {
        if let Ok(val) = lex.parse::<isize>() {
            Some(val.into())
        } else if let Ok(val) = lex.parse::<BigInt>() {
            Some(val.into())
        } else {
            None
        }
    }

    #[must_use]
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

    #[must_use]
    pub fn is_zero(&self) -> bool {
        match self {
            Self::NativeInt(inner) => inner.is_zero(),
            Self::BigInt(inner) => inner.is_zero(),
            Self::Decimal(inner) => inner.is_zero(),
            Self::Float(inner) => inner.is_zero(),
            Self::Double(inner) => inner.is_zero(),
        }
    }

    #[must_use]
    pub fn is_positive(&self) -> bool {
        match self {
            Self::NativeInt(inner) => inner.is_positive(),
            Self::BigInt(inner) => inner.is_positive(),
            Self::Decimal(inner) => inner.is_positive(),
            Self::Float(inner) => inner.is_positive(),
            Self::Double(inner) => inner.is_positive(),
        }
    }

    #[must_use]
    pub fn is_negative(&self) -> bool {
        match self {
            Self::NativeInt(inner) => inner.is_negative(),
            Self::BigInt(inner) => inner.is_negative(),
            Self::Decimal(inner) => inner.is_negative(),
            Self::Float(inner) => inner.is_negative(),
            Self::Double(inner) => inner.is_negative(),
        }
    }

    #[must_use]
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::NativeInt(i) => !i.is_zero(),
            Self::BigInt(i) => !i.is_zero(),
            Self::Decimal(d) => !d.is_zero(),
            Self::Double(d) => !d.is_zero() && !d.is_nan(),
            Self::Float(f) => !f.is_zero() && !f.is_nan(),
        }
    }

    #[must_use]
    pub const fn is_nan(&self) -> bool {
        match self {
            Self::Double(d) => d.is_nan(),
            Self::Float(f) => f.is_nan(),
            _ => false,
        }
    }

    #[must_use]
    pub fn abs(&self) -> Self {
        match self {
            Self::NativeInt(inner) => inner.abs().into(),
            Self::BigInt(inner) => inner.clone().into(),
            Self::Decimal(inner) => inner.abs().into(),
            Self::Float(inner) => inner.abs().into(),
            Self::Double(inner) => inner.abs().into(),
        }
    }

    #[must_use]
    pub fn ceil(&self) -> Self {
        match self {
            Self::NativeInt(inner) => (*inner).into(),
            Self::BigInt(inner) => inner.clone().into(),
            Self::Decimal(inner) => {
                (inner.with_scale_round(0, RoundingMode::Ceiling)).into()
            }
            Self::Float(inner) => inner.ceil().into(),
            Self::Double(inner) => inner.ceil().into(),
        }
    }

    #[must_use]
    pub fn floor(&self) -> Self {
        match self {
            Self::NativeInt(inner) => (*inner).into(),
            Self::BigInt(inner) => inner.clone().into(),
            Self::Decimal(inner) => (inner.with_scale_round(0, RoundingMode::Floor)).into(),
            Self::Float(inner) => inner.floor().into(),
            Self::Double(inner) => inner.floor().into(),
        }
    }

    #[must_use]
    pub fn round(&self) -> Self {
        match self {
            Self::NativeInt(inner) => (*inner).into(),
            Self::BigInt(inner) => inner.clone().into(),
            Self::Decimal(inner) => {
                (inner.with_scale_round(0, RoundingMode::HalfUp)).into()
            }
            Self::Float(inner) => inner.round().into(),
            Self::Double(inner) => inner.round().into(),
        }
    }

    /// Coerce to a decimal
    ///
    /// ## Precondition
    /// Will panic if called on anything but a `NativeInt` or `BigInt`.
    /// NB: Decimal must not be coerced to decimal, as this would cause a clone.
    pub(crate) fn coerce_to_decimal(&self) -> BigDecimal {
        match self {
            Self::NativeInt(inner) => BigDecimal::from_isize(*inner).unwrap(),
            Self::BigInt(inner) => inner.clone().into(),
            _ => panic!(),
        }
    }

    /// Coerce to a f32
    ///
    /// Note that the conversion may lose precision, or even result in NaN for some big integers and decimals.
    #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
    #[must_use]
    pub fn coerce_to_float(&self) -> f32 {
        match self {
            Self::NativeInt(inner) => *inner as f32,
            Self::BigInt(inner) => inner.to_f32().unwrap_or(f32::NAN),
            Self::Decimal(inner) => inner.to_f32().unwrap_or(f32::NAN),
            Self::Float(inner) => *inner,
            Self::Double(inner) => *inner as f32,
        }
    }

    /// Coerce to a f64
    ///
    /// Note that the conversion may lose precision, or even result in NaN for some big integers and decimals.
    #[allow(clippy::cast_precision_loss)]
    #[must_use]
    pub fn coerce_to_double(&self) -> f64 {
        match self {
            Self::NativeInt(inner) => *inner as f64,
            Self::BigInt(inner) => inner.to_f64().unwrap_or(f64::NAN),
            Self::Decimal(inner) => inner.to_f64().unwrap_or(f64::NAN),
            Self::Float(inner) => f64::from(*inner),
            Self::Double(inner) => *inner,
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
