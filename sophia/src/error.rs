//! Types for handling errors.
use crate::term::TermError;
use std::convert::Infallible;

error_chain! {
    errors {
        /// Raised by serializers when they encounter a problem.
        SerializerError(message: String) {
            display("error while serializing: {}", message)
        }
        /// Raised by some mutable dataset
        TermError(te: TermError) {
            display("From term: {}", te)
        }
    }
}

/// Only implemented to satisfy the type system.
impl From<Infallible> for Error {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
