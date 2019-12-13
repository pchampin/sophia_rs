//! Types for handling errors.

// use thiserror::Error;
//
// /// This enum is used to identify an infallible operation where the API
// /// requires to return a `Result`
// /// 
// /// Is equal to `std::convert::Infallible`. However, error-handling requires
// /// `Infallible: From<Infallible>` which is not provided but implemented here.
// #[derive(Debug, Error)]
// pub enum Infallible {}

// impl From<std::convert::Infallible> for Infallible {
//     fn from(_: std::convert::Infallible) -> Self {
//         unreachable!()
//     }
// }

pub type Infallible = std::convert::Infallible;


/// Required for convertability to `anyhow::Error`.
pub trait SafeError: 'static + std::error::Error + Send + Sync {}

impl<E> SafeError for E
where
    E: 'static + std::error::Error + Send + Sync
{}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
