//! This module provides examples of what the [`wrap`] macro generates.
//!
//! [`SimpleWrapper`] is generated with the following code:
//! ```
//! # #[macro_use] extern crate sophia_iri;
//! wrap! { SimpleWrapper<T: PartialEq<isize>> :
//!    /// SimpleWrapper wraps any type comparable to `isize`,
//!    /// and guarantees that the wrapped value is not equal to 0.
//!    #[allow(dead_code)]
//!    pub fn new(inner: T) -> Result<Self, String> {
//!        if inner != 0 {
//!            Ok(Self(inner))
//!        } else {
//!            Err("inner is equal to zero".into())
//!        }
//!    }
//!}
//! ```
//!
//! [`BorrowingWrapper`] is generated with the following code:
//! ```
//! # #[macro_use] extern crate sophia_iri;
//! wrap! { BorrowingWrapper borrowing str :
//!     /// BorrowingWrapper wraps any `Borrow<str>` type,
//!     /// and guarantees that the wrapped value contains `"hello"`.
//!     #[allow(dead_code)]
//!     pub fn new(inner: T) -> Result<Self, String> {
//!         if inner.borrow().contains("hello") {
//!             Ok(Self(inner))
//!         } else {
//!             Err("inner does not contain 'hello'".into())
//!         }
//!     }
//! }
//! ```
//! As can be seen, this second form generates more methods and trait implementations than the previous one.
#![allow(dead_code)]

use crate::wrap;

wrap! { SimpleWrapper<T: PartialEq<isize>> :
    /// SimpleWrapper wraps any type comparable to `isize`,
    /// and guarantees that the wrapped value is not equal to 0.
    pub fn new(inner: T) -> Result<Self, String> {
        if inner != 0 {
            Ok(Self(inner))
        } else {
            Err("inner is equal to zero".into())
        }
    }
}

wrap! { BorrowingWrapper borrowing str :
    /// BorrowingWrapper wraps any `Borrow<str>` type,
    /// and guarantees that the wrapped value contains `"hello"`.
    pub fn new(inner: T) -> Result<Self, String> {
        if inner.borrow().contains("hello") {
            Ok(Self(inner))
        } else {
            Err("inner does not contain 'hello'".into())
        }
    }
}
