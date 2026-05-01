//! I reuse [`LanguageTag`] from [`sophia_bcp47`],
//! and provide [`Term`] implementation for [`I18nString`] of that same crate.

pub use sophia_bcp47::{I18nString, InvalidLanguageTag, LanguageTag};

use std::borrow::Borrow;

use crate::term::Term;

use super::TermKind;

impl<T: Borrow<str> + std::fmt::Debug> Term for I18nString<T> {
    type BorrowTerm<'x>
        = &'x Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn lexical_form(&self) -> Option<mownstr::MownStr<'_>> {
        Some(self.value.borrow().into())
    }

    fn datatype(&self) -> Option<super::IriRef<mownstr::MownStr<'_>>> {
        crate::ns::rdf::langString.iri()
    }

    fn language_tag(&self) -> Option<LanguageTag<mownstr::MownStr<'_>>> {
        Some(self.language.as_ref().map_unchecked(Into::into))
    }

    fn base_direction(&self) -> Option<super::BaseDirection> {
        None
    }
}

#[cfg(test)]
mod test {
    use crate::term::assert_consistent_term_impl;

    use super::*;

    #[test]
    fn i18n_string_as_term() {
        let en = LanguageTag::new("en").unwrap();
        let frfr = LanguageTag::new("fr-FR").unwrap();
        let t1 = "chat" * en;
        assert_consistent_term_impl(&t1);
        let t2 = "chat" * frfr;
        assert_consistent_term_impl(&t2);
        let t3 = "cat" * en;
        assert_consistent_term_impl(&t3);
    }
}
