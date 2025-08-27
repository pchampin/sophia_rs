/// Provide an implementation of [`Term`](super::Term)
/// for any wrapper type `W<T>(T)` where `T` implements [`Term`](super::Term).
#[macro_export]
macro_rules! impl_term_for_wrapper {
    ($wrapper: ident) => {
        impl<T: $crate::term::Term> $crate::term::Term for $wrapper<T> {
            type BorrowTerm<'x>
                = $wrapper<T::BorrowTerm<'x>>
            where
                T: 'x;

            fn kind(&self) -> $crate::term::TermKind {
                self.0.kind()
            }
            fn is_iri(&self) -> bool {
                self.0.is_iri()
            }
            fn is_blank_node(&self) -> bool {
                self.0.is_blank_node()
            }
            fn is_literal(&self) -> bool {
                self.0.is_literal()
            }
            fn is_variable(&self) -> bool {
                self.0.is_variable()
            }
            fn is_atom(&self) -> bool {
                self.0.is_atom()
            }
            fn is_triple(&self) -> bool {
                self.0.is_triple()
            }
            fn iri(&self) -> Option<sophia_iri::IriRef<$crate::MownStr<'_>>> {
                self.0.iri()
            }
            fn bnode_id(&self) -> Option<$crate::term::BnodeId<$crate::MownStr<'_>>> {
                self.0.bnode_id()
            }
            fn lexical_form(&self) -> Option<$crate::MownStr<'_>> {
                self.0.lexical_form()
            }
            fn datatype(&self) -> Option<sophia_iri::IriRef<$crate::MownStr<'_>>> {
                self.0.datatype()
            }
            fn language_tag(&self) -> Option<$crate::term::LanguageTag<$crate::MownStr<'_>>> {
                self.0.language_tag()
            }
            fn base_direction(&self) -> Option<$crate::term::BaseDirection> {
                self.0.base_direction()
            }
            fn variable(&self) -> Option<$crate::term::VarName<$crate::MownStr<'_>>> {
                self.0.variable()
            }
            fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
                self.0.triple().map(|a| a.map($wrapper))
            }
            fn to_triple(self) -> Option<[Self; 3]> {
                self.0.to_triple().map(|a| a.map($wrapper))
            }
            fn borrow_term(&self) -> Self::BorrowTerm<'_> {
                $wrapper(self.0.borrow_term())
            }
            fn eq<U: Term>(&self, other: U) -> bool {
                self.0.eq(other)
            }
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state)
            }
            fn into_term<U: $crate::term::FromTerm>(self) -> U {
                self.0.into_term()
            }
            fn try_into_term<U: $crate::term::TryFromTerm>(self) -> Result<U, U::Error> {
                self.0.try_into_term()
            }
            // NOT overriding the iterator methods
            // (constituents, to_constituents, atoms, to_atoms)
            // because this would introduce an additional Box<dyn ...> indirection,
            // potentially hurting performances,
            // beyond the benefit of a hypothetical custom impl of these methods in T.
        }
    };
}
