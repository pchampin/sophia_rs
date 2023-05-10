use sophia_api::MownStr;
use sophia_iri::Iri;

pub type IriBuf = Iri<MownStr<'static>>;

pub fn iri_buf(iri: &str) -> IriBuf {
    IriBuf::new_unchecked(iri.to_owned().into())
}
