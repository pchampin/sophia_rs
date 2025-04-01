use super::*;
use crate::{Loader, LocalLoader, NoLoader, test::*};
use sophia_api::prelude::*;
use sophia_api::term::SimpleTerm;
use std::sync::Arc;

use ResourceError::*;

#[test]
fn id() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(f1r1.id(), &F1R1);
    Ok(())
}

#[test]
fn get_term() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(f1r1.get_term(EX_ID)?, "res1");
    Ok(())
}

#[test]
fn get_term_no_value() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_term(EX_UNUSED);
    assert!(matches!(res, Err(NoValueFor { .. })));
    Ok(())
}

#[test]
fn get_term_too_many_values() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_term(EX_RELATED);
    assert!(matches!(res, Err(UnexpectedMultipleValueFor { .. })));
    Ok(())
}

#[test]
fn get_term_unreachable() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(
        f1r1.get_term(EX_UNREACHABLE)?,
        Iri::new_unchecked("http://somewhere.else/")
    );
    Ok(())
}

#[test]
fn get_any_term() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(f1r1.get_any_term(EX_ID)?.unwrap(), "res1");
    assert!(f1r1.get_any_term(EX_RELATED)?.is_some());
    Ok(())
}

#[test]
fn get_any_term_fail() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let opt = f1r1.get_any_term(EX_UNUSED)?;
    assert!(opt.is_none());
    Ok(())
}

#[test]
fn get_all_terms() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .get_all_terms(EX_RELATED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable();
    assert_eq!(terms.len(), 3);
    assert!(terms[0].is_blank_node());
    assert_eq!(terms[1], F1R2);
    assert_eq!(terms[2], F1R3);
    Ok(())
}

#[test]
fn get_all_terms_empty() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .get_all_terms(EX_UNUSED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable();
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_terms_items() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let terms = f1r1
        .get_term_items(EX_LIST)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(terms.len(), 3);
    assert_eq!(terms[0], F1R3);
    assert_eq!(terms[1], F1R2);
    assert_eq!(terms[2], F2R1);
    Ok(())
}

#[test]
fn get_terms_items_empty() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let terms = f1r2
        .get_term_items(EX_LIST)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_terms_items_absent() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let terms = f1r3
        .get_term_items(EX_LIST)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn pred_term() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    assert_eq!(f1r3.pred_term(EX_RELATED)?, F1R1);
    Ok(())
}

#[test]
fn pred_term_no_value() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.pred_term(EX_UNUSED);
    assert!(matches!(res, Err(NoValueFor { .. })));
    Ok(())
}

#[test]
fn pred_term_too_many_values() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let res = f1r2.pred_term(EX_RELATED);
    assert!(matches!(res, Err(UnexpectedMultipleValueFor { .. })));
    Ok(())
}

#[test]
fn pred_any_term() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    assert_eq!(f1r3.pred_any_term(EX_RELATED)?.unwrap(), F1R1);
    let f1r2 = make_rsc(F1R2)?;
    assert!(f1r2.pred_any_term(EX_RELATED)?.is_some());
    Ok(())
}

#[test]
fn pred_any_term_fail() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let opt = f1r1.pred_any_term(EX_UNUSED)?;
    assert!(opt.is_none());
    Ok(())
}

#[test]
fn pred_all_terms() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let mut terms = f1r2
        .pred_all_terms(EX_RELATED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable();
    assert_eq!(terms.len(), 2);
    assert_eq!(terms[0], F1R1);
    assert_eq!(terms[1], F1R3);
    Ok(())
}

#[test]
fn pred_all_terms_empty() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .pred_all_terms(EX_UNUSED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable();
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_resource() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(f1r1.get_resource(EX_ID)?.id(), &"res1");
    let rsc = f1r1.get_resource(EX_NEXT)?;
    assert_eq!(rsc.get_term(EX_ID)?, "res2");
    Ok(())
}

#[test]
fn get_resource_no_ext() -> TestResult {
    let f1r1 = make_rsc(F1XR1)?;
    assert_eq!(f1r1.get_resource(EX_ID)?.id(), &"res1");
    let rsc = f1r1.get_resource(EX_NEXT)?;
    assert_eq!(rsc.get_term(EX_ID)?, "res2");
    Ok(())
}

#[test]
fn get_resource_foreign() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let rsc1 = f1r1.get_resource(EX_FOREIGN1)?;
    assert_eq!(rsc1.id(), &F2R1);
    assert_eq!(rsc1.get_term(EX_ID)?, "res1");
    let rsc2 = f1r1.get_resource(EX_FOREIGN2)?;
    assert_eq!(rsc2.id(), &F2R2);
    assert_eq!(rsc2.get_term(EX_NEXT)?, F2R1);
    Ok(())
}

#[test]
fn get_resource_no_value() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_resource(EX_UNUSED);
    assert!(matches!(res, Err(NoValueFor { .. })));
    Ok(())
}

#[test]
fn get_resource_too_many_values() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_resource(EX_RELATED);
    assert!(matches!(res, Err(UnexpectedMultipleValueFor { .. })));
    Ok(())
}

#[test]
fn get_resource_unreachable() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_resource(EX_UNREACHABLE);
    assert!(matches!(res, Err(LoaderError(_))));
    Ok(())
}

#[test]
fn get_any_resource() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(f1r1.get_any_resource(EX_ID)?.unwrap().id(), &"res1");
    assert!(f1r1.get_any_resource(EX_RELATED)?.is_some());
    Ok(())
}

#[test]
fn get_any_resource_fail() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let opt = f1r1.get_any_resource(EX_UNUSED)?;
    assert!(opt.is_none());
    Ok(())
}

#[test]
fn get_all_resources() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .get_all_resources(EX_RELATED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 3);
    assert!(terms[0].id().is_blank_node());
    assert_eq!(terms[0].get_term(EX_ID)?, "res4");
    assert_eq!(terms[1].id(), &F1R2);
    assert_eq!(terms[1].get_term(EX_ID)?, "res2");
    assert_eq!(terms[2].id(), &F1R3);
    assert_eq!(terms[2].get_term(EX_ID)?, "res3");
    Ok(())
}

#[test]
fn get_all_resources_empty() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .get_all_resources(EX_UNUSED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_resources_items() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let terms = f1r1
        .get_resource_items(EX_LIST)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(terms.len(), 3);
    assert_eq!(terms[0].id(), &F1R3);
    assert_eq!(terms[0].get_term(EX_ID)?, "res3");
    assert_eq!(terms[1].id(), &F1R2);
    assert_eq!(terms[1].get_term(EX_ID)?, "res2");
    assert_eq!(terms[2].id(), &F2R1);
    assert_eq!(terms[2].get_term(EX_ID)?, "res1");
    Ok(())
}

#[test]
fn get_resources_items_empty() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let terms = f1r2
        .get_resource_items(EX_LIST)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_resources_items_absence() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let terms = f1r3
        .get_resource_items(EX_LIST)
        .collect::<Result<Vec<_>, _>>()?;
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn pred_resource() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let pred = f1r3.pred_resource(EX_RELATED)?;
    assert_eq!(pred.id(), &F1R1);
    assert_eq!(pred.get_term(EX_ID)?, "res1");
    Ok(())
}

#[test]
fn pred_resource_no_value() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.pred_resource(EX_UNUSED);
    assert!(matches!(res, Err(NoValueFor { .. })));
    Ok(())
}

#[test]
fn pred_resource_too_many_values() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let res = f1r2.pred_resource(EX_RELATED);
    assert!(matches!(res, Err(UnexpectedMultipleValueFor { .. })));
    Ok(())
}

#[test]
fn pred_any_resource() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let pred = f1r3.pred_any_resource(EX_RELATED)?.unwrap();
    assert_eq!(pred.id(), &F1R1);
    assert_eq!(pred.get_term(EX_ID)?, "res1");
    let f1r2 = make_rsc(F1R2)?;
    assert!(f1r2.pred_any_resource(EX_RELATED)?.is_some());
    Ok(())
}

#[test]
fn pred_any_resource_fail() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let opt = f1r1.pred_any_resource(EX_UNUSED)?;
    assert!(opt.is_none());
    Ok(())
}

#[test]
fn pred_all_resources() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let mut terms = f1r2
        .pred_all_resources(EX_RELATED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 2);
    assert_eq!(terms[0].id(), &F1R1);
    assert_eq!(terms[0].get_term(EX_ID)?, "res1");
    assert_eq!(terms[1].id(), &F1R3);
    assert_eq!(terms[1].get_term(EX_ID)?, "res3");
    Ok(())
}

#[test]
fn pred_all_resources_empty() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .pred_all_resources(EX_UNUSED)
        .collect::<Result<Vec<_>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_typed() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let rsc: WithId = f1r1.get_typed(EX_NEXT)?;
    assert_eq!(rsc.get_term(EX_ID)?, "res2");
    Ok(())
}

#[test]
fn get_typed_foreign() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let rsc1: WithId = f1r1.get_typed(EX_FOREIGN1)?;
    assert_eq!(rsc1.id(), &F2R1);
    assert_eq!(rsc1.get_term(EX_ID)?, "res1");
    let res = f1r1.get_typed::<WithId, _>(EX_FOREIGN2);
    assert!(res.is_err());
    Ok(())
}

#[test]
fn get_typed_no_value() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_typed::<WithId, _>(EX_UNUSED);
    assert!(matches!(res, Err(NoValueFor { .. })));
    Ok(())
}

#[test]
fn get_typed_too_many_values() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_typed::<WithId, _>(EX_RELATED);
    assert!(matches!(res, Err(UnexpectedMultipleValueFor { .. })));
    Ok(())
}

#[test]
fn get_typed_unreachable() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.get_typed::<WithId, _>(EX_UNREACHABLE);
    assert!(matches!(res, Err(LoaderError(_))));
    Ok(())
}

#[test]
fn get_any_typed() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    assert_eq!(
        f1r1.get_any_typed::<WithId, _>(EX_NEXT)?.unwrap().id(),
        &F1R2
    );
    assert!(f1r1.get_any_typed::<WithId, _>(EX_RELATED)?.is_some());
    Ok(())
}

#[test]
fn get_any_typed_fail() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let opt = f1r1.get_any_typed::<WithId, _>(EX_UNUSED)?;
    assert!(opt.is_none());
    Ok(())
}

#[test]
fn get_all_typed() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .get_all_typed(EX_RELATED)
        .collect::<Result<Vec<WithId>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 3);
    assert!(terms[0].id().is_blank_node());
    assert_eq!(terms[0].get_term(EX_ID)?, "res4");
    assert_eq!(terms[1].id(), &F1R2);
    assert_eq!(terms[1].get_term(EX_ID)?, "res2");
    assert_eq!(terms[2].id(), &F1R3);
    assert_eq!(terms[2].get_term(EX_ID)?, "res3");
    Ok(())
}

#[test]
fn get_all_typed_empty() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .get_all_typed(EX_UNUSED)
        .collect::<Result<Vec<WithId>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_typed_items() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let terms = f1r1
        .get_typed_items(EX_LIST)
        .collect::<Result<Vec<WithId>, _>>()?;
    assert_eq!(terms.len(), 3);
    assert_eq!(terms[0].id(), &F1R3);
    assert_eq!(terms[0].get_term(EX_ID)?, "res3");
    assert_eq!(terms[1].id(), &F1R2);
    assert_eq!(terms[1].get_term(EX_ID)?, "res2");
    assert_eq!(terms[2].id(), &F2R1);
    assert_eq!(terms[2].get_term(EX_ID)?, "res1");
    Ok(())
}

#[test]
fn get_typed_items_empty() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let terms = f1r2
        .get_typed_items(EX_LIST)
        .collect::<Result<Vec<WithId>, _>>()?;
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn get_typed_items_absent() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let terms = f1r3
        .get_typed_items(EX_LIST)
        .collect::<Result<Vec<WithId>, _>>()?;
    assert_eq!(terms.len(), 0);
    Ok(())
}

#[test]
fn pred_typed() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let pred: WithId = f1r3.pred_typed(EX_RELATED)?;
    assert_eq!(pred.id(), &F1R1);
    assert_eq!(pred.get_term(EX_ID)?, "res1");
    Ok(())
}

#[test]
fn pred_typed_no_value() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let res = f1r1.pred_typed::<WithId, _>(EX_UNUSED);
    assert!(matches!(res, Err(NoValueFor { .. })));
    Ok(())
}

#[test]
fn pred_typed_too_many_values() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let res = f1r2.pred_typed::<WithId, _>(EX_RELATED);
    assert!(matches!(res, Err(UnexpectedMultipleValueFor { .. })));
    Ok(())
}

#[test]
fn pred_any_typed() -> TestResult {
    let f1r3 = make_rsc(F1R3)?;
    let pred: WithId = f1r3.pred_any_typed(EX_RELATED)?.unwrap();
    assert_eq!(pred.id(), &F1R1);
    assert_eq!(pred.get_term(EX_ID)?, "res1");
    let f1r2 = make_rsc(F1R2)?;
    assert!(f1r2.pred_any_typed::<WithId, _>(EX_RELATED)?.is_some());
    Ok(())
}

#[test]
fn pred_any_typed_fail() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let opt = f1r1.pred_any_typed::<WithId, _>(EX_UNUSED)?;
    assert!(opt.is_none());
    Ok(())
}

#[test]
fn pred_all_typed() -> TestResult {
    let f1r2 = make_rsc(F1R2)?;
    let mut terms = f1r2
        .pred_all_typed(EX_RELATED)
        .collect::<Result<Vec<WithId>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 2);
    assert_eq!(terms[0].id(), &F1R1);
    assert_eq!(terms[0].get_term(EX_ID)?, "res1");
    assert_eq!(terms[1].id(), &F1R3);
    assert_eq!(terms[1].get_term(EX_ID)?, "res3");
    Ok(())
}

#[test]
fn pred_all_typed_empty() -> TestResult {
    let f1r1 = make_rsc(F1R1)?;
    let mut terms = f1r1
        .pred_all_typed(EX_UNUSED)
        .collect::<Result<Vec<WithId>, _>>()?;
    terms.sort_unstable_by_key(|r| r.id().try_into_term::<SimpleTerm>());
    assert_eq!(terms.len(), 0);
    Ok(())
}

// Test that the loader is not used when the neighbouring resource is in the same graph
#[test]
fn no_reload() -> TestResult {
    let base = Some(F1.map_unchecked(String::from));
    let ttl = std::fs::read_to_string("test/file1.ttl")?;
    let graph = sophia_turtle::parser::turtle::TurtleParser { base: base.clone() }
        .parse(ttl.as_bytes())
        .collect_triples::<MyGraph>()?;
    let res = Resource::new(F1R1, base, Arc::new(graph), Arc::new(NoLoader()));
    let _ = res.get_resource(EX_NEXT)?;
    Ok(())
}

fn make_rsc(iri: Iri<&str>) -> Result<Resource<MyGraph, LocalLoader>, Box<dyn std::error::Error>> {
    Ok(make_loader().arced().get_resource(iri)?)
}
