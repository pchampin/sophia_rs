//! Tests the [`Loader`] trait, using its [`LocalLoader`] implementation.
use super::*;
use crate::{test::*, Resource};
use sophia_api::MownStr;
use sophia_iri::Iri;
use std::fs::read;
use std::path::Path;

#[test]
fn empty_fails() -> TestResult {
    let ldr = LocalLoader::default();
    assert!(matches!(
        ldr.get(F1R1),
        Err(LoaderError::UnsupportedIri(..)),
    ));
    Ok(())
}

#[test]
fn get_file1() -> TestResult {
    let ldr = make_loader();
    assert_eq!(
        ldr.get(F1)?,
        (read("test/file1.ttl")?, "text/turtle".into()),
    );
    Ok(())
}

#[test]
fn get_file1_no_ext() -> TestResult {
    let ldr = make_loader();
    assert_eq!(
        ldr.get(F1X)?,
        (read("test/file1.ttl")?, "text/turtle".into()),
    );
    Ok(())
}

#[test]
fn get_file1_with_add() -> TestResult {
    let mut ldr = LocalLoader::default();
    let ns = NS.map_unchecked(MownStr::from);
    ldr.add(ns, Path::new("test").canonicalize()?)?;
    assert_eq!(
        ldr.get(F1R1)?,
        (read("test/file1.ttl")?, "text/turtle".into()),
    );
    Ok(())
}

#[test]
fn file_not_found() -> TestResult {
    let ldr = make_loader();
    assert!(matches!(ldr.get(FAIL), Err(LoaderError::NotFound(..)),));
    Ok(())
}

#[test]
fn iri_not_supported() -> TestResult {
    let ldr = make_loader();
    assert!(matches!(
        ldr.get(Iri::new_unchecked("http://w3.org/")),
        Err(LoaderError::UnsupportedIri(..)),
    ));
    Ok(())
}

#[test]
fn io_error() -> TestResult {
    let ldr = make_loader();
    assert!(matches!(ldr.get(SUBDIR), Err(LoaderError::IoError(..)),));
    Ok(())
}

#[test]
fn graph() -> TestResult {
    let ldr = make_loader();
    let g: MyGraph = ldr.get_graph(F1)?;
    assert_eq!(g.len(), F1_LEN);
    Ok(())
}

#[test]
fn resource() -> TestResult {
    let ldr = make_loader().arced();
    let r1: Resource<MyGraph, _> = ldr.get_resource(F1)?;
    assert_eq!(r1.id(), &F1);
    assert_eq!(r1.graph().len(), F1_LEN);
    let r2: Resource<MyGraph, _> = ldr.get_resource(F2)?;
    assert_eq!(r2.id(), &F2);
    assert_eq!(r2.graph().len(), F2_LEN);
    Ok(())
}

#[test]
fn resource_from() -> TestResult {
    let ldr = make_loader().arced();
    let r: Resource<MyGraph, _> = ldr.get_resource_from(F2R1, F1)?;
    assert_eq!(r.id(), &F2R1);
    assert_eq!(r.graph().len(), F1_LEN);
    Ok(())
}

#[test]
fn typed() -> TestResult {
    let ldr = make_loader().arced();
    let r = ldr.get_typed::<WithId, _, _>(F1R1)?;
    assert_eq!(r.id(), &F1R1);
    assert_eq!(r.graph().len(), F1_LEN);
    Ok(())
}

#[test]
fn typed_fail() -> TestResult {
    let ldr = make_loader().arced();
    assert!(matches!(
        ldr.get_typed::<WithId, _, _>(F2R2),
        Err(crate::ResourceError::NoValueFor { .. }),
    ));
    Ok(())
}
