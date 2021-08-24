use super::*;
use Term::{List, Variable};

/// Produce a new formula by applying bindings to f.
///
/// NB: no check is performed to prevent bindings to bind quantified variables of f or its
/// subformulae.
pub fn bind(f: &Formula, bindings: &[(usize, &Term)], offset: usize) -> Formula {
    let for_some = f
        .for_some()
        .iter()
        .filter_map(
            |(vid, label)| match bindings.iter().find(|(bid, _)| vid == bid) {
                None => Some((*vid + offset, label.clone())),
                Some(_) => None,
            },
        )
        .collect();
    let for_all = f
        .for_all()
        .iter()
        .filter_map(
            |(vid, label)| match bindings.iter().find(|(bid, _)| vid == bid) {
                None => Some((*vid + offset, label.clone())),
                Some(_) => None,
            },
        )
        .collect();
    let triples = f
        .triples()
        .iter()
        .map(|[s, p, o]| {
            [
                bind_term(s, bindings, offset),
                bind_term(p, bindings, offset),
                bind_term(o, bindings, offset),
            ]
        })
        .collect();
    Formula::new(for_some, for_all, triples)
}

fn bind_term(t: &Term, bindings: &[(usize, &Term)], offset: usize) -> Term {
    match t {
        Variable(vid) => bindings
            .iter()
            .find_map(|(bid, bterm)| (*bid == *vid).then(|| (*bterm).clone()))
            .unwrap_or_else(|| Variable(vid + offset)),
        List(list) => List(
            list.iter()
                .map(|t| bind_term(t, bindings, offset))
                .collect(),
        ),
        Term::Formula(f) => bind_formula(f, bindings, offset).into(),
        _ => t.clone(),
    }
}

fn bind_formula(f: &Formula, bindings: &[(usize, &Term)], offset: usize) -> Formula {
    let for_some = f
        .for_some()
        .iter()
        .map(|(vid, label)| (*vid + offset, label.clone()))
        .collect();
    let for_all = f
        .for_all()
        .iter()
        .map(|(vid, label)| (*vid + offset, label.clone()))
        .collect();
    let triples = f
        .triples()
        .iter()
        .map(|[s, p, o]| {
            [
                bind_term(s, bindings, offset),
                bind_term(p, bindings, offset),
                bind_term(o, bindings, offset),
            ]
        })
        .collect();
    Formula::new(for_some, for_all, triples)
}
