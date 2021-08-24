use super::*;
use ns::log;

// TODO ultimately change the contract of fc_reason
// to merge the results into f
pub fn fc_reason(f: &Formula) -> Vec<Formula> {
    let empty = Formula::empty();
    let mut results = vec![];
    let mut offset = f.max_var() + 1;

    for [body, pred, head] in f.triples() {
        match pred {
            Term::Iri(iri) if iri == &log::implies => (),
            _ => continue,
        };
        let body = match body {
            Term::Formula(f) => f,
            Term::Literal(Literal::Boolean(true)) => &empty,
            _ => continue,
        };
        let head = match head {
            Term::Formula(f) => f,
            Term::Literal(Literal::Boolean(false)) => todo!("inference fuse"),
            _ => continue,
        };
        unify_with(body, f, Some(f), |bindings| {
            let result = bind(head, bindings, offset);
            offset = offset.max(result.max_var() + 1);
            results.push(result);
            true
        });
    }
    results
}
