use super::*;
use std::convert::Infallible;
use Term::Variable;

/// TODO document
///
/// the callback is called for each bindings that unifies f1 with f2;
/// the returned value of the callback indicates whether further bindings should be searched or not
pub fn unify_with<F>(f1: &Formula, f2: &Formula, context: Option<&Formula>, mut callback: F)
where
    F: FnMut(&[(usize, &Term)]) -> bool,
{
    if f1.for_all().len() > f2.for_all().len() {
        return;
    }
    let mut bindable = vec![];
    let mut bindings = vec![];
    let stash1: Vec<_>;
    if let Some(context) = context {
        // outer existentials are considered as constants
        stash1 = context
            .for_some()
            .iter()
            .map(|(vid, _)| (*vid, Variable(*vid)))
            .collect();
        for (vid, var) in &stash1 {
            bindings.push((*vid, var));
        }
        // outer univerals are bindable by unification
        for (vid, _) in context.for_all() {
            bindable.push(*vid);
        }
    }
    // inner existentials are bindable by unification
    for (vid, _) in f1.for_some() {
        bindable.push(*vid);
    }
    let mut ctx = Context::new(bindable, bindings);
    let cp = ctx.checkpoint();

    // inner universals should unify to inner universals of the target formula
    let uni1: Vec<_> = f1.for_all().iter().map(|(vid, _)| *vid).collect();
    let uni2: Vec<_> = f2.for_all().iter().map(|(vid, _)| Variable(*vid)).collect();
    utils::mappings(&uni1, &uni2, |mapping| -> Result<(), Infallible> {
        for (vid, var) in mapping {
            ctx.bindings.push((**vid, var));
        }
        ctx.unify_triples_with(f1.triples(), f2.triples(), &mut callback);
        ctx.rollback(cp);
        Ok(())
    })
    .unwrap();
}

pub struct Context<'a> {
    /// list of variables that can be bound by unification
    bindable: Vec<usize>,
    /// list of current bindings
    /// NB: this list can contain variables that are not in `self.bindable`;
    /// this happens when unifying subformulae, to map quantified variables of both
    bindings: Vec<(usize, &'a Term)>,
}

impl<'a> Context<'a> {
    fn new(bindable: Vec<usize>, bindings: Vec<(usize, &'a Term)>) -> Context<'a> {
        Context { bindable, bindings }
    }

    /// Tries to unify every triple from t1 wih t2 in this context,
    /// updating them if necessary.
    /// For each successful unification, call `callback` with the given mapping.
    ///
    /// May result to Ok(true), Ok(false), or Err(()) in case the unification must be deferred.
    #[allow(clippy::result_unit_err)]
    pub fn unify_triples_with<F>(
        &mut self,
        triples1: &'a [[Term; 3]],
        triples2: &'a [[Term; 3]],
        callback: &mut F,
    ) where
        F: FnMut(&[(usize, &Term)]) -> bool,
    {
        // TODO maybe change the signature to accept formulae instead of triple vecs,
        // and do the right thing with their bindings here?
        let triples1: Vec<_> = triples1.iter().collect();
        let mut deferred = vec![];
        self.unify_triples_rec(
            &triples1,
            triples2,
            callback,
            self.checkpoint(),
            &mut deferred,
        );
    }

    /// Tries to unify every triple from t1 wih t2 in this context,
    /// updating them if necessary.
    ///
    /// May result to Ok(true), Ok(false), or Err(()) in case the unification must be deferred.
    #[allow(clippy::result_unit_err)]
    fn unify_triples_rec<F>(
        &mut self,
        triples1: &[&'a [Term; 3]],
        triples2: &'a [[Term; 3]],
        callback: &mut F,
        checkpoint: usize,
        deferred: &mut Vec<&'a [Term; 3]>,
    ) -> bool
    where
        F: FnMut(&[(usize, &Term)]) -> bool,
    {
        match triples1 {
            [triple1, rest @ ..] => {
                if is_builtin(&triple1[1]) {
                    todo!("handle builtin")
                }
                if is_open(&triple1[1]) {
                    for triple2 in triples2 {
                        let cp = self.checkpoint();
                        if self.unify_triple_with(triple1, triple2) {
                            let cont =
                                self.unify_triples_rec(rest, triples2, callback, cp, deferred);
                            if !cont {
                                break;
                            }
                        }
                        self.rollback(cp);
                    }
                }
                true
            }
            [] => {
                if !deferred.is_empty() {
                    let new_cp = self.checkpoint();
                    if new_cp != checkpoint {
                        // some progress has been made since last "defer loop", try again
                        let mut new_deferred = vec![];
                        self.unify_triples_rec(
                            deferred,
                            triples2,
                            callback,
                            new_cp,
                            &mut new_deferred,
                        )
                    } else {
                        false // give up
                    }
                } else {
                    callback(&self.bindings)
                }
            }
        }
    }

    /// Tries to unify t1 wih t2 in this context,
    /// updating them if necessary.
    ///
    /// May result to Ok(true), Ok(false), or Err(()) in case the unification must be deferred.
    pub fn unify_triple_with(&mut self, tr1: &'a [Term; 3], tr2: &'a [Term; 3]) -> bool {
        let cp = self.checkpoint();
        for (t1, t2) in tr1.iter().zip(tr2.iter()) {
            if !self.unify_term_with(t1, t2) {
                self.rollback(cp);
                return false;
            }
        }
        true
    }

    /// Tries to unify t1 wih t2 in this context,
    /// updating it if necessary.
    pub fn unify_term_with(&mut self, t1: &'a Term, t2: &'a Term) -> bool {
        match (t1, t2) {
            (Term::Iri(iri1), Term::Iri(iri2)) => iri1 == iri2,
            (Term::Literal(lit1), Term::Literal(lit2)) => lit1 == lit2,
            (Term::Variable(vid1), _) => {
                if let Some(t1) = self.get_binding(*vid1) {
                    t1 == t2
                } else if !self.can_bind(*vid1) {
                    t1 == t2
                } else {
                    self.bindings.push((*vid1, t2));
                    true
                }
            }
            (Term::List(lst1), Term::List(lst2)) => {
                if lst1.len() != lst2.len() {
                    return false;
                }
                let cp = self.checkpoint();
                for (t1, t2) in lst1.iter().zip(lst2.iter()) {
                    if !self.unify_term_with(t1, t2) {
                        self.rollback(cp);
                        return false;
                    }
                }
                true
            }
            (Term::Formula(f1), Term::Formula(f2)) => {
                if f1.for_some().len() != f2.for_some().len() {
                    return false;
                }
                if f1.for_all().len() != f2.for_all().len() {
                    return false;
                }
                todo!("unifying sub-formulae") // for all possible injection (using utils::injections) for existentials and universals in f1 and f2, check if every triple of f1 matches one triple of f2, and if all triples of f2 are matched
            }
            _ => false,
        }
    }

    fn can_bind(&self, vid: usize) -> bool {
        self.bindable.iter().any(|i| *i == vid)
    }

    fn get_binding(&self, vid: usize) -> Option<&'a Term> {
        self.bindings
            .iter()
            .find_map(|(i, t)| (*i == vid).then(|| *t))
    }

    fn checkpoint(&self) -> usize {
        self.bindings.len()
    }

    fn rollback(&mut self, checkpoint: usize) {
        self.bindings.truncate(checkpoint)
    }
}

#[cfg(test)]
#[allow(clippy::bool_assert_comparison)]
mod test {
    use super::*;
    use test_case::test_case;
    use Term::Variable;

    #[test_case(":a", ":a" => true)]
    #[test_case(":a", ":b" => false)]
    #[test_case(":a", "42" => false)]
    #[test_case("42", ":a" => false)]
    #[test_case("?0", ":a" => true)]
    #[test_case("?0", "42" => true)]
    #[test_case(":a", "?0" => false)]
    #[test_case("?0", "?10" => true)]
    #[test_case("?1", "?10" => false)]
    #[test_case("?0", "{ :a :b :c }" => true)]
    #[test_case("?0", "(:a :b)" => true)]
    #[test_case("(:a :b)", "(:a :b)" => true)]
    #[test_case("(:a :b)", "(:a :b :c)" => false)]
    #[test_case("(?0 :b)", "(:a :b)" => true)]
    #[test_case("(?0 ?2)", "(:a :b)" => true)]
    #[test_case("(?0 ?0)", "(:a :b)" => false)]
    #[test_case("(?0 ?0)", "(:a :a)" => true)]
    fn unify_term_with(t1: &str, t2: &str) -> bool {
        Context::new(vec![0, 2, 4, 6], vec![]).unify_term_with(&ezterm(t1), &ezterm(t2))
    }

    #[test_case(":a :b :d", ":a :b :c" => sol(&[]); "different 1")]
    #[test_case(":a :b :c", ":a :b :c" => sol(&[ &[] ]); "identical 1")]
    #[test_case("?x :b :c", ":a :b :c" => sol(&[ &[(0, ":a")] ]); "1 simple match")]
    #[test_case(":a :b :c", ":a :b :c , :d" => sol(&[ &[] ]); "contained 1st")]
    #[test_case(":a :b :d", ":a :b :c , :d" => sol(&[ &[] ]); "contained 2nd")]
    #[test_case(":a :b ?x", ":a :b :c , :d" => sol(&[ &[(0, ":c")], &[(0, ":d")] ]); "2 simple matches")]
    #[test_case(":a :b ?x . ?y :b :d", ":a :b :c , :d" => sol(&[ &[(0, ":c"), (1, ":a")], &[(0, ":d"), (1, ":a")] ]); "2 double matches")]
    #[test_case("?x :b :c , :d", ":a :b :c , :d" => sol(&[ &[(0, ":a")] ]); "compatible co-occurence")]
    #[test_case("?x :b :c . :a :b ?x", ":a :b :c , :d" => sol(&[]); "incompatible co-occurence")]
    fn unify_triples_with(src1: &str, src2: &str) -> Vec<Vec<(usize, Term)>> {
        let mut bindings = vec![];
        let f1 = parse(src1, ParseConfig::new()).unwrap();
        let f2 = parse(src2, ParseConfig::new().with_variable_offset(10)).unwrap();
        let mut ctx = Context::new((0..10).collect(), vec![]);
        ctx.unify_triples_with(f1.triples(), f2.triples(), &mut |b| {
            bindings.push(b.iter().map(|(id, t)| (*id, (*t).clone())).collect());
            true
        });
        bindings
    }

    #[test_case("{ :a :b :c } :vs { :a :b :d } " => sol(&[]); "different 1")]
    #[test_case("{ :a :b :c } :vs { :a :b :c }" => sol(&[ &[] ]); "identical 1")]
    #[test_case("{ ?x :b :c } :vs { :a :b :c }" => sol(&[ &[(0, ":a")] ]); "1 simple match")]
    #[test_case("{ :a :b :c } :vs { :a :b :c , :d }" => sol(&[ &[] ]); "contained 1st")]
    #[test_case("{ :a :b :d } :vs { :a :b :c , :d }" => sol(&[ &[] ]); "contained 2nd")]
    #[test_case("{ :a :b ?x } :vs { :a :b :c , :d }" => sol(&[ &[(0, ":c")], &[(0, ":d")] ]); "2 simple matches")]
    #[test_case("{ :a :b ?x . ?y :b :d } :vs { :a :b :c , :d }" => sol(&[ &[(0, ":c"), (1, ":a")], &[(0, ":d"), (1, ":a")] ]); "2 double matches")]
    #[test_case("{ ?x :b :c , :d } :vs { :a :b :c , :d }" => sol(&[ &[(0, ":a")] ]); "compatible co-occurence")]
    #[test_case("{ ?x :b :c . :a :b ?x } :vs { :a :b :c , :d }" => sol(&[]); "incompatible co-occurence")]
    #[test_case("{ @forAll :x . :x a :Person } :vs { @forAll :y . :y a :Person }" => sol(&[ &[ (0, "?1")] ]); "one local universal")]
    // not implemented yet   #[test_case("{ @forAll :x , :y . :x :likes :y } :vs { @forAll :y , :x . :x :likes :y }" => sol(&[ &[ (0, "?3"), (1, "?2")] ]); "two local universals")]
    fn unify_with(src: &str) -> Vec<Vec<(usize, Term)>> {
        let f = parse(src, Default::default()).unwrap();
        if let [Term::Formula(f1), _, Term::Formula(f2)] = &f.triples()[0] {
            let mut bindings = vec![];
            super::unify_with(f1, f2, Some(&f), |b| {
                bindings.push(b.iter().map(|(id, t)| (*id, (*t).clone())).collect());
                true
            });
            bindings
        } else {
            panic!("Test data has wrong format")
        }
    }

    fn ezterm(txt: &str) -> Term {
        match txt.as_bytes() {
            [b':', ..] => {
                let mut iri = DEFAULT_BASE.to_string();
                iri.push_str(&txt[1..]);
                Iri::new_unchecked(iri).into()
            }
            [b'?', ..] => Variable(txt[1..].parse::<usize>().unwrap()),
            [b'{', .., b'}'] => parse(&txt[1..txt.len() - 1], Default::default())
                .unwrap()
                .into(),
            [b'(', .., b')'] => Term::List(
                txt[1..txt.len() - 1]
                    .trim()
                    .split(' ')
                    .map(ezterm)
                    .collect(),
            ),
            [c, ..] if b"0123456789+-".contains(c) => {
                if txt.contains('.') || txt.contains('e') {
                    Term::Literal(txt.parse::<f64>().unwrap().into())
                } else {
                    Term::Literal(txt.parse::<isize>().unwrap().into())
                }
            }
            _ => panic!("unrecognized EZTerm {:?}", txt),
        }
    }

    fn sol(slice: &[&[(usize, &str)]]) -> Vec<Vec<(usize, Term)>> {
        slice
            .iter()
            .map(|subslice| {
                subslice
                    .iter()
                    .map(|(id, ezt)| (*id, ezterm(ezt)))
                    .collect()
            })
            .collect()
    }
}
