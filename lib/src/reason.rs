use super::ns::log;
use super::*;
use std::mem::swap;

// TODO ultimately change the contract of fc_reason
// to merge the results into f
/// Forward-chaining reasoner
pub fn fc_reason(kb: &Formula) -> Vec<Formula> {
    let empty = Formula::empty();
    let mut results = vec![];

    for [body, pred, head] in kb.triples() {
        if !matches!(pred, Term::Iri(iri) if iri == &log::implies) {
            continue;
        }
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
        for bindings in match_formula(body, kb) {
            results.push(bindings.bind_formula(head));
        }
    }
    results
}

/// Iterate over the matches of a formula in a knowledge base
fn match_formula<'a>(f: &'a Formula, kb: &'a Formula) -> Backtracker<FormulaMatcher<'a>> {
    let tm = TripleMatcher {
        b: Bindings::new().freeze(),
        f,
        kb,
        i: 0,
        j: 0,
    };
    Backtracker::new(tm.into())
}

#[derive(Debug)]
enum FormulaMatcher<'a> {
    TripleMatcher(TripleMatcher<'a>),
    BuiltinUnifier(BuiltinUnifier<'a>),
    TermUnifier(TermUnifier<'a>),
    FormulaUnifier(FormulaUnifier<'a>),
}

impl<'a> Choices for FormulaMatcher<'a> {
    type Output = Arc<Bindings<'a>>;

    fn next(&mut self) -> Option<Tree<Self>> {
        match self {
            Self::TripleMatcher(tm) => tm.next(),
            Self::BuiltinUnifier(bu) => bu.next(),
            Self::TermUnifier(tu) => tu.choice(),
            Self::FormulaUnifier(fu) => fu.next(),
        }
    }
}

#[derive(Debug)]
/// A [`TripleMatcher`] tries to match a triple of f with every triple of kb,
/// delegating to [`TermUnifier`] or [`BuiltinUnifier`] to check if this works.
///
/// If there are no more triples in f,
/// [`TripleMatcher`] instead returns its bindings as the result.
struct TripleMatcher<'a> {
    b: Arc<Bindings<'a>>,
    f: &'a Formula,
    kb: &'a Formula,
    i: usize, // triple index in f
    j: usize, // triple index in kb
}

impl<'a> TripleMatcher<'a> {
    fn next(&mut self) -> Option<Tree<FormulaMatcher<'a>>> {
        if self.i >= self.f.triples().len() {
            if self.j > 0 {
                None
            } else {
                self.j += 1;
                Some(Tree::Leaf(Arc::clone(&self.b)))
            }
        } else  if self.j == self.kb.triples().len() {
            self.i += 1;
            self.next()
        } else {
            let pred = &self.f.triples()[self.i][1];
            let children = if is_builtin(pred) {
                BuiltinUnifier::from(&*self).into()
            } else {
                TermUnifier::from(&*self).into()
            };
            self.j += 1;
            Some(Tree::Children(children))
        }
    }
}

#[derive(Debug)]
/// A [`BuiltinUnifier`] tries to match one triple of f with a builtin predicate.
///
/// When all possible bindings have been tried,
/// it delegates to [`TermUnifier`] if the builtin predicate is [open](`is_open_builtin`),
/// otherwise it delegates to [`TripleMatcher`] to handle the next triple of f.
struct BuiltinUnifier<'a> {
    b: Arc<Bindings<'a>>,
    f: &'a Formula,
    kb: &'a Formula,
    i: usize,    // triple index in f
    j: usize,    // triple index in kb
    state: bool, // replace with a proper state for managing built-ins
}

impl<'a> BuiltinUnifier<'a> {
    fn next(&mut self) -> Option<Tree<FormulaMatcher<'a>>> {
        // TODO handle builtins

        // when no more choices exist for the builtin:
        if !self.state {
            None
        } else {
            self.state = false;
            let pred = &self.f.triples()[self.i][1];
            if is_open_builtin(pred) {
                Some(Tree::Children(
                    TermUnifier {
                        b: self.b.spawn(),
                        f: self.f,
                        kb: self.kb,
                        i: self.i,
                        j: self.j,
                        stack: vec![(&self.f.triples()[self.i], &self.kb.triples()[self.j], 0)],
                    }
                    .into(),
                ))
            } else {
                None
            }
        }
    }
}

#[derive(Debug)]
/// A [`TermUnifier`] tries to unify as many terms as possible
/// of a triple in f with the corresponding term of kb,
/// and yields at most one choice:
///
/// - if fails to unify the terms, no choice at all;
/// - if it succeeds in unifying all the terms,
///   a [`TripleMatcher`] handling the next triple in f,
/// - if it encounters quoted graphs, a [`FormulaMatcher`] handling them.
struct TermUnifier<'a> {
    b: Bindings<'a>,
    f: &'a Formula,
    kb: &'a Formula,
    i: usize,                         // triple index in f
    j: usize,                         // triple index in kb
    stack: Vec<UnificationFrame<'a>>, // term indexes in the current triples and possibly lists
}

impl<'a> TermUnifier<'a> {
    fn choice(&mut self) -> Option<Tree<FormulaMatcher<'a>>> {
        if self.stack.is_empty() {
            None
        } else {
            use UnificationResult::*;

            // move b and stack out of self,
            // first because this is required to pass them to unity,
            // second because self will not need them afterwards anyway.
            // (next call to choice will return 'None')
            let mut b = Bindings::new();
            let mut stack = vec![];
            swap(&mut self.b, &mut b);
            swap(&mut self.stack, &mut stack);
            match unify(
                &mut stack,
                &|vid| self.can_bind(vid),
                &|vid| self.kb_universal(vid),
                &mut b,
            ) {
                Failed => None,
                Total => {
                    debug_assert!(stack.is_empty());
                    Some(Tree::Children(
                        TripleMatcher {
                            b: b.freeze(),
                            f: self.f,
                            kb: self.kb,
                            i: self.i + 1,
                            j: 0,
                        }
                        .into(),
                    ))
                }
                Partial => Some(Tree::Children(
                    FormulaUnifier {
                        b: b.freeze(),
                        f: self.f,
                        kb: self.kb,
                        i: self.i,
                        j: self.j,
                        stack,
                    }
                    .into(),
                )),
            }
        }
    }

    fn kb_universal(&self, vid: &LocalId) -> bool {
        self.kb.for_all().iter().any(|i| i == vid)
    }

    fn can_bind(&self, vid: &LocalId) -> bool {
        self.kb
            .for_all()
            .iter()
            .chain(self.f.for_some().iter())
            .any(|i| i == vid)
    }
}

#[allow(dead_code)] // TODO remove this eventually
#[derive(Debug)]
/// A [`FormulaUnifier`] tries to unify two formulae (quoted graphs).
///
/// For each possible binding, it delegates to [`TermUnifier`]
/// to continue the unification of the current triples.
struct FormulaUnifier<'a> {
    b: Arc<Bindings<'a>>,
    f: &'a Formula,
    kb: &'a Formula,
    i: usize, // triple index in f
    j: usize, // triple index in kb
    stack: Vec<UnificationFrame<'a>>,
}

impl<'a> FormulaUnifier<'a> {
    fn next(&mut self) -> Option<Tree<FormulaMatcher<'a>>> {
        todo!()
    }
}

// useful conversions

impl<'a> From<&TripleMatcher<'a>> for BuiltinUnifier<'a> {
    fn from(other: &TripleMatcher<'a>) -> BuiltinUnifier<'a> {
        BuiltinUnifier {
            b: Arc::clone(&other.b),
            f: other.f,
            kb: other.kb,
            i: other.i,
            j: other.j,
            state: true,
        }
    }
}

impl<'a> From<&TripleMatcher<'a>> for TermUnifier<'a> {
    fn from(other: &TripleMatcher<'a>) -> TermUnifier<'a> {
        TermUnifier {
            b: other.b.spawn(),
            f: other.f,
            kb: other.kb,
            i: other.i,
            j: other.j,
            stack: vec![(&other.f.triples()[other.i], &other.kb.triples()[other.j], 0)],
        }
    }
}

impl<'a> From<TripleMatcher<'a>> for FormulaMatcher<'a> {
    fn from(other: TripleMatcher<'a>) -> FormulaMatcher<'a> {
        FormulaMatcher::TripleMatcher(other)
    }
}

impl<'a> From<BuiltinUnifier<'a>> for FormulaMatcher<'a> {
    fn from(other: BuiltinUnifier<'a>) -> FormulaMatcher<'a> {
        FormulaMatcher::BuiltinUnifier(other)
    }
}

impl<'a> From<TermUnifier<'a>> for FormulaMatcher<'a> {
    fn from(other: TermUnifier<'a>) -> FormulaMatcher<'a> {
        FormulaMatcher::TermUnifier(other)
    }
}

impl<'a> From<FormulaUnifier<'a>> for FormulaMatcher<'a> {
    fn from(other: FormulaUnifier<'a>) -> FormulaMatcher<'a> {
        FormulaMatcher::FormulaUnifier(other)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case(":a :b :c.", vec![])]
    #[test_case(":a :b :c. { :a :b :d } => { :s :p :o }.", vec![])]
    #[test_case(":a :b :c. { :a :b :c } => { :s :p :o }.", vec![":s :p :o."])]
    #[test_case(":a :b :c. { :a :b ?x } => { :s :p ?x }.", vec![":s :p :c."])]
    #[test_case(":a :b :c, :d. { :a :b ?x } => { :s :p ?x }.", vec![":s :p :c.", ":s :p :d."])]
    #[test_case(":a :b :c; :d :e. { :a :d ?x. ?y :b :c. } => { :s ?x ?y }.", vec![":s :e :a."])]
    #[test_case(":a :b :c; :d :e. { :a _:p ?x; _:q ?y. } => { :s ?x ?y }.", vec![":s :c :c.", ":s :c :e.", ":s :e :c.", ":s :e :e."])]
    #[test_case(":a :b (:c :d). { :a :b ?x } => { :s :p ?x }.", vec![":s :p (:c :d)."])]
    #[test_case(":a :b (:c :d). { :a :b (?x ?y) } => { :s ?x ?y }.", vec![":s :c :d."])]
    #[test_case("?x a :A. { :b a :A } => { :b a :B }.", vec![":b a :B."])]
    #[test_case("?x a :A. ?y a :B. { :b a :A, :B } => { :b a :C }.", vec![":b a :C."])]
    fn fc_reason(doc: &str, expected: Vec<&str>) {
        let kb = parse(doc, Default::default()).unwrap();
        let exp: Vec<_> = expected
            .into_iter()
            .map(|i| parse(i, Default::default()).unwrap())
            .collect();
        let res = super::fc_reason(&kb);
        assert_eq!(exp, res);
    }

    // Those test produce graphs with free variables,
    // so we ignore quantifiers for checking them
    #[test_case(":a :b :c. { ?s ?p ?o } => { ?s ?p ?o }.", vec![":a :b :c.", "{ ?s ?p ?o } => { ?s ?p ?o }."])]
    #[test_case("?x a :A. { ?y a :A } => { ?y a :B }.", vec!["?y a :B."])]
    #[test_case("?x a :A. ?y a :B. { ?z a :A, :B } => { ?z a :C }.", vec!["?z a :C."])]
    fn fc_reason_ignore_quantifiers(doc: &str, expected: Vec<&str>) {
        let kb = parse(doc, Default::default()).unwrap();
        let exp: Vec<_> = expected
            .into_iter()
            .map(|i| parse(i, Default::default()).unwrap().triples().to_vec())
            .collect();
        let res: Vec<_> = super::fc_reason(&kb)
            .into_iter()
            .map(|i| i.triples().to_vec())
            .collect();
        assert_eq!(exp, res);
    }
}
