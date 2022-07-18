#![allow(unused_imports, unused_variables, unused_mut, dead_code)] // TODO remove this enventually

use super::*;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Bindings<'a> {
    parent: Option<Arc<Bindings<'a>>>,
    map: HashMap<&'a Variable, &'a Term>,
}

impl<'a> Bindings<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn freeze(self) -> Arc<Self> {
        Arc::new(self)
    }

    pub fn spawn(self: &Arc<Self>) -> Self {
        Bindings {
            parent: Some(Arc::clone(self)),
            map: HashMap::new(),
        }
    }

    pub fn get(&self, variable: &Variable) -> Option<&Term> {
        self.map
            .get(variable)
            .copied()
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(variable)))
    }

    pub fn set(&mut self, variable: &'a Variable, term: &'a Term) {
        self.map.insert(variable, term);
    }

    pub fn bind_formula(&self, f: &Formula) -> Formula {
        let compiled = self.compile(f.for_all(), f.for_some());
        let triples = f.triples().iter().map(|t| self.bind_triple(t)).collect();
        Formula::new(f.for_all().to_vec(), f.for_some().to_vec(), triples)
    }

    pub fn bind_triple(&self, t: &[Term; 3]) -> [Term; 3] {
        [
            self.bind_term(&t[0]),
            self.bind_term(&t[1]),
            self.bind_term(&t[2]),
        ]
    }

    pub fn bind_term(&self, t: &Term) -> Term {
        use Term::*;
        match t {
            Variable(vid) => match self.get(vid) {
                Some(t) => t.clone(),
                None => Variable(vid.clone()),
            },
            List(v) => List(v.iter().map(|t| self.bind_term(t)).collect()),
            Formula(f) => Formula(self.bind_formula(f)),
            _ => t.clone(),
        }
    }

    fn compile(&self, exclude1: &[Variable], exclude2: &[Variable]) -> Bindings {
        let mut compiled = match self.parent.as_ref() {
            None => Bindings::new(),
            Some(p) => p.compile(exclude1, exclude2),
        };
        for (var, term) in self.map.iter() {
            if exclude1.iter().chain(exclude2.iter()).any(|v| v == *var) {
                continue;
            }
            compiled.map.insert(*var, *term);
        }
        compiled
    }
}

/// Tries to unify the two slice of terms (triple or lists) at the top of the stack.
/// On success, return `Total` after popping the top of the stack.
/// On failure, return `Fail` (the stack can be dropped).
/// On partial success (i.e. when encountering quoted graphs that require a backtracker),
/// return `Partial`, and the stack should be kept for recovering.
pub fn unify<'a, F1, F2>(
    stack: &mut Vec<UnificationFrame<'a>>,
    bindable: &F1,
    kb_universal: &F2,
    bindings: &mut Bindings<'a>,
) -> UnificationResult
where
    F1: Fn(&Variable) -> bool,
    F2: Fn(&Variable) -> bool,
{
    use Term::*;
    use UnificationResult::*;

    let (l1, l2, start) = stack.pop().unwrap();
    debug_assert_eq!(l1.len(), l2.len());
    for i in start..l1.len() {
        let (t1, t2) = (&l1[i], &l2[i]);
        match (t1, t2) {
            (Iri(iri1), Iri(iri2)) if iri1 == iri2 => (),
            (Literal(lit1), Literal(lit2)) if lit1 == lit2 => (),
            (_, Variable(vid)) if kb_universal(vid) => (),
            (Variable(vid), _) => {
                if let Some(tb) = bindings.get(vid) {
                    if tb != t2 {
                        return Failed;
                    }
                } else if bindable(vid) {
                    bindings.set(vid, t2);
                } else if t1 != t2 {
                    return Failed;
                }
            }
            (List(lst1), List(lst2)) if lst1.len() == lst2.len() => {
                stack.push((l1, l2, i));
                stack.push((lst1, lst2, 0));
                let r = unify(stack, bindable, kb_universal, bindings);
                if r != Total {
                    return r;
                } else {
                    // restore context where it was, in order to continue.
                    // (lst1, lst2, 0) were popped by the recursive call,
                    // but we still need to pop (again) (l1, l2, i)
                    stack.pop();
                }
            }
            (Formula(f1), Formula(f2)) => {
                stack.push((l1, l2, i));
                return Partial;
            }
            _ => return Failed,
        }
    }
    Total
}

pub type UnificationFrame<'a> = (&'a [Term], &'a [Term], usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnificationResult {
    Failed,
    Partial,
    Total,
}

#[cfg(test)]
#[allow(clippy::bool_assert_comparison)]
mod test {
    use super::*;
    use formula::test::ezterm;
    use test_case::test_case;
    use UnificationResult::*;

    #[test_case(":a", ":a" => (Total, 0))]
    #[test_case(":a", ":b" => (Failed, 0))]
    #[test_case(":a", "42" => (Failed, 0))]
    #[test_case("42", ":a" => (Failed, 0))]
    #[test_case("?0", ":a" => (Total, 1))]
    #[test_case("?0", "42" => (Total, 1))]
    #[test_case(":a", "?10" => (Total, 0))]
    #[test_case("?0", "?10" => (Total, 0))]
    #[test_case("?0", "{ :s :p :o }" => (Total, 1))]
    #[test_case("?0", "(:a :b)" => (Total, 1))]
    #[test_case("(:a :b)", "(:a :b)" => (Total, 0))]
    #[test_case("(:a :b)", "(:a :b :c)" => (Failed, 0))]
    #[test_case("(?0 :b)", "(:a :b)" => (Total, 1))]
    #[test_case("(?0 ?1)", "(:a :b)" => (Total, 2))]
    #[test_case("(?0 ?1)", "(:a :a)" => (Total, 2))]
    #[test_case("(?0 ?0)", "(:a :b)" => (Failed, 1))]
    #[test_case("(?0 ?0)", "(:a :a)" => (Total, 1))]
    #[test_case("(?0 :b)", "?10" => (Total, 0))]
    #[test_case("(:a (:l) :c)", "(:a (:l) :c)" => (Total, 0))]
    #[test_case("(:a (:l) :c)", "(:a (:l) :d)" => (Failed, 0))]
    #[test_case("(?0 (:l) :c)", "(:a (:l) :d)" => (Failed, 1))]
    #[test_case("(?0 ?1 :c)", "(:a (:l) :c)" => (Total, 2))]
    #[test_case("(?1 (?0) :c)", "(:a (:l) :c)" => (Total, 2))]
    #[test_case("(?1 (?0) :c)", "(:a ((:m)) :c)" => (Total, 2))]
    #[test_case("(?0 ((?1)) :c)", "(:a ((:m)) :c)" => (Total, 2))]
    #[test_case("{:s :p :o}", "{:s :p :o}" => (Partial, 0))]
    #[test_case("(?0 {})", "(:a {})" => (Partial, 1))]
    fn unify_simple(t1: &str, t2: &str) -> (UnificationResult, usize) {
        let l1 = vec![ezterm(t1)];
        let l2 = vec![ezterm(t2)];
        let mut stack = vec![(&l1[..], &l2[..], 0)];
        let mut b = Bindings::new();
        let r = unify(&mut stack, &|_| true, &|_| true, &mut b);
        (r, b.map.len())
    }

    #[test_case("?0", ":a", vec![":a"])]
    #[test_case("?0", "42", vec!["42"])]
    #[test_case("?0", "{ :s :p :o }", vec!["{ :s :p :o }"])]
    #[test_case("?0", "(:a :b)", vec!["(:a :b)"])]
    #[test_case("(?1 ?0)", "(:a :b)", vec![":b", ":a"])]
    #[test_case("(?1 ?0)", "(:a :a)", vec![":a", ":a"])]
    #[test_case("(?0 ?0)", "(:a :a)", vec![":a"])]
    #[test_case("(?0 ?1)", "(:a (:l))", vec![":a", "(:l)"])]
    #[test_case("(?1 (?0))", "(:a (:l))", vec![":l", ":a"])]
    #[test_case("(?0 ?1)", "(:a ((:m)))", vec![":a", "((:m))"])]
    #[test_case("(?1 ((?0)))", "(:a ((:m)))", vec![":m", ":a"])]
    #[test_case("(?0 ?1 {})", "(:b :a {})", vec![":b", ":a"])]
    #[test_case("(?0 {})", "(:b {})", vec![":b"])]
    #[test_case("(?0 :a ({}))", "(:b :a ({}))", vec![":b"])]
    fn unify_bindings(t1: &str, t2: &str, map: Vec<&str>) {
        let l1 = vec![ezterm(t1)];
        let l2 = vec![ezterm(t2)];
        let mut stack = vec![(&l1[..], &l2[..], 0)];
        let mut b = Bindings::new();
        let r = unify(&mut stack, &|_| true, &|_| true, &mut b);
        assert!(r != Failed);
        assert_eq!(map.len(), b.map.len());
        for (i, val) in map.iter().enumerate() {
            let key = LocalId::new_unchecked(format!("{}", i)).into();
            assert_eq!(b.get(&key), Some(ezterm(val)).as_ref());
        }
    }
}
