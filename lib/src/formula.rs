use super::*;
use std::collections::BTreeSet;

/// An N3 Formula is a set of triples, associated to a set of quantified variables.
///
/// # Invariant
///
/// * Variable numeric identifiers are never used twice in a given formula
///   (also considering its subformulae).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Formula {
    for_all: Vec<Variable>,
    for_some: Vec<Variable>,
    triples: Vec<[Term; 3]>,
}

impl Formula {
    pub fn new(
        for_all: Vec<Variable>,
        for_some: Vec<Variable>,
        triples: Vec<[Term; 3]>,
    ) -> Formula {
        // remove duplicates from 'triples'
        Formula {
            for_all,
            for_some,
            triples,
        }
    }

    pub fn empty() -> Formula {
        Formula::new(vec![], vec![], vec![])
    }

    pub fn for_some(&self) -> &[Variable] {
        &self.for_some
    }

    pub fn for_all(&self) -> &[Variable] {
        &self.for_all
    }

    pub fn quantified_variables(&self) -> impl Iterator<Item = &Variable> {
        self.for_some.iter().chain(self.for_all.iter())
    }

    pub fn triples(&self) -> &[[Term; 3]] {
        &self.triples
    }

    pub fn is_closed(&self) -> bool {
        self.free_variables().into_iter().next().is_none()
    }

    pub fn free_variables(&self) -> impl Iterator<Item = &Variable> {
        let scoped: BTreeSet<_> = self.quantified_variables().collect();
        self.iter_terms(false)
            .flat_map(|t| -> Box<dyn Iterator<Item = _>> {
                use Term::*;
                match t {
                    Variable(var) => Box::new(std::iter::once(var)),
                    Formula(f) => Box::new(f.free_variables()),
                    _ => Box::new(std::iter::empty()),
                }
            })
            .filter(move |v| !scoped.contains(v))
    }

    pub fn iter_terms(&self, subformulae: bool) -> TermIterator {
        TermIterator::new(self, subformulae)
    }
}

impl From<Vec<[Term; 3]>> for Formula {
    fn from(other: Vec<[Term; 3]>) -> Formula {
        Formula::new(vec![], vec![], other)
    }
}

pub struct TermIterator<'s> {
    formula: &'s Formula,
    subformulae: bool,
    iterm: usize,
    subterm_iterator: Option<SubtermIterator<'s>>,
}

impl<'s> TermIterator<'s> {
    pub fn new(formula: &'s Formula, subformulae: bool) -> Self {
        TermIterator {
            formula,
            subformulae,
            iterm: 0,
            subterm_iterator: None,
        }
    }
}

impl<'s> Iterator for TermIterator<'s> {
    type Item = &'s Term;

    fn next(&mut self) -> Option<Self::Item> {
        let (itr, ite) = (self.iterm / 3, self.iterm % 3);
        if let Some(iter) = &mut self.subterm_iterator {
            match iter.next() {
                Some(i) => Some(i),
                None => {
                    self.subterm_iterator = None;
                    self.next()
                }
            }
        } else if itr < self.formula.triples.len() {
            let ret = &self.formula.triples[itr][ite];
            self.iterm += 1;
            self.subterm_iterator = Some(ret.iter_subterms(self.subformulae));
            Some(ret)
        } else {
            None
        }
    }
}

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use test_case::test_case;

    #[test_case(":a :b :c." => tl(&[":a", ":b", ":c"]); "one triple")]
    #[test_case(":a :b :c, :d." => tl(&[":a", ":b", ":c", ":a", ":b", ":d"]); "two triples")]
    #[test_case(":a :b (:c :d (:e))." => tl(&[":a", ":b", "(:c :d (:e))", ":c", ":d", "(:e)", ":e"]); "nested lists")]
    #[test_case(":a :b {:c :d {:e :f :g}}." => tl(&[":a", ":b", "{:c :d {:e :f :g}}", ":c", ":d", "{:e :f :g}", ":e", ":f", ":g"]); "nested formulae")]
    #[test_case(":a :b {:c :d (:e :f :g)}." => tl(&[":a", ":b", "{:c :d (:e :f :g)}", ":c", ":d", "(:e :f :g)", ":e", ":f", ":g"]); "list in formula")]
    fn iter_terms(src: &str) -> Vec<Term> {
        let f = parse(src, Default::default()).unwrap();
        f.iter_terms(true).map(Clone::clone).collect()
    }

    pub fn ezterm(txt: &str) -> Term {
        match txt.as_bytes() {
            [b':', ..] => {
                let mut iri = DEFAULT_BASE.to_string();
                iri.push_str(&txt[1..]);
                Iri::new_unchecked(iri).into()
            }
            [b'?', ..] => Variable::new_unchecked(&txt[1..]).into(),
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

    fn tl(ezterms: &[&str]) -> Vec<Term> {
        ezterms.iter().copied().map(ezterm).collect()
    }
}
