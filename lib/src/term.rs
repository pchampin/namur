use super::*;
use sophia_api::term::SimpleIri;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Term {
    Iri(Iri),
    Literal(Literal),
    Variable(Variable),
    List(Vec<Term>),
    Formula(Formula),
}

impl From<Iri> for Term {
    fn from(other: Iri) -> Term {
        Term::Iri(other)
    }
}

impl From<SimpleIri<'_>> for Term {
    fn from(other: SimpleIri<'_>) -> Term {
        Iri::from_simple_iri(other).into()
    }
}

impl From<Literal> for Term {
    fn from(other: Literal) -> Term {
        Term::Literal(other)
    }
}

impl From<Variable> for Term {
    fn from(other: Variable) -> Term {
        Term::Variable(other)
    }
}

impl From<Vec<Term>> for Term {
    fn from(other: Vec<Term>) -> Term {
        Term::List(other)
    }
}

impl From<Formula> for Term {
    fn from(other: Formula) -> Term {
        Term::Formula(other)
    }
}

impl Term {
    pub fn is_atom(&self) -> bool {
        matches!(self, Term::Iri(_) | Term::Literal(_))
    }

    pub fn is_ground(&self) -> bool {
        use Term::*;
        match self {
            Iri(_) | Literal(_) => true,
            Variable(_) => false,
            List(items) => items.iter().all(Term::is_ground),
            Formula(f) => f.is_closed(),
        }
    }

    pub fn iter_subterms(&self, subformulae: bool) -> SubtermIterator {
        SubtermIterator::new(self, subformulae)
    }
}

pub enum SubtermIterator<'s> {
    Empty,
    ListItems {
        items: &'s [Term],
        subterms: Option<Box<SubtermIterator<'s>>>,
        subformulae: bool,
    },
    QuotedFormula(Box<TermIterator<'s>>),
}

impl<'s> SubtermIterator<'s> {
    pub fn new(term: &'s Term, subformulae: bool) -> Self {
        use Term::*;
        match term {
            List(items) => Self::ListItems {
                items,
                subterms: None,
                subformulae,
            },
            Formula(f) if subformulae => Self::QuotedFormula(Box::new(f.iter_terms(true))),
            _ => Self::Empty,
        }
    }
}

impl<'s> Iterator for SubtermIterator<'s> {
    type Item = &'s Term;

    fn next(&mut self) -> Option<Self::Item> {
        use SubtermIterator::*;
        match self {
            Empty => None,
            QuotedFormula(i) => i.next(),
            ListItems {
                items,
                subterms,
                subformulae,
            } => {
                if let Some(iter) = subterms {
                    match iter.next() {
                        Some(i) => Some(i),
                        None => {
                            *subterms = None;
                            self.next()
                        }
                    }
                } else if !items.is_empty() {
                    let ret = &items[0];
                    *items = &items[1..];
                    *subterms = Some(Box::new(ret.iter_subterms(*subformulae)));
                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}
