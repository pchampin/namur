use super::*;
use sophia_api::term::SimpleIri;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Term {
    Iri(Iri),
    Literal(Literal),
    Variable(usize),
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

    pub(crate) fn shift_variable(&mut self, offset: usize) {
        if let Term::Variable(vid) = self {
            *vid += offset;
        }
    }
}
