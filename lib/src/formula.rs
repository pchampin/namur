use super::*;
use std::collections::HashSet;

/// An N3 Formula is a set of triples, associated to a set of quantified variables.
///
/// # Invariant
///
/// * Variable numeric identifiers are never used twice in a given formula
///   (also considering its subformulae).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Formula {
    for_some: Vec<(usize, VarLabel)>,
    for_all: Vec<(usize, VarLabel)>,
    triples: Vec<[Term; 3]>,
    max_var: usize,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum VarLabel {
    None,
    LocalId(LocalId),
    Iri(Iri),
}

impl Formula {
    pub fn new(
        for_some: Vec<(usize, VarLabel)>,
        for_all: Vec<(usize, VarLabel)>,
        triples: Vec<[Term; 3]>,
    ) -> Formula {
        // remove duplicates from 'triples'
        let mut f = Formula {
            for_some,
            for_all,
            triples,
            max_var: 0,
        };
        let mut varset = HashSet::new();
        assert!(f.check_variables(&mut varset));
        let max_var = varset.iter().copied().max().unwrap_or(0);
        f.max_var = max_var;
        f
    }

    pub fn empty() -> Formula {
        Formula::new(vec![], vec![], vec![])
    }

    pub fn for_some(&self) -> &[(usize, VarLabel)] {
        &self.for_some
    }

    pub fn for_all(&self) -> &[(usize, VarLabel)] {
        &self.for_all
    }

    pub fn quantified_variables(&self) -> impl Iterator<Item = &(usize, VarLabel)> {
        self.for_some.iter().chain(self.for_all.iter())
    }

    pub fn triples(&self) -> &[[Term; 3]] {
        &self.triples
    }

    pub fn max_var(&self) -> usize {
        self.max_var
    }

    /// Checks that no two variables have the same numeric identifier.
    fn check_variables(&self, varset: &mut HashSet<usize>) -> bool {
        for (vid, _) in self.quantified_variables() {
            if !varset.insert(*vid) {
                return false;
            }
        }
        for triple in &self.triples {
            for term in triple {
                if let Term::Formula(f) = term {
                    if !f.check_variables(varset) {
                        return false;
                    }
                }
            }
        }
        true
    }

    pub fn shift_variables(&mut self, offset: usize) {
        for (vid, _) in &mut self.for_some {
            *vid += offset;
        }
        for (vid, _) in &mut self.for_all {
            *vid += offset;
        }
        for triple in &mut self.triples {
            for term in triple {
                term.shift_variable(offset)
            }
        }
    }
}

impl From<Vec<[Term; 3]>> for Formula {
    fn from(other: Vec<[Term; 3]>) -> Formula {
        Formula::new(vec![], vec![], other)
    }
}

// BinderName impl

impl From<LocalId> for VarLabel {
    fn from(other: LocalId) -> VarLabel {
        VarLabel::LocalId(other)
    }
}

impl From<Iri> for VarLabel {
    fn from(other: Iri) -> VarLabel {
        VarLabel::Iri(other)
    }
}
