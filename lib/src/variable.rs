use super::*;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Variable {
    LocalId(LocalId),
    Iri(Iri),
}

impl Variable {
    pub fn as_str(&self) -> &str {
        match self {
            Variable::LocalId(lid) => lid.as_str(),
            Variable::Iri(iri) => iri,
        }
    }
}

impl From<LocalId> for Variable {
    fn from(other: LocalId) -> Variable {
        Variable::LocalId(other)
    }
}

impl From<Iri> for Variable {
    fn from(other: Iri) -> Variable {
        Variable::Iri(other)
    }
}
