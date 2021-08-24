use super::ns::{rdf, xsd};
use super::*;
use mownstr::MownStr;
use sophia_api::term::SimpleIri;
use std::hash::{Hash, Hasher};

type LitF64 = f64;

#[derive(Clone, Debug)]
pub enum Literal {
    CharString(Arc<str>, Option<LanguageTag>),
    Integer(isize),
    Double(LitF64),
    Boolean(bool),
    TypedLiteral(Arc<str>, Iri),
}
pub use Literal::*;

impl Literal {
    fn lexical_value(&self) -> MownStr<'_> {
        match self {
            Integer(i) => i.to_string().into(),
            Double(d) => d.to_string().into(),
            Boolean(b) => b.to_string().into(),
            CharString(val, _) | TypedLiteral(val, _) => MownStr::from(&val[..]),
        }
    }

    fn datatype(&self) -> SimpleIri<'_> {
        match self {
            CharString(_, None) => xsd::string,
            CharString(_, Some(_)) => rdf::langString,
            Integer(..) => xsd::integer,
            Double(..) => xsd::double,
            Boolean(..) => xsd::boolean,
            TypedLiteral(_, dt) => SimpleIri::new_unchecked(&dt[..], None),
        }
    }

    fn language_tag(&self) -> Option<&LanguageTag> {
        if let CharString(_, tag) = self {
            tag.as_ref()
        } else {
            None
        }
    }
}

impl From<Arc<str>> for Literal {
    fn from(other: Arc<str>) -> Self {
        CharString(other, None)
    }
}

impl From<&str> for Literal {
    fn from(other: &str) -> Self {
        CharString(other.into(), None)
    }
}

impl From<isize> for Literal {
    fn from(other: isize) -> Self {
        Integer(other)
    }
}

impl From<f64> for Literal {
    fn from(other: f64) -> Self {
        Double(other)
    }
}

impl From<bool> for Literal {
    fn from(other: bool) -> Self {
        Boolean(other)
    }
}

impl From<(Arc<str>, LanguageTag)> for Literal {
    fn from(other: (Arc<str>, LanguageTag)) -> Self {
        CharString(other.0, Some(other.1))
    }
}

impl From<(&str, LanguageTag)> for Literal {
    fn from(other: (&str, LanguageTag)) -> Self {
        CharString(other.0.into(), Some(other.1))
    }
}

impl From<(Arc<str>, Iri)> for Literal {
    fn from(other: (Arc<str>, Iri)) -> Self {
        TypedLiteral(other.0, other.1)
    }
}

impl From<(&str, Iri)> for Literal {
    fn from(other: (&str, Iri)) -> Self {
        TypedLiteral(other.0.into(), other.1)
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Literal) -> bool {
        match (self, other) {
            (CharString(v1, t1), CharString(v2, t2)) => v1 == v2 && t1 == t2,
            (Integer(i1), Integer(i2)) => i1 == i2,
            (Double(d1), Double(d2)) => d1 == d2,
            (Boolean(b1), Boolean(b2)) => b1 == b2,
            (TypedLiteral(v1, d1), TypedLiteral(v2, d2)) => v1 == v2 && d1 == d2,
            (lit1, TypedLiteral(v2, d2)) => {
                v2[..] == lit1.lexical_value()[..] && *d2 == lit1.datatype()
            }
            (TypedLiteral(v1, d1), lit2) => {
                v1[..] == lit2.lexical_value()[..] && *d1 == lit2.datatype()
            }
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.lexical_value().as_bytes());
        if let Some(tag) = self.language_tag() {
            state.write(tag.as_bytes());
        } else {
            let (pr, sf) = self.datatype().destruct();
            state.write(pr.as_bytes());
            if let Some(sf) = sf {
                state.write(sf.as_bytes());
            }
        }
    }
}
