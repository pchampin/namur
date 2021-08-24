use sophia_api::term::{RawValue, SimpleIri, TTerm, TermKind};
use sophia_iri::{error::InvalidIri, Iri as SophiaIri, IsIri, IsIriRef};
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Iri(Arc<str>);

impl Iri {
    pub fn new<T: Into<Arc<str>>>(iri: T) -> Result<Iri, InvalidIri> {
        let arc = iri.into();
        SophiaIri::new(&arc)?;
        Ok(Iri(arc))
    }

    pub fn new_unchecked<T: Into<Arc<str>>>(iri: T) -> Iri {
        Iri(iri.into())
    }

    pub fn from_simple_iri(si: SimpleIri<'_>) -> Iri {
        let (ns, sf) = si.destruct();
        let mut txt = ns.to_string();
        if let Some(sf) = sf {
            txt.push_str(sf);
        }
        Self::new_unchecked(txt)
    }
}

impl std::ops::Deref for Iri {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for Iri {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for Iri {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl IsIriRef for Iri {}

impl IsIri for Iri {}

impl PartialEq<SimpleIri<'_>> for Iri {
    fn eq(&self, other: &SimpleIri<'_>) -> bool {
        match other.destruct() {
            (ns, None) => &self[..] == ns,
            (ns, Some(sf)) => self.starts_with(ns) && &self[ns.len()..] == sf,
        }
    }
}

impl TTerm for Iri {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn value_raw(&self) -> RawValue {
        RawValue::from(&self[..])
    }

    fn as_dyn(&self) -> &dyn TTerm {
        &*self
    }
}
