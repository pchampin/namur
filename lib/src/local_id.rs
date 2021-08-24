//! Local IDs are used for blank nodes and variables.
//!
//! # Note
//! This implementation accepts the same regex for blank nodes and variables,
//! while N3 was originally using SPARQL's grammar for variables,
//! which is more restrictive than for blank nodes.

//! Blank node like specified in [RDF](https://www.w3.org/TR/rdf11-primer/#section-blank-node).
//!

use super::*;
use lazy_static::lazy_static;
use regex::Regex;
use std::fmt;

lazy_static! {
    /// A modified production of Turtle's BLANK_NODE_LABEL according to the
    /// [Turtle spec](https://www.w3.org/TR/turtle/#grammar-production-BlankNode).
    ///
    /// In contrast to the original rule this regular expression does not look
    /// for a leading `_:`. Accordingly it only checks if the label is valid.
    ///
    /// Actually, this regex is also valid for Notation3 nodes. Even Turtle is
    /// a derivate of N3, it does change the syntax of blank nodes.
    ///
    /// # Captures
    ///
    /// This regular expression matches the whole input (`^...$`),
    /// therefore, it can not be used to capture local IDs in an arbitrary string.
    ///
    /// # Rule
    ///
    /// `LOCAL_ID ::= (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?`
    static ref LOCAL_ID: Regex = Regex::new(r"(?x)
      ^
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9]
      (
          [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
          |
          \u{2e} [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
      )*
      $
    ").unwrap();
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LocalId(Arc<str>);

impl LocalId {
    pub fn new<T: Into<Arc<str>>>(id: T) -> Result<LocalId, InvalidLocalId> {
        let arc = id.into();
        if !LOCAL_ID.is_match(&arc[..]) {
            Err(InvalidLocalId(arc.to_string()))
        } else {
            Ok(LocalId(arc))
        }
    }

    pub fn new_unchecked<T: Into<Arc<str>>>(id: T) -> LocalId {
        LocalId(id.into())
    }
}

impl std::ops::Deref for LocalId {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for LocalId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for LocalId {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct InvalidLocalId(String);

impl fmt::Display for InvalidLocalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid Local ID {:?}", &self.0)
    }
}

impl std::error::Error for InvalidLocalId {}
