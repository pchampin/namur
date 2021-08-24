use super::*;
pub use sophia_api::ns::{rdf, xsd};
pub use sophia_api::term::SimpleIri;

pub mod log {
    use sophia_api::namespace;
    namespace!(
        "http://www.w3.org/2000/10/swap/log#",
        // classes
        Chaff,
        Formula,
        List,
        N3Document,
        Other,
        String,
        Truth,
        Type,
        // properties
        conclusion,
        conjunction,
        content,
        definitiveDocument,
        definitiveService,
        dtlit,
        equalTo,
        findall,
        forAllIn,
        forAllInClosure,
        implies,
        includes,
        n3String,
        notEqualTo,
        notIn,
        notInClosure,
        notIncludes,
        outputString,
        parsedAsN3,
        racine,
        rawType,
        rawUri,
        semantics,
        semanticsOrError,
        skolem,
        uri,
        // datatypes
        N3
    );
}

pub const VARIABLE_NS: &str = "tag:w3.org,2021:n3-variable#";

pub fn variable_iri(id: &LocalId) -> SimpleIri {
    SimpleIri::new_unchecked(VARIABLE_NS, Some(&id[..]))
}
