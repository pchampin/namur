use super::*;

/// Retun true iff t is a built-in predicate
pub fn is_builtin(t: &Term) -> bool {
    is_closed_builtin(t) || is_open_builtin(t)
}

/// Retun true iff t is an open predicate
/// (i.e. either an open built-in or a regular predicate)
pub fn is_open(t: &Term) -> bool {
    is_open_builtin(t) || !is_closed_builtin(t)
}

/// Closed built-ins can only be used in rule bodies.
pub fn is_closed_builtin(_t: &Term) -> bool {
    false // todo implement some cloed built-ins
}

/// Open built-ins' theoribox can be manually augmented,
/// by using them as predicate in regular triples (or in rule heads).
pub fn is_open_builtin(_t: &Term) -> bool {
    false // todo implement some open built ins
}
