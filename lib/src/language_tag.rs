use oxilangtag::LanguageTagParseError;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct LanguageTag(Arc<str>);

impl LanguageTag {
    pub fn new<T: Into<Arc<str>>>(tag: T) -> Result<LanguageTag, LanguageTagParseError> {
        let arc = tag.into();
        let arc = oxilangtag::LanguageTag::parse(arc)?.into_inner();
        Ok(LanguageTag(arc))
    }

    pub fn new_unchecked<T: Into<Arc<str>>>(tag: T) -> LanguageTag {
        LanguageTag(tag.into())
    }
}

impl<T> From<oxilangtag::LanguageTag<T>> for LanguageTag
where
    T: std::ops::Deref<Target = str> + Into<Arc<str>>,
{
    fn from(other: oxilangtag::LanguageTag<T>) -> LanguageTag {
        LanguageTag(other.into_inner().into())
    }
}

impl std::ops::Deref for LanguageTag {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for LanguageTag {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for LanguageTag {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl PartialEq for LanguageTag {
    fn eq(&self, other: &LanguageTag) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

impl Eq for LanguageTag {}

impl Hash for LanguageTag {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for c in self.bytes() {
            state.write_u8(c.to_ascii_lowercase())
        }
    }
}
