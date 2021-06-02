use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use uuid::Uuid;

/* Well known symbols */
lazy_static! {
    pub static ref SYMBOL_HAS_INSTANCE: SymbolData =
        SymbolData::new("Symbol.hasInstance".to_string());
    pub static ref SYMBOL_IS_CONCAT_SPREADABLE: SymbolData =
        SymbolData::new("Symbol.isConcatSpreadable".to_string());
    pub static ref SYMBOL_ITERATOR: SymbolData = SymbolData::new("Symbol.iterator".to_string());
    pub static ref SYMBOL_MATCH: SymbolData = SymbolData::new("Symbol.match".to_string());
    pub static ref SYMBOL_REPLACE: SymbolData = SymbolData::new("Symbol.replace".to_string());
    pub static ref SYMBOL_SEARCH: SymbolData = SymbolData::new("Symbol.search".to_string());
    pub static ref SYMBOL_SPECIES: SymbolData = SymbolData::new("Symbol.species".to_string());
    pub static ref SYMBOL_SPLIT: SymbolData = SymbolData::new("Symbol.split".to_string());
    pub static ref SYMBOL_TO_PRIMITIVE: SymbolData =
        SymbolData::new("Symbol.toPrimitive".to_string());
    pub static ref SYMBOL_TO_STRING_TAG: SymbolData =
        SymbolData::new("Symbol.toStringTag".to_string());
}

pub struct SymbolData {
    description: String,
}
impl SymbolData {
    pub fn new(description: String) -> Self {
        SymbolData { description }
    }

    pub fn new_empty() -> Self {
        SymbolData {
            description: Uuid::new_v4().to_hyphenated().to_string(),
        }
    }
}
impl Clone for SymbolData {
    fn clone(&self) -> Self {
        SymbolData {
            description: self.description.to_string(),
        }
    }
}
impl PartialEq for SymbolData {
    fn eq(&self, other: &Self) -> bool {
        self.description == other.description
    }
}
impl Display for SymbolData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({})", self.description)
    }
}
impl Hash for SymbolData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.description.hash(state)
    }
}
