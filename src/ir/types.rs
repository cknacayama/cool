use std::fmt;

use crate::types::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I1,
    I64,
    Ptr,
    String,
    Object,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I1 => write!(f, "i1"),
            Type::I64 => write!(f, "i64"),
            Type::Ptr => write!(f, "ptr"),
            Type::String => write!(f, "%String"),
            Type::Object => write!(f, "%Object"),
        }
    }
}

impl From<TypeId> for Type {
    fn from(id: TypeId) -> Self {
        match id {
            TypeId::INT => Type::I64,
            TypeId::BOOL => Type::I1,
            TypeId::STRING => Type::String,
            _ => Type::Object,
        }
    }
}
