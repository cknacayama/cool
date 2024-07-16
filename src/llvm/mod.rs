pub mod compiler;

use std::rc::Rc;

use crate::{
    index_vec::IndexVec,
    ir::{GlobalId, Value},
};

impl Value {
    pub fn to_llvm_string(&self, globals: &IndexVec<GlobalId, Rc<str>>) -> String {
        match self {
            Value::Id(id) => format!("{}", id.to_ir_string(globals)),
            Value::Int(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Void => "{ ptr null, ptr null }".to_string(),
        }
    }
}
